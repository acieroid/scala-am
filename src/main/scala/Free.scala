/**
 * Implementation of "Pushdown Control-Flow Analysis for Free", which is
 * basically a variant of AAC with better complexity (Gilray, Thomas, et
 * al. "Pushdown Control-Flow Analysis for Free." arXiv preprint
 * arXiv:1507.03137 (2015)).
 */
class Free[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "Free"

  val primitives = new Primitives[Addr, Abs]()
  val initialEnv = Environment.empty[Addr].extend(primitives.forEnv)
  val initialStore = Store.initial[Addr, Abs](primitives.forStore)

  trait KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }
  /**
   * A continuation address is either for a normal continuation, and contains an
   * expression and the corresponding binding environment from the moment where
   * the continuation has been allocated
   */
  case class NormalKontAddress(exp: Exp, ρ: Environment[Addr]) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  /** Or it is the address of the halt continuation */
  object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  /**
   * A state contains the control component, a store, a continuation store, and
   * the address of the current continuation. The stores aren't actually local
   * to the state, but they are injected inside it during the state space
   * exploration phase.
   */
  case class State(control: Control, σ: Store[Addr, Abs], kstore: KontStore[KontAddr], k: KontAddr, t: Time) {
    def this(exp: Exp) = this(ControlEval(exp, initialEnv), initialStore,
                              new KontStore[KontAddr](), HaltKontAddress, time.initial(""))
    override def toString() = control.toString(σ)
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && kstore.subsumes(that.kstore) && k.equals(that.k)

    /** Integrate a set of action to compute the successor states */
    private def integrate(k: KontAddr, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
      actions.map({
        case ActionReachedValue(v, σ, _) => State(ControlKont(v), σ, kstore, k, time.tick(t))
        case ActionPush(e, frame, ρ, σ, _) => {
          val next = new NormalKontAddress(e, ρ)
          State(ControlEval(e, ρ), σ, kstore.extend(next, Kont(frame, k)), next, time.tick(t))
        }
        case ActionEval(e, ρ, σ, _) => State(ControlEval(e, ρ), σ, kstore, k, time.tick(t))
        case ActionStepIn(fexp, _, e, ρ, σ, _, _) => State(ControlEval(e, ρ), σ, kstore, k, time.tick(t, fexp))
        case ActionError(err) => State(ControlError(err), σ, kstore, k, time.tick(t))
      })

    /** Computes the successors states of this one relying on the given semantics */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[State] = control match {
      case ControlEval(e, ρ) => integrate(k, sem.stepEval(e, ρ, σ, t))
      case ControlKont(v) if abs.isError(v) => Set()
      case ControlKont(v) => kstore.lookup(k).foldLeft(Set[State]())((acc, k) => k match {
        case Kont(frame, next) => acc ++ integrate(next, sem.stepKont(v, frame, σ, t))
      })
      case ControlError(_) => Set()
    }

    /** Checks whether this state has finished evaluation */
    def halted = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) =>
        k.equals(HaltKontAddress) || abs.isError(v)
      case ControlError(_) => true
    }
  }

  /**
   * A configuration is basically a state without the store and continuation
   * store (because these stores are global).
   */
  case class Configuration(control: Control, k: KontAddr, t: Time) {
    override def toString = s"($control, $k)"
  }
  /**
   * Represents multiple states as a set of configuration that share the same
   * store and continuation store
   */
  case class States(R: Set[Configuration], σ: Store[Addr, Abs], kstore: KontStore[KontAddr]) {
    def this(exp: Exp) = this(Set(Configuration(ControlEval(exp, initialEnv),
                                                HaltKontAddress, time.initial(""))),
                              initialStore, new KontStore[KontAddr]())
    override def toString = R.toString
    /** Performs a step on all the contained states */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): States = {
      val states = R.map(conf => State(conf.control, σ, kstore, conf.k, conf.t))
      val succs = states.flatMap(ς => ς.step(sem))
      val (σ1, kstore1) = succs.foldLeft((Store.empty[Addr, Abs], new KontStore[KontAddr]()))((acc, ς) => (acc._1.join(ς.σ), acc._2.join(ς.kstore)))
      States(succs.map(ς => Configuration(ς.control, ς.k, ς.t)), σ1, kstore1)
    }
    def isEmpty = R.isEmpty
    def toStateSet: Set[State] = R.map({ case Configuration(control, k, t) => State(control, σ, kstore, k, t) })
    def size: Int = R.size
  }

  /** The output of the machine */
  case class FreeOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[Graph[State, Unit]], timedOut: Boolean)
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString)),
        (s) => if (halted.contains(s)) { Colors.Yellow } else { s.control match {
          case ControlEval(_, _) => Colors.Green
          case ControlKont(_) => Colors.Pink
          case ControlError(_) => Colors.Red
        }}, _ => List())
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  /**
   * Performs state space exploration and builds the state graph at the same
   * time. We lose the "for free" part of this approach by constructing the
   * graph, since we have to take every possible combination of configurations
   * and draw edges between them.
   */
  @scala.annotation.tailrec
  private def loopWithLocalGraph(s: States, visited: Set[States],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Graph[State, Unit],
    sem: Semantics[Exp, Abs, Addr, Time]): Output[Abs] = {
    val s2 = s.step(sem)
    val h = halted ++ s.toStateSet.filter(_.halted)
    if (s2.isEmpty || visited.contains(s2) || timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      FreeOutput(h, visited.foldLeft(0)((acc, s) => acc + s.size),
        (System.nanoTime - startingTime) / Math.pow(10, 9), Some(graph),
        timeout.map(System.nanoTime - startingTime > _).getOrElse(false))
    } else {
      loopWithLocalGraph(s2, visited + s, h, startingTime, timeout,
        graph.addEdges(s.toStateSet.flatMap(ς1 =>
          s2.toStateSet.map(ς2 => (ς1, (), ς2)))), sem)
    }
  }

  /**
   * Performs state space exploration without building the graph
   */
  private def loop(s: States, visited: Set[States],
    halted: Set[State], startingTime: Long, timeout: Option[Long],
    sem: Semantics[Exp, Abs, Addr, Time]): Output[Abs] = {
    val s2 = s.step(sem)
    val h = halted ++ s.toStateSet.filter(_.halted)
    if (s2.isEmpty || visited.contains(s2) || timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      FreeOutput(h, visited.foldLeft(0)((acc, s) => acc + s.size),
        (System.nanoTime - startingTime) / Math.pow(10, 9),
        None, timeout.map(System.nanoTime - startingTime > _).getOrElse(false))
    } else {
      loop(s2, visited + s, h, startingTime, timeout, sem)
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] =
    if (graph) {
      loopWithLocalGraph(new States(exp), Set(), Set(), System.nanoTime, timeout, new Graph[State, Unit](), sem)
    } else {
      loop(new States(exp), Set(), Set(), System.nanoTime, timeout, sem)
    }
}
