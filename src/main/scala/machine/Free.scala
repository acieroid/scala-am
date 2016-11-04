/**
 * Implementation of "Pushdown Control-Flow Analysis for Free", which is
 * basically a variant of AAC with better complexity (Gilray, Thomas, et
 * al. "Pushdown Control-Flow Analysis for Free." arXiv preprint
 * arXiv:1507.03137 (2015)).
 */
class Free[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "Free"

  trait KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }
  /**
   * A continuation address is either for a normal continuation, and contains an
   * expression and the corresponding binding environment from the moment where
   * the continuation has been allocated
   */
  case class NormalKontAddress(exp: Exp, env: Environment[Addr]) extends KontAddr {
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
  case class State(control: Control, store: Store[Addr, Abs], kstore: KontStore[KontAddr], k: KontAddr, t: Time) {
    override def toString = control.toString
    def subsumes(that: State): Boolean = control.subsumes(that.control) && store.subsumes(that.store) && kstore.subsumes(that.kstore) && k.equals(that.k)

    /** Integrate a set of action to compute the successor states */
    private def integrate(k: KontAddr, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
      actions.map({
        case ActionReachedValue(v, store, _) => State(ControlKont(v), store, kstore, k, Timestamp[Time].tick(t))
        case ActionPush(frame, e, env, store, _) => {
          val next = new NormalKontAddress(e, env)
          State(ControlEval(e, env), store, kstore.extend(next, Kont(frame, k)), next, Timestamp[Time].tick(t))
        }
        case ActionEval(e, env, store, _) => State(ControlEval(e, env), store, kstore, k, Timestamp[Time].tick(t))
        case ActionStepIn(fexp, _, e, env, store, _, _) => State(ControlEval(e, env), store, kstore, k, Timestamp[Time].tick(t, fexp))
        case ActionError(err) => State(ControlError(err), store, kstore, k, Timestamp[Time].tick(t))
      })

    /** Computes the successors states of this one relying on the given semantics */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[State] = control match {
      case ControlEval(e, env) => integrate(k, sem.stepEval(e, env, store, t))
      case ControlKont(v) => kstore.lookup(k).foldLeft(Set[State]())((acc, k) => k match {
        case Kont(frame, next) => acc ++ integrate(next, sem.stepKont(v, frame, store, t))
      })
      case ControlError(_) => Set()
    }

    /** Checks whether this state has finished evaluation */
    def halted = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => k.equals(HaltKontAddress)
      case ControlError(_) => true
    }
  }
  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) =
      State(ControlEval(exp, Environment.empty[Addr].extend(env)),
        Store.initial[Addr, Abs](store),
        KontStore.empty[KontAddr], HaltKontAddress, Timestamp[Time].initial(""))
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
  case class States(R: Set[Configuration], store: Store[Addr, Abs], kstore: KontStore[KontAddr]) {
    override def toString = R.toString
    /** Performs a step on all the contained states */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): States = {
      val states = R.map(conf => State(conf.control, store, kstore, conf.k, conf.t))
      val succs = states.flatMap(state => state.step(sem))
      val (store1, kstore1) = succs.foldLeft((Store.empty[Addr, Abs], KontStore.empty[KontAddr]))((acc, state) => (acc._1.join(state.store), acc._2.join(state.kstore)))
      States(succs.map(state => Configuration(state.control, state.k, state.t)), store1, kstore1)
    }
    def isEmpty = R.isEmpty
    def toStateSet: Set[State] = R.map({ case Configuration(control, k, t) => State(control, store, kstore, k, t) })
    def size: Int = R.size
  }

  /** The output of the machine */
  case class FreeOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[Graph[State, Unit]], timedOut: Boolean)
      extends Output {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => JoinLattice[Abs].subsumes(v2, v))
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
    override def joinedStore: Store[Addr, Abs] =
      halted.map(s => s.store).foldLeft(Store.empty[Addr, Abs])((acc, store) => acc.join(store))
  }
  object States {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) =
      States(Set(Configuration(ControlEval(exp, Environment.empty[Addr].extend(env)), HaltKontAddress, Timestamp[Time].initial(""))),
        Store.initial[Addr, Abs](store), KontStore.empty[KontAddr])
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
    sem: Semantics[Exp, Abs, Addr, Time]): Output = {
    val s2 = s.step(sem)
    val h = halted ++ s.toStateSet.filter(_.halted)
    if (s2.isEmpty || visited.contains(s2) || timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      FreeOutput(h, visited.foldLeft(0)((acc, s) => acc + s.size),
        (System.nanoTime - startingTime) / Math.pow(10, 9), Some(graph),
        timeout.map(System.nanoTime - startingTime > _).getOrElse(false))
    } else {
      loopWithLocalGraph(s2, visited + s, h, startingTime, timeout,
        graph.addEdges(s.toStateSet.flatMap(state1 =>
          s2.toStateSet.map(state2 => (state1, (), state2)))), sem)
    }
  }

  /**
   * Performs state space exploration without building the graph
   */
  private def loop(s: States, visited: Set[States],
    halted: Set[State], startingTime: Long, timeout: Option[Long],
    sem: Semantics[Exp, Abs, Addr, Time]): Output = {
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

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output =
    if (graph) {
      loopWithLocalGraph(States.inject(exp, sem.initialEnv, sem.initialStore), Set(), Set(), System.nanoTime, timeout, new Graph[State, Unit](), sem)
    } else {
      loop(States.inject(exp, sem.initialEnv, sem.initialStore), Set(), Set(), System.nanoTime, timeout, sem)
    }
}
