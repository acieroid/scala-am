import AbstractValue._

/**
 * Implementation of "Pushdown Control-Flow Analysis for Free", which is
 * basically a variant of AAC with better complexity
 * TODO: global store & kstore
 */
case class Free[Exp : Expression, Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr])
    extends AbstractMachine[Exp, Abs, Addr] {
  def abs = implicitly[AbstractValue[Abs]]
  def absi = implicitly[AbstractInjection[Abs]]
  def addr = implicitly[Address[Addr]]
  def addri = implicitly[AddressInjection[Addr]]
  def exp = implicitly[Expression[Exp]]

  def name = "Free"

  trait Control {
    def subsumes(that: Control): Boolean
    def toString(store: Store[Addr, Abs]): String = toString()
  }

  case class ControlEval(exp: Exp, env: Environment[Addr]) extends Control {
    override def toString() = s"ev($exp)"
    def subsumes(that: Control) = that match {
      case ControlEval(exp2, env2) => exp.equals(exp2) && env.subsumes(env2)
      case _ => false
    }
  }

  case class ControlKont(v: Abs) extends Control {
    override def toString = s"ko($v)"
    override def toString(store: Store[Addr, Abs]) = s"ko(${abs.toString(v, store)})"
    def subsumes(that: Control) = that match {
      case ControlKont(v2) => abs.subsumes(v, v2)
      case _ => false
    }
  }

  case class ControlError(reason: String) extends Control {
    override def toString = s"err($reason)"
    def subsumes(that: Control) = that.equals(this)
  }

  val primitives = new Primitives[Addr, Abs]()
  val initialEnv = Environment.empty[Addr]().extend(primitives.forEnv)
  val initialStore = Store.initial[Addr, Abs](primitives.forStore)

  trait KontAddr
  case class NormalKontAddress(exp: Exp, ρ: Environment[Addr]) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  case class State(control: Control, σ: Store[Addr, Abs], kstore: KontStore[KontAddr], k: KontAddr) {
    def this(exp: Exp) = this(ControlEval(exp, initialEnv), initialStore,
                              new KontStore[KontAddr](), HaltKontAddress)
    override def toString() = control.toString(σ)
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && kstore.subsumes(that.kstore) && k.equals(that.k)

    private def integrate(k: KontAddr, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
      actions.map({
        case ActionReachedValue(v, σ) => State(ControlKont(v), σ, kstore, k)
        case ActionPush(e, frame, ρ, σ) => {
          val next = new NormalKontAddress(e, ρ)
          State(ControlEval(e, ρ), σ, kstore.extend(next, Kont(frame, k)), next)
        }
        case ActionEval(e, ρ, σ) => State(ControlEval(e, ρ), σ, kstore, k)
        case ActionStepIn(_, e, ρ, σ, _) => State(ControlEval(e, ρ), σ, kstore, k)
        case ActionError(err) => State(ControlError(err), σ, kstore, k)
      })

    def step(sem: Semantics[Exp, Abs, Addr]): Set[State] = control match {
      case ControlEval(e, ρ) => integrate(k, sem.stepEval(e, ρ, σ))
      case ControlKont(v) if abs.isError(v) => Set()
      case ControlKont(v) => kstore.lookup(k).foldLeft(Set[State]())((acc, k) => k match {
        case Kont(frame, next) => acc ++ integrate(next, sem.stepKont(v, σ, frame))
      })
      case ControlError(_) => Set()
    }

    def halted = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) =>
        k.equals(HaltKontAddress) || abs.isError(v)
      case ControlError(_) => true
    }
  }

  case class Configuration(control: Control, k: KontAddr) {
    override def toString = s"($control, $k)"
  }
  case class States(R: Set[Configuration], σ: Store[Addr, Abs], kstore: KontStore[KontAddr]) {
    def this(exp: Exp) = this(Set(Configuration(ControlEval(exp, initialEnv),
                                                HaltKontAddress)),
                              initialStore, new KontStore[KontAddr]())
    override def toString = R.toString
    def step(sem: Semantics[Exp, Abs, Addr]): States = {
      val states = R.map(conf => State(conf.control, σ, kstore, conf.k))
      val succs = states.flatMap(ς => ς.step(sem))
      val (σ1, kstore1) = succs.foldLeft((Store.empty[Addr, Abs](), new KontStore[KontAddr]()))((acc, ς) => (acc._1.join(ς.σ), acc._2.join(ς.kstore)))
      States(succs.map(ς => Configuration(ς.control, ς.k)), σ1, kstore1)
    }
    def isEmpty = R.isEmpty
    def toStateSet: Set[State] = R.map({ case Configuration(control, k) => State(control, σ, kstore, k) })
    def size: Int = R.size
  }

  case class FreeOutput(halted: Set[State], count: Int, t: Double, graph: Option[Graph[State]])
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def numberOfStates = count
    def time = t
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toString.take(40),
        (s) => if (halted.contains(s)) { "#FFFFDD" } else { s.control match {
          case ControlEval(_, _) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
          case ControlError(_) => "#FF0000"
        }})
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  @scala.annotation.tailrec
  private def loopWithLocalGraph(s: States, visited: Set[States],
    startingTime: Long, graph: Graph[State], sem: Semantics[Exp, Abs, Addr]): Output[Abs] = {
    val s2 = s.step(sem)
    if (s2.isEmpty) {
      FreeOutput(s.toStateSet.filter(_.halted),
        visited.foldLeft(0)((acc, s) => acc + s.size),
        (System.nanoTime - startingTime) / Math.pow(10, 9), Some(graph))
    } else {
      /* TODO: we lose the "for free" when constructing the graph, since we have to
       * take every possible combination of configurations and draw edges
       * between them */
      val g = graph.addEdges(s.toStateSet.flatMap(ς1 =>
        s2.toStateSet.map(ς2 => (ς1, ς2))))
      if (visited.contains(s2)) {
        FreeOutput(s2.toStateSet,
          visited.foldLeft(0)((acc, s) => acc + s.size),
          (System.nanoTime - startingTime) / Math.pow(10, 9), Some(g))
      } else {
        loopWithLocalGraph(s2, visited + s, startingTime, g, sem)
      }
    }
  }

  private def loop(s: States, visited: Set[States],
    halted: Set[State], startingTime: Long,
    sem: Semantics[Exp, Abs, Addr]): Output[Abs] = {
    val s2 = s.step(sem)
    val h = halted ++ s.toStateSet.filter(_.halted)
    if (s2.isEmpty || visited.contains(s2)) {
      FreeOutput(h, visited.foldLeft(0)((acc, s) => acc + s.size),
        (System.nanoTime - startingTime) / Math.pow(10, 9),
        None)
    } else {
      loop(s2, visited + s, h, startingTime, sem)
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr], graph: Boolean): Output[Abs] =
    if (graph) {
      loop(new States(exp), Set(), Set(), System.nanoTime, sem)
    } else {
      loopWithLocalGraph(new States(exp), Set(), System.nanoTime, new Graph[State](), sem)
    }
}
