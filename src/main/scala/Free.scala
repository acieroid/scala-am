import AbstractValue._

/**
 * Implementation of "Pushdown Control-Flow Analysis for Free", which is
 * basically a variant of AAC with better complexity
 * TODO: global store & kstore
 */
case class Free[Abs, Addr, Exp : Expression](sem: Semantics[Exp, Abs, Addr])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                                                             addr: Address[Addr], addri: AddressInjection[Addr]) {
  sealed abstract class Control {
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
    override def toString(store: Store[Addr, Abs]) = s"ko(${abs.toString(v, store)}"
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
        case ActionStepIn(_, e, ρ, σ) => State(ControlEval(e, ρ), σ, kstore, k)
        case ActionError(err) => State(ControlError(err), σ, kstore, k)
      })

    def step: Set[State] = control match {
      case ControlEval(e, ρ) => integrate(k, sem.stepEval(e, ρ, σ))
      case ControlKont(v) if abs.isError(v) => Set()
      case ControlKont(v) => kstore.lookup(k).foldLeft(Set[State]())((acc, k) => k match {
        case Kont(frame, next) => acc ++ integrate(next, sem.stepKont(v, σ, frame))
      })
      case ControlError(_) => Set()
    }

    def halted = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => k.equals(HaltKontAddress) || abs.isError(v)
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
    def step: States = {
      val states = R.map(conf => State(conf.control, σ, kstore, conf.k))
      val succs = states.flatMap(ς => ς.step)
      val (σ1, kstore1) = succs.foldLeft((Store.empty[Addr, Abs](), new KontStore[KontAddr]()))((acc, ς) => (acc._1.join(ς.σ), acc._2.join(ς.kstore)))
      States(succs.map(ς => Configuration(ς.control, ς.k)), σ1, kstore1)
    }
    def isEmpty = R.isEmpty
    def toStateSet: Set[State] = R.map({ case Configuration(control, k) => State(control, σ, kstore, k) })
  }

  @scala.annotation.tailrec
  private def loopLocal(todo: Set[State], visited: Set[State], halted: Set[State], graph: Graph[State]): (Set[State], Graph[State]) = {
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s) || visited.exists(s2 => s2.subsumes(s))) {
          loopLocal(todo.tail, visited, halted, graph)
        } else if (s.halted) {
          loopLocal(todo.tail, visited + s, halted + s, graph)
        } else {
          val succs = s.step
          val newGraph = graph.addEdges(succs.map(s2 => (s, s2)))
          loopLocal(todo.tail ++ succs, visited + s, halted, newGraph)
        }
      case None => (halted, graph)
    }
  }

  def outputLocalDot(graph: Graph[State], path: String) =
    graph.toDotFile(path, _.toString.take(40), _.control match {
      case ControlEval(_, _) => "#DDFFDD"
      case ControlKont(_) => "#FFDDDD"
      case ControlError(_) => "#FF0000"
    })

  def evalLocal(exp: Exp, dotfile: Option[String]): Set[State] = {
    loopLocal(Set(new State(exp)), Set(), Set(), new Graph[State]()) match {
      case (halted, graph: Graph[State]) => {
        println(s"${graph.size} states")
        dotfile match {
          case Some(file) => outputLocalDot(graph, file)
          case None => ()
        }
        halted
      }
    }
  }

  @scala.annotation.tailrec
  private def loopWithLocalGraph(s: States, visited: Set[States], graph: Graph[State]): (Set[State], Graph[State]) = {
    val s2 = s.step
    if (s2.isEmpty || visited.size > 500) {
      (s.toStateSet, graph)
    } else {
      /* TODO: we probably lose the "for free" when constructing the graph, since we
       * have to take every possible combination of configurations and draw
       * edges between them */
      val g = graph.addEdges(s.toStateSet.flatMap(ς1 => s2.toStateSet.map(ς2 => (ς1, ς2))))
      if (visited.contains(s2)) {
        (s2.toStateSet, g)
      } else {
        loopWithLocalGraph(s2, visited + s, g)
      }
    }
  }

  private def loop(s: States, visited: Set[States], halted: Set[State], graph: Graph[States]): (Set[State], Graph[States]) = {
    val s2 = s.step
    val h = halted ++ s.toStateSet.filter(_.halted)
    if (s2.isEmpty) {
      (h, graph)
    } else {
      val g = graph.addEdge(s, s2)
      if (visited.contains(s2)) {
        (h, g)
      } else {
        loop(s2, visited + s, h, g)
      }
    }
  }

  def outputDot(graph: Graph[States], path: String) =
    graph.toDotFile(path, _.toString.take(40), _ => "#FFFFFF")

  def evalNoGraph(exp: Exp): Set[State] = {
    loop(new States(exp), Set(), Set(), new Graph[States]()) match {
      case (halted, graph: Graph[States]) => {
        val (states, confs) = (graph.size, graph.foldNodes(0)((acc, st) => acc + st.R.size))
        println(s"$states states, $confs configurations")
        halted
      }
    }
  }

  def evalBuildGraph(exp: Exp, dotfile: String): Set[State] = {
    loopWithLocalGraph(new States(exp), Set(), new Graph[State]()) match {
      case (halted, graph: Graph[State]) => {
        println(s"${graph.size} states")
        outputLocalDot(graph, dotfile)
        halted
      }
    }
  }

  def eval(exp: Exp, dotfile: Option[String]): Set[State] = dotfile match {
    case Some(file) => evalBuildGraph(exp, file)
    case None => evalNoGraph(exp)
  }
}
