import AbstractValue._

/**
  * Implementation of a CESK machine for ANF following the AAM approach
  */
case class AAM[Abs, Addr, Exp : Expression](sem: Semantics[Exp, Abs, Addr])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                                                            addr: Address[Addr], addri: AddressInjection[Addr]) {
  /** The control component represents what needs to be evaluated; it can either be an expression or a continuation */
  sealed abstract class Control {
    def subsumes(that: Control): Boolean
  }
  case class ControlEval(exp: Exp, env: Environment[Addr]) extends Control {
    override def toString() = s"ev(${exp.toString})"
    def subsumes(that: Control) = that match {
      case ControlEval(exp2, env2) => exp.equals(exp2) && env.subsumes(env2)
      case _ => false
    }
  }
  case class ControlKont(v: Abs) extends Control {
    override def toString() = s"ko(${v.toString})"
    def subsumes(that: Control) = that match {
      case ControlKont(v2) => abs.subsumes(v, v2)
      case _ => false
    }
  }
  case class ControlError(reason: String) extends Control {
    override def toString() = s"err($reason)"
    def subsumes(that: Control) = that.equals(this)
  }

  val primitives = new Primitives[Abs, Addr]()
  case class State(control: Control, σ: Store[Addr, Abs], a: Addr) {
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
                              Store.empty[Addr, Abs]().extend(primitives.forStore), addri.halt)
    override def toString() = control.toString
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && addr.subsumes(a, that.a)
    private def integrate(actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
      actions.map({
        case ActionReachedValue(v, σ, a) => State(ControlKont(v), σ, a)
        case ActionPush(e, κ, ρ, σ) => {
          val a = addri.kont(e)
          State(ControlEval(e, ρ), σ.extend(a, absi.inject(κ)), a)
        }
        case ActionEval(e, ρ, σ, a) => State(ControlEval(e, ρ), σ, a)
        case ActionError(err) => State(ControlError(err), σ, a)
      })
    def step: Set[State] = integrate(control match {
      case ControlEval(e, ρ) => sem.stepEval(e, ρ, σ, a)
      case ControlKont(v) => abs.foldValues(σ.lookup(a),
                                            (v2) => abs.getKont(v2) match {
                                              case Some(κ) => sem.stepKont(v, σ, κ)
                                              case None => Set()
                                            })
      case ControlError(_) => Set()
    })
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(_) => a.equals(addri.halt)
      case ControlError(_) => true
    }
  }

  def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Graph[State]): (Set[State], Graph[State]) =
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s) || visited.exists(s2 => s2.subsumes(s))) {
          loop(todo.tail, visited, halted, graph)
        } else if (s.halted) {
          loop(todo.tail, visited + s, halted + s, graph)
        } else {
          val succs = s.step
          val newGraph = graph.addEdges(succs.map(s2 => (s, s2)))
          loop(todo.tail ++ succs, visited + s, halted, newGraph)
        }
      case None => (halted, graph)
    }

  def outputDot(graph: Graph[State], path: String) =
    graph.toDotFile(path, _.toString.take(40), _.control match {
      case ControlEval(_, _) => "#DDFFDD"
      case ControlKont(_) => "#FFDDDD"
      case ControlError(_) => "#FF0000"
    })

  def eval(exp: Exp, dotfile: Option[String]): Set[State] = {
    val state = new State(exp)
    loop(Set(state), Set(), Set(), new Graph[State](state)) match {
      case (halted, graph: Graph[State]) => {
        println(s"${graph.size} states")
        dotfile match {
          case Some(file) => outputDot(graph, file)
          case None => ()
        }
        halted
      }
    }
  }
}
