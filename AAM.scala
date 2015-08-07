import AbstractValue._

/**
  * Implementation of a CESK machine following the AAM approach
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

  case class AAMKont(frame: Frame, next: Addr) extends Kontinuation {
    def subsumes(that: Kontinuation) = that match {
      case AAMKont(frame2, next2) => frame.subsumes(frame2) && addr.subsumes(next, next2)
      case _ => false
    }
    def getFrame = frame
  }

  val primitives = new Primitives[Abs, Addr]()
  case class State(control: Control, σ: Store[Addr, Abs], a: Addr) {
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
                              Store.empty[Addr, Abs]().extend(primitives.forStore), addri.halt)
    override def toString() = control.toString
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && addr.subsumes(a, that.a)
    private def integrate(a: Addr, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
      actions.flatMap({
        case ActionReachedValue(v, σ) => Set(State(ControlKont(v), σ, a))
        case ActionPush(e, frame, ρ, σ) => {
          val next = addri.kont(e)
          Set(State(ControlEval(e, ρ), σ.extend(next, absi.inject(AAMKont(frame, a))), next))
        }
        case ActionEval(e, ρ, σ) => Set(State(ControlEval(e, ρ), σ, a))
        case ActionStepIn(_, e, ρ, σ) => Set(State(ControlEval(e, ρ), σ, a))
        case ActionError(err) => Set(State(ControlError(err), σ, a))
      })
    def step: Set[State] = control match {
      case ControlEval(e, ρ) => integrate(a, sem.stepEval(e, ρ, σ))
      case ControlKont(v) => abs.foldValues(σ.lookup(a),
                                            (v2) => { abs.getKont(v2) match {
                                              case Some(κ) => κ match {
                                                case AAMKont(frame, next) => integrate(next, sem.stepKont(v, σ, frame))
                                              }
                                              case None => Set()
                                            }} : Set[State])
      case ControlError(_) => Set()
    }
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
