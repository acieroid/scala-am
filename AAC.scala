import AbstractValue._
import scalaz.Scalaz._

/**
 * Implementation of Johnson's CESIK*Ξ machine with a global continuation store
 */
case class AAC[Abs, Addr, Exp : Expression](sem: Semantics[Exp, Abs, Addr])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                                                            addr: Address[Addr], addri: AddressInjection[Addr]) {
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
    override def toString = s"ko(${v.toString})"
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

  case class Context(clo: (Exp, Environment[Addr]), v: Abs, σ: Store[Addr, Abs]) {
    def subsumes(that: Context) = clo._1.equals(that.clo._1) && clo._2.subsumes(that.clo._2) && abs.subsumes(v, that.v) && σ.subsumes(that.σ)
  }

  sealed abstract class Kont {
    def subsumes(that: Kont) : Boolean
  }
  case class KontCtx(ctx: Context) extends Kont {
    def subsumes(that: Kont) = that match {
      case KontCtx(ctx2) => ctx.subsumes(ctx2)
      case _ => false
    }
  }
  object KontEmpty extends Kont {
    def subsumes(that: Kont) = that.equals(this)
  }

  case class LocalKont(frames: List[Frame]) {
    def this() = this(List())
    def subsumes(that: LocalKont) = frames.zip(that.frames).forall({ case (f1, f2) => f1.subsumes(f2) })
    def isEmpty = frames.isEmpty
    def deconstruct = frames match {
      case List() => None
      case h :: t => Some((h, LocalKont(t)))
    }
    def push(frame: Frame): LocalKont = new LocalKont(frame :: frames)
  }

  case class KontStore(content: Map[Context, Set[(LocalKont, Kont)]]) {
    def this() = this(Map())
    def lookup(τ: Context): Set[(LocalKont, Kont)] = content.getOrElse(τ, Set())
    def extend(τ: Context, v: (LocalKont, Kont)): KontStore = KontStore(content + (τ -> (lookup(τ) + v)))
    def join(that: KontStore): KontStore =
      KontStore(content |+| that.content)
  }


  case class State(control: Control, σ: Store[Addr, Abs], ι: LocalKont, κ: Kont) {
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
                               Store.empty[Addr, Abs]().extend(primitives.forStore), new LocalKont(), KontEmpty)
    override def toString() = control.toString
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && ι.subsumes(that.ι) && κ.subsumes(that.κ)

    private def pop(ι: LocalKont, κ: Kont, Ξ: KontStore, G: Set[Kont]): Set[(Frame, LocalKont, Kont)] =
      ι.deconstruct match {
        case None => κ match {
          case KontEmpty => Set()
          case KontCtx(τ) => {
            val G2: Set[Kont] = Ξ.lookup(τ).flatMap({
              case (ι, κ) => ι.deconstruct match {
                case None => Set(κ)
                case Some(_) => Set[Kont]()
              }
            }).diff(G)
            val GuG2 = G.union(G2)
            Ξ.lookup(τ).flatMap({
              case (ι, κ) => ι.deconstruct match {
                case None => Set[(Frame, LocalKont, Kont)]()
                case Some((top, rest)) => Set((top, rest, κ))
              }
            }).union(G2.flatMap((κ) => pop(new LocalKont(), κ, Ξ, GuG2)))
          }
        }
        case Some((top, rest)) => Set((top, rest, κ))
      }

    private def pop(ι: LocalKont, κ: Kont, Ξ: KontStore): Set[(Frame, LocalKont, Kont)] =
      pop(ι, κ, Ξ, Set())

    private def integrate(ι: LocalKont, κ: Kont, Ξ: KontStore, v: Abs, actions: Set[Action[Exp, Abs, Addr]]): (Set[State], KontStore) =
      actions.foldLeft((Set[State](), Ξ))({ (acc, act) =>
        val states = acc._1
        val Ξ = acc._2
        act match {
          case ActionReachedValue(v, σ) => (states + State(ControlKont(v), σ, ι, κ), Ξ)
          case ActionPush(e, frame, ρ, σ) => (states + State(ControlEval(e, ρ), σ, ι.push(frame), κ), Ξ)
          case ActionEval(e, ρ, σ) => (states + State(ControlEval(e, ρ), σ, ι, κ), Ξ)
          case ActionStepIn(clo, e, ρ, σ) => {
            val τ = Context(clo, v, σ)
            (states + State(ControlEval(e, ρ), σ, new LocalKont(), new KontCtx(τ)),
             Ξ.extend(τ, (ι, κ)))
          }
          case ActionError(err) => (states + State(ControlError(err), σ, ι, κ), Ξ)
        }})

    def step(Ξ: KontStore): (Set[State], KontStore) = control match {
      case ControlEval(e, ρ) => integrate(ι, κ, Ξ, absi.bottom, sem.stepEval(e, ρ, σ))
      case ControlKont(v) => pop(ι, κ, Ξ).foldLeft((Set[State](), Ξ))((acc, popped) => {
        val (states, ksi1) = integrate(popped._2, popped._3, acc._2, v, sem.stepKont(v, σ, popped._1))
        (acc._1 ++ states, ksi1)
      })
      case ControlError(_) => (Set(), Ξ)
    }

    def halted = control match {
      case ControlEval(_, _) => false
      case ControlKont(_) => ι.isEmpty && κ.equals(KontEmpty)
      case ControlError(_) => true
    }
  }

  /* frontier-based state exploration */
  def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Graph[State], Ξ: KontStore): (Set[State], Graph[State]) = {
    if (todo.isEmpty) {
      (halted, graph)
    } else {
      val (edges, xi2) = todo.foldLeft((Set[(State, State)](), Ξ))({ (acc, ς) =>
        ς.step(Ξ) match {
          case (next, xi2) => (acc._1 ++ next.map((ς2) => (ς, ς2)), acc._2.join(xi2))
        }
      })
      loop(edges.map({ case (_, ς2) => ς2 }).diff(visited),
           visited ++ todo,
           halted ++ todo.filter((ς) => ς.halted),
           graph.addEdges(edges),
           xi2)
    }
  }

  def outputDot(graph: Graph[State], path: String) =
    graph.toDotFile(path, _.toString.take(40), _.control match {
      case ControlEval(_, _) => "#DDFFDD"
      case ControlKont(_) => "#FFDDDD"
      case ControlError(_) => "#FF0000"
    })


  def eval(exp: Exp, dotfile: Option[String]): Set[State] = {
    loop(Set(new State(exp)), Set(), Set(), new Graph[State](), new KontStore()) match {
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
