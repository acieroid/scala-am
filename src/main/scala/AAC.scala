import AbstractValue._
import scalaz.Scalaz._

/**
 * Implementation of Johnson's CESIK*Ξ machine with a global continuation store
 */
case class AAC[Exp : Expression, Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr])
    extends AbstractMachine[Exp, Abs, Addr] {
  def abs = implicitly[AbstractValue[Abs]]
  def absi = implicitly[AbstractInjection[Abs]]
  def addr = implicitly[Address[Addr]]
  def addri = implicitly[AddressInjection[Addr]]
  def exp = implicitly[Expression[Exp]]

  def name = "AAC"

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

  case class Context(clo: (Exp, Environment[Addr]), argsv: List[(Exp, Abs)], σ: Store[Addr, Abs]) {
    def subsumes(that: Context) = {
      clo._1.equals(that.clo._1) && clo._2.subsumes(that.clo._2) && σ.subsumes(that.σ) &&
      argsv.zip(that.argsv).forall({ case ((e1, v1), (e2, v2)) => e1 == e2 && abs.subsumes(v1, v2) })
    }
  }

  sealed abstract class Kont {
    def subsumes(that: Kont) : Boolean
  }
  case class KontCtx(ctx: Context) extends Kont {
    def subsumes(that: Kont) = that match {
      case KontCtx(ctx2) => ctx.subsumes(ctx2)
      case _ => false
    }
    override def toString = s"KontCtx(${ctx.clo._1}, ${ctx.argsv})"
  }
  object KontEmpty extends Kont {
    def subsumes(that: Kont) = that.equals(this)
    override def toString = "KontEmpty"
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
    def extend(τ: Context, v: (LocalKont, Kont)): KontStore = {
      /*
      content.get(τ) match {
        case Some(vals) if !vals.contains(v) => println(s"Joining at $τ: $v + $vals")
        case _ => ()
      } */
      KontStore(content + (τ -> (lookup(τ) + v)))
    }
    def join(that: KontStore): KontStore = KontStore(content |+| that.content)
    /** Useful for debugging purposes, in order to have a visualization of the
      * kontinuation store */
    def toDotFile(file: String): Unit = {
      val graph = content.foldLeft(new Graph[Kont]())({ case (g, (τ, succs)) =>
        succs.foldLeft(g)({ case (g, (local, κ)) =>
          // TODO: annotate with local
          g.addEdge(KontCtx(τ), κ)
        })
      })
      graph.toDotFile(file, {
        case KontCtx(τ) => τ.toString.take(40)
        case KontEmpty => "ε"
      }, x => "#FFFFFF")
    }
  }

  case class State(control: Control, σ: Store[Addr, Abs], ι: LocalKont, κ: Kont) {
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
                               Store.initial[Addr, Abs](primitives.forStore), new LocalKont(), KontEmpty)
    override def toString() = control.toString(σ)
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && ι.subsumes(that.ι) && κ.subsumes(that.κ)

    /* TODO: functions inspecting the continuation can probably be factored into a
     * single function with the stack-walking mechanics, and some helper
     * functions telling what to do when encountering each kind of stack */
    private def pop(ι: LocalKont, κ: Kont, kstore: KontStore, G: Set[Kont]): Set[(Frame, LocalKont, Kont)] = ι.deconstruct match {
      case None => κ match {
        case KontEmpty => Set()
        case KontCtx(τ) => {
          val G2: Set[Kont] = kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set(κ)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2)
          kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set[(Frame, LocalKont, Kont)]()
              case Some((top, rest)) => Set((top, rest, κ))
            }
          }).union(G2.flatMap((κ) => pop(new LocalKont(), κ, kstore, GuG2)))
        }
      }
      case Some((top, rest)) => Set((top, rest, κ))
    }

    private def pop(ι: LocalKont, κ: Kont, kstore: KontStore): Set[(Frame, LocalKont, Kont)] =
      pop(ι, κ, kstore, Set())

    private def computeKont(ι: LocalKont, κ: Kont, kstore: KontStore, G: Set[Kont]): Set[List[Frame]] = ι.deconstruct match {
      case None => κ match {
        case KontEmpty => Set(List())
        case KontCtx(τ) => {
          val G2: Set[Kont] = kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set(κ)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2)
          kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set[List[Frame]]()
              case Some((top, rest)) => computeKont(rest, κ, kstore, GuG2).map(k => top :: k)
            }
          }).union(G2.flatMap((κ) => computeKont(new LocalKont(), κ, kstore, GuG2)))
        }
      }
      case Some((top, rest)) => computeKont(rest, κ, kstore, G).map(k => top :: k)
    }

    private def computeKont(ι: LocalKont, κ: Kont, kstore: KontStore): Set[List[Frame]] =
      computeKont(ι, κ, kstore, Set())

    private def kontCanBeEmpty(ι: LocalKont, κ: Kont, kstore: KontStore, G: Set[Kont]): Boolean = ι.deconstruct match {
      case None => κ match {
        case KontEmpty => true
        case KontCtx(τ) => {
          val G2: Set[Kont] = kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set(κ)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2)
          kstore.lookup(τ).map({
            case (ι, KontEmpty) => true
            case (ι, κ) => ι.deconstruct match {
              case None => false
              case Some((top, rest)) => false
            }
          }).union(G2.map((κ) => kontCanBeEmpty(new LocalKont(), κ, kstore, GuG2))).foldLeft(false)((x, y) => x || y)
        }
      }
      case Some((top, rest)) => false
    }

    private def kontCanBeEmpty(ι: LocalKont, κ: Kont, kstore: KontStore): Boolean =
      kontCanBeEmpty(ι, κ, kstore, Set())

    private def integrate(ι: LocalKont, κ: Kont, kstore: KontStore, v: Abs, actions: Set[Action[Exp, Abs, Addr]]): (Set[State], KontStore) =
      actions.foldLeft((Set[State](), kstore))({ (acc, act) =>
        val states = acc._1
        val kstore = acc._2
        act match {
          case ActionReachedValue(v, σ) => (states + State(ControlKont(v), σ, ι, κ), kstore)
          case ActionPush(e, frame, ρ, σ) => (states + State(ControlEval(e, ρ), σ, ι.push(frame), κ), kstore)
          case ActionEval(e, ρ, σ) => (states + State(ControlEval(e, ρ), σ, ι, κ), kstore)
          case ActionStepIn(clo, e, ρ, σ, argsv) => {
            val τ = Context(clo, argsv, σ)
            (states + State(ControlEval(e, ρ), σ, new LocalKont(), new KontCtx(τ)),
             kstore.extend(τ, (ι, κ)))
          }
          case ActionError(err) => (states + State(ControlError(err), σ, ι, κ), kstore)
        }})

    def step(kstore: KontStore, sem: Semantics[Exp, Abs, Addr]): (Set[State], KontStore) = control match {
      case ControlEval(e, ρ) => integrate(ι, κ, kstore, absi.bottom, sem.stepEval(e, ρ, σ))
      case ControlKont(v) if abs.isError(v) => (Set(), kstore)
      case ControlKont(v) => pop(ι, κ, kstore).foldLeft((Set[State](), kstore))((acc, popped) => {
        val (states, kstore1) = integrate(popped._2, popped._3, acc._2, v, sem.stepKont(v, σ, popped._1))
        (acc._1 ++ states, kstore1)
      })
      case ControlError(_) => (Set(), kstore)
    }

    def halted(kstore: KontStore) = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => abs.isError(v) || (ι.isEmpty && kontCanBeEmpty(ι, κ, kstore))
      case ControlError(_) => true
    }
  }

  case class AACOutput(halted: Set[State], graph: Option[Graph[State]])
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
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

  /* frontier-based state exploration */
  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Option[Graph[State]], kstore: KontStore, sem: Semantics[Exp, Abs, Addr]): AACOutput = {
    if (todo.isEmpty) {
      AACOutput(halted, graph)
    } else {
      val (edges, kstore2) = todo.foldLeft((Set[(State, State)](), kstore))({ (acc, ς) =>
        ς.step(kstore, sem) match {
          case (next, kstore2) =>
            (acc._1 ++ next.map((ς2) => (ς, ς2)), acc._2.join(kstore2))
        }
      })
      if (kstore.equals(kstore2)) {
        loop(edges.map({ case (_, ς2) => ς2 }).diff(visited),
          visited ++ todo,
          halted ++ todo.filter((ς) => ς.halted(kstore)),
          graph.map(_.addEdges(edges)),
          kstore2,
          sem)
      } else {
        /* KontStore changed, discard set of seen states */
        loop(edges.map({ case (_, ς2) => ς2 }),
          Set(),
          halted ++ todo.filter((ς) => ς.halted(kstore)),
          graph.map(_.addEdges(edges)),
          kstore2,
          sem)
      }
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr], graph: Boolean): Output[Abs] =
    loop(Set(new State(exp)), Set(), Set(),
      if (graph) { Some(new Graph[State]()) } else { None },
      new KontStore(), sem)
}
