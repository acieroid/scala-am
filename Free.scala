import AbstractValue._
import scalaz.Scalaz._

/**
 * Implementation of "Pushdown Control-Flow Analysis for Free", which is
 * basically a variant of AAC with better complexity
 * TODO: global store & kstore
 */
case class Free[Abs, Addr, Exp : Expression](sem: Semantics[Exp, Abs, Addr])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                                                             addr: Address[Addr], addri: AddressInjection[Addr]) {
  sealed abstract class Control {
    def subsumes(that: Control): Boolean
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
    def subsumes(that: Control) = that match {
      case ControlKont(v2) => abs.subsumes(v, v2)
      case _ => false
    }
  }

  case class ControlError(reason: String) extends Control {
    override def toString = s"err($reason)"
    def subsumes(that: Control) = that.equals(this)
  }

  val primitives = new Primitives[Abs, Addr]()

  case class Kont(frame: Frame, next: KontAddress) extends Kontinuation {
    def subsumes(that: Kontinuation) = that match {
      case Kont(frame2, next2) => frame.subsumes(frame2) && next.equals(next2)
      case _ => false
    }
    def getFrame = frame
  }

  abstract class KontAddress
  case class NormalKontAddress(exp: Exp, ρ: Environment[Addr]) extends KontAddress
  object HaltKontAddress extends KontAddress

  case class KontStore(content: Map[KontAddress, Set[Kont]]) {
    def this() = this(Map())
    def lookup(a: KontAddress): Set[Kont] = content.getOrElse(a, Set())
    def extend(a: KontAddress, κ: Kont): KontStore = KontStore(content + (a -> (lookup(a) + κ)))
    def join(that: KontStore): KontStore = KontStore(content |+| that.content)
    def forall(p: ((KontAddress, Set[Kont])) => Boolean) = content.forall(p)
    def subsumes(that: KontStore): Boolean =
      that.forall({ case (a, ks) =>
        ks.forall((k1) => lookup(a).exists(k2 => k2.subsumes(k1)))
      })
  }

  case class State(control: Control, σ: Store[Addr, Abs], kstore: KontStore, k: KontAddress) {
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
                              Store.empty[Addr, Abs]().extend(primitives.forStore),
                              new KontStore(), HaltKontAddress)
    override def toString() = control.toString
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && kstore.subsumes(that.kstore) && k.equals(that.k)

    private def integrate(k: KontAddress, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
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
      case ControlKont(v) => kstore.lookup(k).foldLeft(Set[State]())((acc, k) => k match {
        case Kont(frame, next) => acc ++ integrate(next, sem.stepKont(v, σ, frame))
      })
      case ControlError(_) => Set()
    }

    def halted = control match {
      case ControlEval(_, _) => false
      case ControlKont(_) => k.equals(HaltKontAddress)
      case ControlError(_) => true
    }
  }

  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Graph[State]): (Set[State], Graph[State]) = {
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
  }

  def outputDot(graph: Graph[State], path: String) =
    graph.toDotFile(path, _.toString.take(40), _.control match {
      case ControlEval(_, _) => "#DDFFDD"
      case ControlKont(_) => "#FFDDDD"
      case ControlError(_) => "#FF0000"
    })

  def eval(exp: Exp, dotfile: Option[String]): Set[State] = {
    loop(Set(new State(exp)), Set(), Set(), new Graph[State]()) match {
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
