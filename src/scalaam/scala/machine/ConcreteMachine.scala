package scalaam.machine

import scalaam.graph._
import Graph.GraphOps
import scalaam.core._

/** Similar to BasicStore, but always perform strong updates when update is called */
case class ConcreteStore[A <: Address, V](val content: Map[A, V])(implicit val lat: Lattice[V])
    extends Store[A, V] {
  override def toString              = content.filterKeys(_.printable).mkString("\n")
  def keys                           = content.keys
  def forall(p: ((A, V)) => Boolean) = content.forall({ case (a, v) => p((a, v)) })
  def lookup(a: A)                   = content.get(a)
  def extend(a: A, v: V) = content.get(a) match {
    case None     => new ConcreteStore[A, V](content + (a -> v))
    case Some(v2) =>
      println(s"Losing precision on concrete store when extending key $a with new value $v (old value is $v2)")
      new ConcreteStore[A, V](content + (a -> lat.join(v, v2)))
  }
  override def update(a: A, v: V) = new ConcreteStore(content + (a -> v)) /* No presence check */
  def join(that: Store[A, V]) =
    /* In practice, join shouldn't be called on a concrete store */
    keys.foldLeft(that)((acc, k) => lookup(k).fold(acc)(v => acc.extend(k, v)))
  def subsumes(that: Store[A, V]) =
    that.forall((binding: (A, V)) =>
      content.get(binding._1).exists(v => lat.subsumes(v, binding._2)))
}


class ConcreteMachine[Exp, A <: Address, V, T](val sem: Semantics[Exp, A, V, T, Exp])(
  implicit val timestamp: Timestamp[T, Exp],
  implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp] {

  val Action = sem.Action

  object ControlComp extends ControlComponent[Exp, A, V]
  import ControlComp._

  case class State(control: Control, store: Store[A, V], konts: List[Frame], t: T)
      extends GraphElement
      with SmartHash {
    override def toString = control.toString
    override def label = toString
    override def color = if (halted) { Colors.Yellow } else {
      control match {
        case _: ControlEval => Colors.Green
        case _: ControlKont => Colors.Pink
        case _: ControlError => Colors.Red
      }
    }
    override def metadata =
      GraphMetadataMap(
        Map(
          "halted" -> GraphMetadataBool(halted),
          "type" -> (control match {
            case _: ControlEval  => GraphMetadataString("eval")
            case _: ControlKont  => GraphMetadataString("kont")
            case _: ControlError => GraphMetadataString("error")
          })
        ) ++ (control match {
          case ControlKont(v) => Map("value" -> GraphMetadataValue[V](v))
          case _              => Map()
        }))

    def halted: Boolean = control match {
      case _: ControlEval  => false
      case _: ControlKont  => konts.isEmpty
      case _: ControlError => true
    }
  }

  type Transition = NoTransition
  val empty = new NoTransition

  def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    var state = State(ControlEval(program, Environment.initial[A](sem.initialEnv)), new ConcreteStore[A, V](sem.initialStore.toMap), List(), Timestamp[T, Exp].initial(""))
    var graph = Graph[G, State, Transition].empty
    var finished = false
    while (!finished) {
      def applyAction(konts: List[Frame], actions: Set[Action.A]): Unit = {
        if (actions.size == 0) {
          println("Got no action while one was expected. Terminating concrete machine.")
          finished = true;
        } else {
          if (actions.size > 1) println(s"Got more than one action in concrete machine. Picking the first one.")
          val state2 = actions.head match {
            case Action.Value(v, store2) => State(ControlKont(v), store2, konts, Timestamp[T, Exp].tick(state.t))
            case Action.Push(frame, e, env, store2) => State(ControlEval(e, env), store2, frame :: konts, Timestamp[T, Exp].tick(state.t))
            case Action.Eval(e, env, store2) => State(ControlEval(e, env), store2, konts, Timestamp[T, Exp].tick(state.t))
            case Action.StepIn(fexp, _, e, env, store2) => State(ControlEval(e, env), store2, konts, Timestamp[T, Exp].tick(state.t, fexp))
            case Action.Err(err) => State(ControlError(err), state.store, konts, Timestamp[T, Exp].tick(state.t))
          }
          state = state2
          graph = graph.addEdge(state, empty, state2)
        }
      }
      if (timeout.reached) {
        finished = true
      } else {
        state.control match {
          case ControlEval(e, env) =>
            applyAction(state.konts, sem.stepEval(e, env, state.store, state.t))
          case ControlKont(v) =>
            state.konts match {
              case frame :: tl =>
                applyAction(tl, sem.stepKont(v, frame, state.store, state.t))
              case Nil =>
                println(s"Concrete machine finished its execution with value $v")
                finished = true
            }
          case ControlError(err) =>
            println(s"Concrete machine finished its execution with error $err")
            finished = true
        }
      }
    }
    graph
  }
}
