package scalaam.machine

import scalaam.graph._
import Graph.GraphOps
import scalaam.core._

/**
  * Implementation of a CESK machine following the AAM approach (Van Horn, David,
  * and Matthew Might. "Abstracting abstract machines." ACM Sigplan
  * Notices. Vol. 45. No. 9. ACM, 2010).
  *
  * A difference with the paper is that we separate the continuation store
  * from the value store. That simplifies the implementation
  * of both stores, and the only change it induces is that we are not able to
  * support first-class continuation as easily (we don't support them at all, but
  * they could be added).
  *
  * Also, in the paper, a CESK state is made of 4 components: Control,
  * Environment, Store, and Kontinuation. Here, we include the environment in the
  * control component, and we distinguish "eval" states from "continuation"
  * states. An eval state has an attached environment, as an expression needs to
  * be evaluated within this environment, whereas a continuation state only
  * contains the value reached.

  * E are used as context for the timestamp
  */
class AAM[E <: Exp, A <: Address, V, T](val sem: Semantics[E, A, V, T, E])(
    implicit val timestamp: Timestamp[T, E],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[E, A, V, T, E] with AAMUtils[E, A, V, T] {

  val Action = sem.Action

  /**
    * A machine state is made of a control component, a value store, a
    * continuation store, and an address representing where the current
    * continuation lives.
    */
  case class State(control: Control, store: Store[A, V], kstore: Store[KA, Set[Kont]], a: KA, t: T)
      extends GraphElement
      with SmartHash {
    override def toString = control.toString

    override def label = toString
    override def color = if (halted) { Colors.Yellow } else {
      control match {
        case _: ControlEval  => Colors.Green
        case _: ControlKont  => Colors.Pink
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

    /**
      * Checks if the current state is a final state. It is the case if it
      * reached the end of the computation, or an error
      */
    def halted: Boolean = control match {
      case _: ControlEval  => false
      case _: ControlKont  => a == HaltKontAddr
      case _: ControlError => true
    }

    /**
      * Integrates a set of actions (returned by the semantics, see
      * Semantics.scala), in order to generate a set of states that succeeds this
      * one.
      */
    private def integrate(a: KA, actions: Set[Action.A]): Set[State] =
      actions.flatMap({
        /* When a value is reached, we go to a continuation state */
        case Action.Value(v, store) =>
          Set(State(ControlKont(v), store, kstore, a, Timestamp[T, E].tick(t)))
        /* When a continuation needs to be pushed, push it in the continuation store */
        case Action.Push(frame, e, env, store) => {
          val next = KontAddr(e, t)
          Set(
            State(ControlEval(e, env),
                  store,
                  kstore.extend(next, Set(Kont(frame, a))),
                  next,
                  Timestamp[T, E].tick(t)))
        }
        /* When a value needs to be evaluated, we go to an eval state */
        case Action.Eval(e, env, store) =>
          Set(State(ControlEval(e, env), store, kstore, a, Timestamp[T, E].tick(t)))
        /* When a function is stepped in, we also go to an eval state */
        case Action.StepIn(fexp, _, e, env, store) =>
          Set(State(ControlEval(e, env), store, kstore, a, Timestamp[T, E].tick(t, fexp)))
        /* When an error is reached, we go to an error state */
        case Action.Err(err) =>
          Set(State(ControlError(err), store, kstore, a, Timestamp[T, E].tick(t)))
      })

    /**
      * Computes the set of states that follow the current state
      */
    def step: Set[State] = control match {
      /** In a eval state, call the semantic's evaluation method */
      case ControlEval(e, env) => integrate(a, sem.stepEval(e, env, store, t))
      /** In a continuation state, call the semantics' continuation method */
      case ControlKont(v) =>
        kstore.lookup(a) match {
          case Some(konts) =>
            konts.flatMap({
              case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, store, t))
            })
          case None => Set()
        }
      /** In an error state, the state is not able to make a step */
      case ControlError(_) => Set()
    }
  }

  object State {
    def inject(exp: E, env: Environment[A], store: Store[A, V]) =
      State(ControlEval(exp, env),
            store,
            Store.empty[KA, Set[Kont]],
            HaltKontAddr,
            Timestamp[T, E].initial(""))

    /** TODO: do this without typeclass, e.g., class State extends WithKey[KA](a) */
    implicit val stateWithKey = new WithKey[State] {
      type K = KA
      def key(st: State) = st.a
    }
    implicit val stateWithKey2 = new WithKey2[State] {
      type K1 = KA
      type K2 = Control
      def key1(st: State) = st.a
      def key2(st: State) = st.control
    }
  }

  type Transition = NoTransition
  val empty = new NoTransition

  /**
    * Performs the evaluation of an expression @param exp (more generally, a
    * program) under the given semantics @param sem. If @param graph is true, it
    * will compute and generate the graph corresponding to the execution of the
    * program (otherwise it will just visit every reachable state). A @param
    * timeout can also be given.
    */
  def run[G](program: E, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    val fvs = program.fv
    val initialEnv = Environment.initial[A](sem.initialEnv).restrictTo(fvs)
    val initialStore = Store.initial[A, V](sem.initialStore)
    val initialState = State.inject(program, initialEnv, initialStore)
    val worklist = scala.collection.mutable.Queue(initialState)
    val visited = scala.collection.mutable.Map[KA, Set[State]]().withDefaultValue(Set.empty[State])
    var graph = Future { Graph[G, State, Transition].empty }

    while (!timeout.reached && !worklist.isEmpty) {
      val s = worklist.dequeue
      if (!visited(s.a).contains(s) && !s.halted) {
        /* unvisited non-halted state */
        val succs = s.step
        graph = graph.map(g => g.addEdges(succs.map(s2 => (s, empty, s2))))
        visited += ((s.a, visited(s.a) + s))
        worklist ++= succs
      }
    }
    Await.result(graph, Duration.Inf)
  }
}
