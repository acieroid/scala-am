package scalaam.machine

import scalaam.graph._
import Graph.GraphOps
import scalaam.core._

/** This is a global-store AAM-like machine, where local continuations are used
  * (only looping continuations are pushed on the kont store) */
class GAAM[E <: Exp, A <: Address, V, T](val sem: Semantics[E, A, V, T, E])(
    implicit val timestamp: Timestamp[T, E],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[E, A, V, T, E] with AAMUtils[E, A, V, T] {

  val Action = sem.Action

  val store: GlobalStore[A, V] = GlobalStore.initial[A, V](sem.initialStore)
  val kstore: GlobalStore[KA, Set[LKont]] = GlobalStore.empty[KA, Set[LKont]]

  case class State(control: Control, lkont: LKont, t: T) extends GraphElement with SmartHash {
    override def toString = control.toString
    override def label    = toString
    override def color = if (halted) { Colors.Yellow } else {
      control match {
        case _: ControlEval  => Colors.Green
        case _: ControlKont  => Colors.Pink
        case _: ControlError => Colors.Red
      }
    }
    override def metadata = GraphMetadataNone
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(_)    => lkont.next == HaltKontAddr && lkont.isEmpty
      case ControlError(_)   => true
    }

    private def integrate(actions: Set[Action.A]): Set[State] = {
      actions.map({
        case Action.Value(v, _) =>
          State(ControlKont(v), lkont, Timestamp[T, E].tick(t))
        case Action.Push(frame, e, env, _) =>
          State(ControlEval(e, env), lkont.push(frame), Timestamp[T, E].tick(t))
        case Action.Eval(e, env, _) =>
          State(ControlEval(e, env), lkont, Timestamp[T, E].tick(t))
        case Action.StepIn(fexp, _, e, env, _) =>
          val next = KontAddr(e, t)
          kstore.extend(next, Set(lkont))
          State(ControlEval(e, env),
            LKont.empty(next),
            Timestamp[T, E].tick(t, fexp))
        case Action.Err(err) =>
          State(ControlError(err), lkont, Timestamp[T, E].tick(t))
      })
    }

    def step: Set[State] =
      control match {
        case ControlEval(e, env) => integrate(sem.stepEval(e, env, store, t))
        case ControlKont(v)      =>
          /* XXX This case should be double checked */
          lkont.get match {
            case Some((frame, rest)) =>
              /* If we have a non-empty lkont, we can pop its first element */
              this.copy(lkont = rest).integrate(sem.stepKont(v, frame, store, t))
            case None =>
              /* Otherwise, find the next kont that has a non-empty lkont */
              val konts = lkont.findKonts(kstore)
              konts.flatMap(lkont => {
                if (lkont.isEmpty && lkont.next == HaltKontAddr) {
                  if (halted) {
                    /* If this is a halted state with an empty kont, we stop. */
                    /* TODO: this case might not be necessary? */
                    Set()
                  } else {
                    /* The kont may be empty but we still have to evaluate something */
                    Set(this.copy(lkont = lkont))
                  }
                } else {
                  this.copy(lkont = lkont).step
                }
              })
          }
        case ControlError(_) => Set()
      }
  }

  object State {
    def inject(exp: E, env: Environment[A]): State =
      State(ControlEval(exp, env), LKont.empty(HaltKontAddr), Timestamp[T, E].initial(""))
    implicit val stateWithKey = new WithKey[State] {
      type K = KA
      def key(st: State) = st.lkont.next
    }
    implicit val stateWithKey2 = new WithKey2[State] {
      type K1 = KA
      type K2 = Control
      def key1(st: State) = st.lkont.next
      def key2(st: State) = st.control
    }
  }

  type Transition = NoTransition
  val empty = new NoTransition

  def run[G](program: E, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    object VS extends MapVisitedSetImpl[State]
    import VS._
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    var graph = Future { Graph[G, State, Transition].empty }

    @scala.annotation.tailrec
    /* An invariant is that for all states in todo, stores(state) is defined */
    def loop(todo: Set[State], visited: VisitedSet.T): Unit = {
      if (todo.isEmpty || timeout.reached) {
        ()
      } else {
        /* Frontier-based semantics */
        val successors = todo.flatMap(s => {
          val succs = s.step
          graph = graph.map(g => g.addEdges(succs.map(s2 => (s, empty, s2))))
          succs
        })
        val shouldClearVisited = store.mutated || kstore.mutated
        if (shouldClearVisited) {
          /* Changes in stores/kstore, we have to clear visited set */
          store.clearMutated()
          kstore.clearMutated()
          loop(successors, VisitedSet.empty)
        } else {
          loop(successors.filter(s2 => !VisitedSet.contains(visited, s2)),
               VisitedSet.append(visited, todo))
        }
      }
    }
    val fvs = program.fv
    val initialEnv = Environment.initial[A](sem.initialEnv).restrictTo(fvs)
    val state = State.inject(program, initialEnv)
    loop(Set(state), VisitedSet.empty)
    Await.result(graph, Duration.Inf)
  }
}
