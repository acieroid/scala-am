package scalaam.machine

import scalaam.graph._
import Graph.GraphOps
import scalaam.core._

/** This is an AAM-like machine, where local continuations are used (only
  * looping continuations are pushed on the kont store), and stores are not
  * stored in the states but rather in a separate map. The continuation
  * store itself is global. */
class AAMLKSS[E <: Exp, A <: Address, V, T](val sem: Semantics[E, A, V, T, E])(
    implicit val timestamp: Timestamp[T, E],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[E, A, V, T, E] with AAMUtils[E, A, V, T] {

  val Action = sem.Action

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

    private def integrate(
        actions: Set[Action.A],
        store: Store[A, V],
        kstore: Store[KA, Set[LKont]]): (Set[(State, Store[A, V])], Store[KA, Set[LKont]]) = {
      actions.foldLeft((Set.empty[(State, Store[A, V])], kstore))((acc, act) => {
        val states = acc._1
        val kstore = acc._2
        act match {
          case Action.Value(v, store) =>
            (states + ((State(ControlKont(v), lkont, Timestamp[T, E].tick(t)), store)), kstore)
          case Action.Push(frame, e, env, store) =>
            (states + ((State(ControlEval(e, env), lkont.push(frame), Timestamp[T, E].tick(t)),
                        store)),
             kstore)
          case Action.Eval(e, env, store) =>
            (states + ((State(ControlEval(e, env), lkont, Timestamp[T, E].tick(t)), store)),
             kstore)
          case Action.StepIn(fexp, _, e, env, store) =>
            val next = KontAddr(e, t)
            (states + ((State(ControlEval(e, env),
                              LKont.empty(next),
                              Timestamp[T, E].tick(t, fexp)),
                        store)),
             kstore.extend(next, Set(lkont)))
          case Action.Err(err) =>
            (states + ((State(ControlError(err), lkont, Timestamp[T, E].tick(t)), store)), kstore)
        }
      })
    }

    def step(store: Store[A, V],
             kstore: Store[KA, Set[LKont]]): (Set[(State, Store[A, V])], Store[KA, Set[LKont]]) =
      control match {
        case ControlEval(e, env) => integrate(sem.stepEval(e, env, store, t), store, kstore)
        case ControlKont(v)      =>
          /* XXX This case should be double checked */
          lkont.get match {
            case Some((frame, rest)) =>
              /* If we have a non-empty lkont, we can pop its first element */
              this.copy(lkont = rest).integrate(sem.stepKont(v, frame, store, t), store, kstore)
            case None =>
              /* Otherwise, find the next kont that has a non-empty lkont */
              val konts = lkont.findKonts(kstore)
              konts.foldLeft((Set.empty[(State, Store[A, V])], kstore))((acc, lkont) => {
                val states = acc._1
                val kstore = acc._2
                if (lkont.isEmpty && lkont.next == HaltKontAddr) {
                  if (halted) {
                    /* If this is a halted state with an empty kont, we stop. */
                    /* TODO: this case might not be necessary? */
                    acc
                  } else {
                    /* The kont may be empty but we still have to evaluate something */
                    (states + ((this.copy(lkont = lkont), store)), kstore)
                  }
                } else {
                  this.copy(lkont = lkont).step(store, kstore) match {
                    case (states2, kstore2) => (states ++ states2, kstore2)
                  }
                }
              })
          }
        case ControlError(_) => (Set(), kstore)
      }
  }

  object State {
    def inject(exp: E, env: Environment[A]): State =
      State(ControlEval(exp, env), LKont.empty(HaltKontAddr), Timestamp[T, E].initial(""))
    implicit val stateWithKey = new WithKey[State] {
      type K = KA
      def key(st: State) = st.lkont.next
    }
  }

  type Transition = NoTransition
  val empty = new NoTransition

  class StoreMap(val content: Map[State, Store[A, V]]) {
    def add(kv: (State, Store[A, V])): (StoreMap, Boolean) =
      if (content.contains(kv._1)) {
        val contents2 = content + (if (!kv._2.subsumes(content(kv._1))) {
          (kv._1 -> ((content(kv._1).join(kv._2))))
        } else {
          kv
        })
        (new StoreMap(contents2), (contents2(kv._1) != content(kv._1)))
      } else {
        (new StoreMap(content + kv), true)
      }
    def apply(k: State): Store[A, V] = content(k)
  }
  object StoreMap {
    def apply(kv: (State, Store[A, V])): StoreMap =
      new StoreMap(Map(kv))
  }

  def run[G](program: E, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    import scala.language.higherKinds

    @scala.annotation.tailrec
    /* An invariant is that for all states in todo, stores(state) is defined */
    def loop[VS[_]: VisitedSet](todo: Set[State],
                                visited: VS[State],
                                stores: StoreMap,
                                kstore: Store[KA, Set[LKont]],
                                graph: G): G = {
      if (todo.isEmpty || timeout.reached) {
        graph
      } else {
//        println(s"I have ${todo.size} states to explore")
        /* Frontier-based semantics */
        val (graph2, successors, stores2, kstore2, shouldClearVisited) =
          todo.foldLeft((graph, Set.empty[State], stores, kstore, false))((acc, state) => {
            val (graph, successors, stores, kstore, shouldClearVisited) = acc
            val (statesWithStores, kstore2)                             = state.step(stores(state), kstore)
            val (stores2, shouldClearVisited2) =
              statesWithStores.foldLeft((stores, shouldClearVisited))((acc, stateWithStore) => {
                val (stores2, shouldClearVisited) = acc._1.add(stateWithStore)
                (stores2, acc._2 || shouldClearVisited)
              })
            val states = statesWithStores.map(_._1)
            ( /* Update the graph */
             graph.addEdges(states.map(state2 => (state, empty, state2))),
             /* Update the next worklist */
             successors ++ states,
             /* Update the store map */
             stores2,
             /* Update the kstore */
             kstore2,
             /* Should the visited set be cleared? */
             shouldClearVisited2)
          })
        if (shouldClearVisited || kstore2 != kstore /* TODO: add timestamping + fastEq that only checks the timestamp */ ) {
          /* Changes in stores/kstore, we have to clear visited set */
          loop(successors, VisitedSet[VS].empty[State], stores2, kstore2, graph2)
        } else {
          loop(successors.filter(s2 => !VisitedSet[VS].contains(visited, s2)),
               VisitedSet[VS].append(visited, todo),
               stores2,
               kstore2,
               graph2)
        }
      }
    }
    val fvs = program.fv
    val initialEnv = Environment.initial[A](sem.initialEnv).restrictTo(fvs)
    val initialStore = Store.initial[A, V](sem.initialStore)
    val state = State.inject(program, initialEnv)
    val kstore = Store.empty[KA, Set[LKont]]
    loop(
      Set(state),
      VisitedSet.MapVisitedSet.empty[State],
      StoreMap(state -> initialStore),
      kstore,
      Graph[G, State, Transition].empty
    )
  }
}
