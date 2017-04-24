/**
 * Implementation of a CESK machine following the AAM approach (Van Horn, David,
 * and Matthew Might. "Abstracting abstract machines." ACM Sigplan
 * Notices. Vol. 45. No. 9. ACM, 2010).
 *
 * A difference with the paper is that we separate the continuation store
 * (KontStore) from the value store (Store). That simplifies the implementation
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
 */
class AAM[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "AAM"

  /**
   * The store used for continuations is a KontStore (defined in
   * Kontinuation.scala). It is parameterized by continuation addresses, that
   * are element of the KontAddress typeclass.
   */
  trait KontAddr
  case class NormalKontAddress(exp: Exp, time: Time) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  case class FunctionKontAddress(fexp: Exp, exp: Exp, time: Time) extends KontAddr {
    override def toString = s"FunctionKontAddress($fexp, $exp)"
  }
  case object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  case class FunctionMarkerFrame(fexp: Exp) extends Frame

  /**
   * A machine state is made of a control component, a value store, a
   * continuation store, and an address representing where the current
   * continuation lives.
   */
  case class State(control: Control, store: Store[Addr, Abs], kstore: KontStore[KontAddr], a: KontAddr, t: Time) {
    override def toString = control.toString

    /**
     * Checks whether a states subsumes another, i.e., if it is "bigger". This
     * is used to perform subsumption checking when exploring the state space,
     * in order to avoid exploring states for which another state that subsumes
     * them has already been explored.
     */
    def subsumes(that: State): Boolean = control.subsumes(that.control) && store.subsumes(that.store) && a == that.a && kstore.subsumes(that.kstore) && t == that.t

    /**
     * Integrates a set of actions (returned by the semantics, see
     * Semantics.scala), in order to generate a set of states that succeeds this
     * one.
     */
    private def integrate(a: KontAddr, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
      actions.flatMap({
        /* When a value is reached, we go to a continuation state */
        case ActionReachedValue(v, store, _) => Set(State(ControlKont(v), store, kstore, a, Timestamp[Time].tick(t)))
        /* When a continuation needs to be pushed, push it in the continuation store */
        case ActionPush(frame, e, env, store, _) => {
          val next = NormalKontAddress(e, t)
          Set(State(ControlEval(e, env), store, kstore.extend(next, Kont(frame, a)), next, Timestamp[Time].tick(t)))
        }
        /* When a value needs to be evaluated, we go to an eval state */
        case ActionEval(e, env, store, _) => Set(State(ControlEval(e, env), store, kstore, a, Timestamp[Time].tick(t)))
        /* When a function is stepped in, we also go to an eval state */
        case ActionStepIn(fexp, _, e, env, store, _, _) => {
          // Set(State(ControlEval(e, env), store, kstore, a, Timestamp[Time].tick(t, fexp)))
          val next = FunctionKontAddress(fexp, e, t)
          val frame = FunctionMarkerFrame(fexp)
          Set(State(ControlCall(fexp, e, env), store, kstore.extend(next, Kont(frame, a)), next, Timestamp[Time].tick(t)))
        }
        /* When an error is reached, we go to an error state */
        case ActionError(err) => Set(State(ControlError(err), store, kstore, a, Timestamp[Time].tick(t)))
      })

    /**
     * Computes the set of states that follow the current state
     */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[State] = control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e, env) => integrate(a, sem.stepEval(e, env, store, t))
      /* In a continuation state, call the semantics' continuation method */
      case ControlKont(v) => kstore.lookup(a).flatMap({
        case Kont(FunctionMarkerFrame(fexp), next) =>
          Set(State(ControlReturn(fexp, v), store, kstore, next, Timestamp[Time].tick(t)))
        case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, store, t))
      })
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => Set()
      case ControlCall(fexp, e, env) =>
        Set(State(ControlEval(e, env), store, kstore, a, Timestamp[Time].tick(t, fexp)))
      case ControlReturn(fexp, v) =>
        if (a == HaltKontAddress) {
          Set(State(ControlKont(v), store, kstore, a, Timestamp[Time].tick(t)))
        } else {
          kstore.lookup(a).flatMap({
            case Kont(FunctionMarkerFrame(fexp), next) =>
              Set(State(ControlReturn(fexp, v), store, kstore, next, Timestamp[Time].tick(t)))
            case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, store, t))
          })
        }
    }

    /**
     * Checks if the current state is a final state. It is the case if it
     * reached the end of the computation, or an error
     */
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress
      case ControlError(_) => true
      case _ => false
    }
  }
  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) =
      State(ControlEval(exp, Environment.initial[Addr](env)),
        Store.initial[Addr, Abs](store), KontStore.empty[KontAddr], HaltKontAddress, Timestamp[Time].initial(""))
    import scala.language.implicitConversions

    implicit val graphNode = new GraphNode[State, Unit] {
      override def label(s: State) = s.toString
      override def color(s: State) = if (s.halted) { Colors.Yellow } else { s.control match {
        case _: ControlEval => Colors.Green
        case _: ControlKont => Colors.Pink
        case _: ControlError => Colors.Red
        case _: ControlCall => Colors.DarkGreen
        case _: ControlReturn => Colors.DarkPink
      }}

      import org.json4s._
      import org.json4s.JsonDSL._
      import org.json4s.jackson.JsonMethods._
      import JSON._
      override def content(s: State) =
        ("control" -> s.control) ~ ("store" -> s.store) ~ ("kstore" -> s.kstore) ~ ("kont" -> s.a.toString) ~ ("time" -> s.t.toString)
    }
  }

  type G = Option[Graph[State, Unit, Unit]]
  case class AAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: G, timedOut: Boolean)
      extends Output {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })

    def toFile(path: String)(output: GraphOutput) = graph match {
      case Some(g) => output.toFile(g, ())(path)
      case None => println("Not generating graph because no graph was computed")
    }
  }

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    @scala.annotation.tailrec
    def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: G): AAMOutput = {
      if (timeout.reached) {
        AAMOutput(halted, visited.size, timeout.time, graph, true)
      } else {
        todo.headOption match {
          case Some(s) =>
            if (visited.contains(s) || visited.exists(s2 => s2.subsumes(s))) {
              /* If we already visited the state, or if it is subsumed by another already
               * visited state, we ignore it. The subsumption part reduces the
               * number of visited states but leads to non-determinism due to the
               * non-determinism of Scala's headOption (it seems so at least). */
              loop(todo.tail, visited, halted, graph)
            } else if (s.halted) {
              /* If the state is a final state, add it to the list of final states and
               * continue exploring the graph */
              loop(todo.tail, visited + s, halted + s, graph)
            } else {
              /* Otherwise, compute the successors of this state, update the graph, and push
               * the new successors on the todo list */
              val succs = s.step(sem)
              val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, (), s2))))
              loop(todo.tail ++ succs, visited + s, halted, newGraph)
            }
          case None => AAMOutput(halted, visited.size, timeout.time, graph, false)
        }
      }
    }
    loop(Set(State.inject(exp, sem.initialEnv, sem.initialStore)), Set(), Set(), if (graph) { Some(Graph.empty) } else { None })
  }
}
