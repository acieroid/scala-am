/**
 * AAM with a global store.
 */
class AAMGlobalStore[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp](monostore: Boolean)
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "AAM"

  trait KontAddr
  case class NormalKontAddress(exp: Exp, addr: Addr) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  val timestampedStore = true
  val primitives = new Primitives[Addr, Abs]()
  val emptyStore: Store[Addr, Abs] = Store.empty[Addr, Abs]
  val initialStore: Store[Addr, Abs] = Store.initial[Addr, Abs](primitives.forStore)
  val emptyKStore: KontStore[KontAddr] = if (timestampedStore) { TimestampedKontStore[KontAddr](Map(), 0) } else { KontStore.empty[KontAddr] }

  //case class GlobalStore(val store: Store[Addr, Abs], delta: Map[Addr, Abs], timestamp: Int) {
  //  def includeDelta(d: Map[Addr, Abs]): GlobalStore = this.copy(delta = delta |+| d)
  //  def commit = if (delta.isEmpty) { this } else { this.copy(store = store |+| delta, delta = Map(), timestamp = timestamp + 1) }
  //}

  case class State(control: Control, a: KontAddr, t: Time) {
    override def toString = control.toString
    def subsumes(that: State): Boolean = control.subsumes(that.control) && a == that.a && t == that.t

    private def integrate(a: KontAddr, actions: Set[Action[Exp, Abs, Addr]], store: Store[Addr, Abs], kstore: KontStore[KontAddr]): (Set[State], Store[Addr, Abs], KontStore[KontAddr]) =
      actions.foldLeft((Set[State](), emptyStore, kstore))((acc, action) => action match {
        case ActionReachedValue(v, store2, _) => (acc._1 + State(ControlKont(v), a, time.tick(t)), acc._2.join(store2), acc._3)
        case ActionPush(e, frame, env, store2, _) =>
          val next = NormalKontAddress(e, addr.variable("__kont__", abs.bottom, t))
          (acc._1 + State(ControlEval(e, env), next, time.tick(t)), acc._2.join(store2), acc._3.extend(next, Kont(frame, a)))
        case ActionEval(e, env, store2, _) =>
          (acc._1 + State(ControlEval(e, env), a, time.tick(t)), acc._2.join(store2), acc._3)
        case ActionStepIn(fexp, _, e, env, store2, _, _) =>
          (acc._1 + State(ControlEval(e, env), a, time.tick(t)), acc._2.join(store2), acc._3)
        case ActionError(err) =>
          (acc._1 + State(ControlError(err), a, time.tick(t)), acc._2, acc._3)
      })

    /**
     * Computes the set of states that follow the current state, and return the new store as well.
     */
    def step(sem: Semantics[Exp, Abs, Addr, Time], store: Store[Addr, Abs], kstore: KontStore[KontAddr]): (Set[State], Store[Addr, Abs], KontStore[KontAddr]) = control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e, env) => integrate(a, sem.stepEval(e, env, store, t), store, kstore)
      /* In a continuation state, if the value reached is not an error, call the
       * semantic's continuation method */
      case ControlKont(v) if abs.isError(v) => (Set(), store, kstore)
      case ControlKont(v) => kstore.lookup(a).foldLeft((Set[State](), emptyStore, kstore))((acc, kont) => kont match {
        case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, store, t), store, acc._3) match {
          case (states, store2, kstore2) => (acc._1 ++ states, acc._2.join(store2), kstore2)
        }
      })
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => (Set(), store, kstore)
    }
    /**
     * Checks if the current state is a final state. It is the case if it
     * reached the end of the computation, or an error
     */
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress || abs.isError(v)
      case ControlError(_) => true
    }
  }
  object State {
    def inject(exp: Exp): (State, Store[Addr, Abs], KontStore[KontAddr]) =
      (State(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)), HaltKontAddress, time.initial("")),
        initialStore,
        emptyKStore)
  }

  case class AAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[Graph[State, Unit]], timedOut: Boolean)
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString)),
        (s) => if (halted.contains(s)) { Colors.Yellow } else { s.control match {
          case ControlEval(_, _) => Colors.Green
          case ControlKont(_) => Colors.Pink
          case ControlError(_) => Colors.Red
        }}, _ => List())
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  var count = 0
  /* Explore state graph when a monotonically growing store is present */
  @scala.annotation.tailrec
  private def loopMono(todo: Set[State], visited: Set[State], store: Store[Addr, Abs], kstore: KontStore[KontAddr],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, Unit]],
    sem: Semantics[Exp, Abs, Addr, Time]): AAMOutput =
    if (todo.isEmpty || timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      println(s"Done $count iterations")
      AAMOutput(halted, graph.map(g => g.nodes.size).getOrElse(0), (System.nanoTime - startingTime) / Math.pow(10, 9), graph,
        timeout.map(System.nanoTime - startingTime > _).getOrElse(false))
    } else {
      count += 1
      //println(s"Iteration $count, exploring: $todo with kstore $kstore")
      val (edges, store2, kstore2) = todo.foldLeft(Set[(State, State)](), emptyStore, kstore)({ (acc, state) =>
        state.step(sem, store, acc._3) match {
          case (next, store2, kstore2) =>
            (acc._1 ++ next.map(state2 => (state, state2)), acc._2.join(store2), kstore2)
        }
      })
      //println(s"New kstore is $kstore2")
      if (store == store2 && kstore.fastEq(kstore2)) {
        //println("Stores are equal")
        loopMono(edges.map({ case (s1, s2) => s2 }).diff(visited),
          visited ++ todo,
          store2, kstore2,
          halted ++ todo.filter(_.halted),
          startingTime, timeout,
          graph.map(_.addEdges(edges.map({ case (s1, s2) => (s1, (), s2) }))),
          sem)
      } else {
        //println(s"Stores are different: store: ${store == store2}, kstore: ${kstore == kstore2}")
        loopMono(edges.map({ case (s1, s2) => s2 }),
          Set(),
          store2, kstore2,
          halted ++ todo.filter(_.halted),
          startingTime, timeout,
          graph.map(_.addEdges(edges.map({ case (s1, s2) => (s1, (), s2) }))),
          sem)
      }
    }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] = monostore match {
    case true =>
      val (state, store, kstore) = State.inject(exp)
      loopMono(Set(state), Set(), store, kstore, Set(), System.nanoTime, timeout,
        if (graph) { Some(new Graph[State, Unit]()) } else { None },
        sem)
    case false => ???
  }
}
