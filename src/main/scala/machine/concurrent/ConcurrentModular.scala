import scalaz.Scalaz._
import scalaz._

/* TODO: how to deal with termination and join? Additional store? */
class ConcurrentModular[Exp : Expression, Abs : IsCSchemeLattice, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def name = "ConcurrentModular"

  trait KontAddr
  case class NormalKontAddress(tid: TID, exp: Exp, time: Time) extends KontAddr
  case object HaltKontAddress extends KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  type KStoreDelta = Map[KontAddr, Set[Kont[KontAddr]]]
  object KStoreDelta {
    def empty: KStoreDelta = Map().withDefaultValue(Set.empty)
  }
  type StoreDelta = Map[Addr, Abs]
  object StoreDelta {
    def empty: StoreDelta = Map()
  }
  type ReturnStoreDelta = Map[TID, Abs]
  object ReturnStoreDelta {
    def empty: ReturnStoreDelta = Map()
  }

  type G = Graph[ThreadState, Unit, Unit]
  implicit val graphNode = new GraphNode[ThreadState, Unit] {
    override def labelXml(n: ThreadState) = n.toXml
    override def color(n: ThreadState) = if (n.hasError) {
      Colors.Red
    } else if (n.halted) {
      Colors.Yellow
    } else {
      Colors.White
    }
  }

  object G {
    def apply(): G = Graph.empty[ThreadState, Unit, Unit]
    def apply(s: ThreadState): G = Graph.node[ThreadState, Unit, Unit](s)
  }

  /* Quick trick to use DeltaStore for return addresses */
  implicit val addressTID = new Address[TID] {
    def name = "tid"
    def isPrimitive(x: TID) = false
    def primitive(name: String) = ???
    def variable[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time) = ???
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = ???
    def allocationSite[Exp : Expression](a: TID) = Option.empty
  }

  case class GlobalStore(store: DeltaStore[Addr, Abs], oldStore: DeltaStore[Addr, Abs],
    storeDelta: StoreDelta, mainStoreDelta: StoreDelta,
    kstore: TimestampedKontStore[KontAddr], oldKStore: TimestampedKontStore[KontAddr],
    kstoreDelta: KStoreDelta, mainKStoreDelta: KStoreDelta,
    returnStore: DeltaStore[TID, Abs], oldReturnStore: DeltaStore[TID, Abs],
    returnStoreDelta: ReturnStoreDelta, mainReturnStoreDelta: ReturnStoreDelta) {
    def includeDelta(d: Option[Map[Addr, Abs]]): GlobalStore = d match {
      case Some(d) => this.copy(storeDelta = storeDelta |+| d, mainStoreDelta = mainStoreDelta |+| d)
      case None => throw new Exception("GlobalStore should be used with a store that supports delta!")
    }
    def push(a: KontAddr, kont: Kont[KontAddr]): GlobalStore =
      if (kstore.lookup(a).contains(kont)) { this } else {
        this.copy(kstoreDelta = kstoreDelta + (a -> (kstoreDelta(a) + kont)),
          mainKStoreDelta = mainKStoreDelta + (a -> (mainKStoreDelta(a) + kont)))
      }
    def isUnchanged = storeDelta.isEmpty && kstoreDelta.isEmpty && returnStoreDelta.isEmpty
    def mainIsUnchanged = mainStoreDelta.isEmpty && mainKStoreDelta.isEmpty && mainReturnStoreDelta.isEmpty
    private def addKStoreDelta(kstore: TimestampedKontStore[KontAddr], kstoreDelta: KStoreDelta): TimestampedKontStore[KontAddr] =
      kstoreDelta.foldLeft(kstore)((kstore, toAdd) => toAdd match {
        case (k, vs) => vs.foldLeft(kstore)((kstore, v) => kstore.extend(k, v).asInstanceOf[TimestampedKontStore[KontAddr]])
      })
    def commit = if (isUnchanged) { this } else {
      this.copy(store = store.addDelta(storeDelta), storeDelta = StoreDelta.empty,
        kstore = addKStoreDelta(kstore, kstoreDelta), kstoreDelta = KStoreDelta.empty,
        returnStore = returnStore.addDelta(returnStoreDelta), returnStoreDelta = ReturnStoreDelta.empty)
    }
    def commitMain = if (mainIsUnchanged) { this } else {
      val newStore = oldStore.addDelta(mainStoreDelta)
      val newKStore = addKStoreDelta(oldKStore, mainKStoreDelta)
      val newReturnStore = oldReturnStore.addDelta(mainReturnStoreDelta)
      this.copy(store = newStore, oldStore = newStore, storeDelta = StoreDelta.empty, mainStoreDelta = StoreDelta.empty,
        kstore = newKStore, oldKStore = newKStore, kstoreDelta = KStoreDelta.empty, mainKStoreDelta = KStoreDelta.empty,
        returnStore = newReturnStore, oldReturnStore = newReturnStore, returnStoreDelta = ReturnStoreDelta.empty, mainReturnStoreDelta = ReturnStoreDelta.empty)
    }
    def restore =
      this.copy(store = oldStore, kstore = oldKStore, storeDelta = StoreDelta.empty, kstoreDelta = KStoreDelta.empty)
    def lookupReturnValue(t: TID): Option[Abs] = returnStore.lookup(t)
    def setReturnValue(t: TID, v: Abs): GlobalStore =
      if (returnStore.lookup(t).map(v2 => JoinLattice[Abs].subsumes(v2, v)).getOrElse(false)) { this } else {
        this.copy(returnStoreDelta = returnStoreDelta + (t -> JoinLattice[Abs].join(returnStoreDelta.get(t).getOrElse(JoinLattice[Abs].bottom), v)),
          mainReturnStoreDelta = mainReturnStoreDelta + (t -> JoinLattice[Abs].join(mainReturnStoreDelta.get(t).getOrElse(JoinLattice[Abs].bottom), v)))
      }
  }
  object GlobalStore {
    def initial(storeMappings: Iterable[(Addr, Abs)]): GlobalStore = {
      val store = DeltaStore[Addr, Abs](storeMappings.toMap, Map())
      val kstore = TimestampedKontStore[KontAddr](Map(), 0)
      val emptyReturnStore = DeltaStore[TID, Abs](Map(), Map())
      new GlobalStore(store, store, StoreDelta.empty, StoreDelta.empty, kstore, kstore, KStoreDelta.empty, KStoreDelta.empty,
        emptyReturnStore, emptyReturnStore, ReturnStoreDelta.empty, ReturnStoreDelta.empty)
    }
  }

  object ActionHelpers extends ActorActionHelpers[Exp, Abs, Addr, Time, TID]
  import ActionHelpers._
  type Eff = Effect[Addr]

  trait Control
  case class ControlEval(e: Exp, env: Environment[Addr]) extends Control {
    override def toString = s"ev($e)"
  }
  case class ControlKont(v: Abs) extends Control {
    override def toString = s"ko($v)"
  }
  case class ControlError(err: SemanticError) extends Control {
    override def toString = s"err($err)"
  }

  implicit val stateWithKey = new WithKey[ThreadState] {
    type K = KontAddr
    def key(st: ThreadState) = st.kont
  }
  case class ThreadState(tid: TID, control: Control, kont: KontAddr, t: Time) {
    def toXml: List[scala.xml.Node] = (control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
    })

    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => kont == HaltKontAddress
      case ControlError(_) => true
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }
    def integrate(act: Act, store: GlobalStore):
        (
          /* Successor state */
          Option[ThreadState],
          /* Resulting store */
          GlobalStore,
          /* Created thread */
          Option[ThreadState],
          /* Set of effects generated */
          Set[Eff],
          /* Thread joined if any */
          Option[TID]
        ) = act match {
      case ActionReachedValue(v, store2, effs) =>
        (Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta), Option.empty, effs, Option.empty)
      case ActionPush(frame, e, env, store2, effs) =>
        val next = NormalKontAddress(tid, e, t)
        (Some(this.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta).push(next, Kont(frame, kont)),
          Option.empty, effs, Option.empty)
      case ActionEval(e, env, store2, effs) =>
        (Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta), Option.empty, effs, Option.empty)
      case ActionStepIn(fexp, clo, e, env, store2, argsv, effs) =>
        (Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t, fexp))),
          store.includeDelta(store2.delta), Option.empty, effs, Option.empty)
      case ActionError(err) =>
        (Some(this.copy(control = ControlError(err))), store, Option.empty, Set.empty, Option.empty)
      case ActionSpawn(t2: TID @unchecked, e, env, store2, v, effs) =>
        (Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta), Some(ThreadState(t2, ControlEval(e, env), HaltKontAddress, t)), effs, Option.empty)
      case ActionJoin(tid2: TID @unchecked, store2, effs) =>
        (store.lookupReturnValue(tid2).map(v => this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta), Option.empty, effs, Some(tid2))
    }
    def step(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore):
        (
          /* Successor states */
          Set[ThreadState],
          /* Created threads */
          Set[ThreadState],
          /* Effects */
          Set[Eff],
          /* Threads joined */
          Set[TID],
          /* Resulting store */
          GlobalStore
        ) = {
      val init: (Set[ThreadState], Set[ThreadState], Set[Eff], Set[TID], GlobalStore) = (Set.empty, Set.empty, Set.empty, Set.empty, store)
      control match {
        case ControlEval(e, env) => sem.stepEval(e, env, store.store, t).foldLeft(init)((acc, action) =>
          integrate(action, acc._5) match { case (s, store2, spawned, effs, joined) =>
            (acc._1 ++ s.toSet, acc._2 ++ spawned.toSet, acc._3 ++ effs.toSet, acc._4 ++ joined.toSet, store2)
          })
        case ControlKont(v) if kont != HaltKontAddress =>
          store.kstore.lookup(kont).foldLeft(init)((acc, kont) => kont match {
            case Kont(frame, next) => sem.stepKont(v, frame, acc._5.store, t).foldLeft(acc)((acc, action) =>
              this.copy(kont = next).integrate(action, acc._5) match {
                case (s, store2, spawned, effs, joined) =>
                  (acc._1 ++ s.toSet, acc._2 ++ spawned.toSet, acc._3 ++ effs.toSet, acc._4 ++ joined.toSet, store2)
              })
          })
        case ControlKont(v) if kont == HaltKontAddress =>
          (Set.empty, Set.empty, Set.empty, Set.empty, store.setReturnValue(tid, v))
        case ControlError(_) => init
      }
    }
  }
  case class ThreadModularOutput(time: Double, graphs: Map[TID, Option[G]], timedOut: Boolean) extends Output {
    def numberOfStates = 0
    def finalValues: Set[Abs] = Set()
    def toFile(path: String)(output: GraphOutput) =
      graphs.foreach({
        case (k, Some(g)) => output.toFile(g, ())(path + "-" + k.toString + ".dot")
        case (_, None) => ()
      })
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    type WL[A] = Set[A]
    type VS[A] = KeyMap[A]

    case class InnerLoopState(
      /* Wich tid this corresponds to */
      tid: TID,
      /* What's still to visit */
      todo: WL[ThreadState],
      /* Set of seen states */
      visited: VS[ThreadState],
      /* Set of seen states that cannot be cleared */
      reallyVisited: VS[ThreadState],
      /* The graph computed */
      graph: Option[G],
      /* Threads created */
      created: Set[ThreadState],
      /* Effects */
      effs: Set[Eff],
      /* Threads which are joined */
      joined: Set[TID]
    )
    implicit val innerLoopStateWithKey = new WithKey[InnerLoopState] {
      type K = TID
      def key(st: InnerLoopState) = st.tid
    }
    @scala.annotation.tailrec
    def innerLoop(st: InnerLoopState, store: GlobalStore):
        (
          /* Final state */
          InnerLoopState,
          /* Resulting store */
          GlobalStore
        ) =
      if (st.todo.isEmpty || timeout.reached) {
        (st, store)
      } else {
        val (edges, store2, created, effs, joined) = st.todo.foldLeft((Set[(ThreadState, Unit, ThreadState)](), store, Set[ThreadState](), Set[Eff](), Set[TID]()))((acc, state) =>
          state.step(sem, acc._2) match {
            case (succs, created, effs, joined, store2) =>
              println(s"Explored $state, joined: $joined")
              (acc._1 ++ succs.map(state2 => (state, (), state2)), store2, acc._3 ++ created, acc._4 ++ effs, acc._5 ++ joined)
          })
        val newTodo = edges.map(_._3)
        val newGraph = st.graph.map(_.addEdges(edges))
        if (store2.mainIsUnchanged) {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            reallyVisited = VisitedSet[VS].append(st.reallyVisited, st.todo),
            graph = newGraph,
            effs = st.effs ++ effs,
            created = st.created ++ created,
            joined = st.joined ++ joined
          ), store2)
        } else {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            visited = VisitedSet[VS].empty,
            reallyVisited = VisitedSet[VS].append(st.reallyVisited, st.todo),
            graph = newGraph,
            effs = st.effs ++ effs,
            created = st.created ++ created,
            joined = st.joined ++ joined), store2.commitMain)
        }
      }
    case class OuterLoopState(
      /* What's still to visit */
      todo: WL[InnerLoopState],
      /* Which inner loop states corresponds to which tid */
      tids: Map[TID, Set[InnerLoopState]],
      /* Join dependencies: which threads depends on which other thread's value. t1 -> (t2, t3) mean that t2 and t3 depend on t1's return value */
      joinDeps: Map[TID, Set[TID]],
      /* Global store */
      store: GlobalStore,
      /* Graphs for each thread */
      graphs: Map[TID, Option[G]]
    )

    def fromCreated(created: Set[ThreadState], tids: Map[TID, Set[InnerLoopState]]):
        (Set[InnerLoopState], Map[TID, Set[InnerLoopState]]) =
      created.foldLeft((Set[InnerLoopState](), tids))((acc, st) => {
        if (acc._2(st.tid).exists((inner: InnerLoopState) => inner.todo.contains(st))) {
          /* already spawned this thread, nothing changed */
          acc
        } else {
          val inner = InnerLoopState(st.tid, Set(st), VisitedSet[VS].empty, VisitedSet[VS].empty, Option(G()), Set.empty, Set.empty, Set.empty)
          (acc._1 + inner, acc._2 + (st.tid -> (acc._2(st.tid) + inner)))
        }
      })
    def fromJoined(tid: TID, joined: Set[TID], tids: Map[TID, Set[InnerLoopState]], joinDeps: Map[TID, Set[TID]]):
        Map[TID, Set[TID]] =
      joined.foldLeft(joinDeps)((acc, t) => acc + ((t -> (acc(t) + tid))))
    def  fromReturn(tid: TID, tids: Map[TID, Set[InnerLoopState]], joinDeps: Map[TID, Set[TID]]): Set[InnerLoopState] =
      /* Thread tid returned, need to explore all its dependencies */
      /* TODO: improve this by actually triggering it only if it returned a different value */
      tids(tid)

    /* TODO: from effs */
    /* TODO: from joins/term */
    @scala.annotation.tailrec
    def outerLoop(st: OuterLoopState, iteration: Int): Output = {
      println("---------------")
      println(s"Iteration: $iteration")
      if (st.todo.isEmpty || timeout.reached) {
        println(s"Number of iterations: $iteration")
        new ThreadModularOutput(timeout.time, st.graphs, timeout.reached)
      } else {
        val succ = st.todo.foldLeft((Set[InnerLoopState](), st.tids, st.joinDeps, st.store, st.graphs))((acc, threadState) => {
          println(s"Exploring thread ${threadState.tid}")
          val (ist, store2) = innerLoop(threadState, acc._4)
          val (todoCreated, tidsCreated) = fromCreated(ist.created, acc._2)
          val joinDeps = fromJoined(threadState.tid, ist.joined, acc._2, acc._3)
          val todoJoined = fromReturn(threadState.tid, acc._2, acc._3)
          println(s"Created tids: ${ist.created.map(_.tid)}")
          println(s"Joined on tids: ${ist.joined}")
          println(s"Triggering evaluation of ${todoJoined.map(_.tid)}")
          (acc._1 ++ todoCreated ++ todoJoined, tidsCreated, joinDeps, store2, acc._5 + (ist.tid -> ist.graph))
        })
        val newOuter = OuterLoopState(succ._1, succ._2, succ._3, succ._4, succ._5)
        newOuter.graphs.foreach({
          case (k, Some(g)) => GraphDOTOutput.toFile(g, ())(s"iteration-$iteration-$k.dot")
          case (_, None) => ()
        })
        /*if (newOuter.mailboxes == st.mailboxes && newOuter.tids == st.tids) {
          /* if it didn't change, we skip the todos */
          outerLoop(newOuter.copy(todo = Set.empty), iteration+1)
        } else { */
          outerLoop(newOuter, iteration+1)
/*        }*/
      }
    }
    val mainPid = ThreadIdentifier[TID].initial
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): (ThreadState, GlobalStore) = {
      val initEnv = Environment.initial[Addr](env)
      (ThreadState(mainPid, ControlEval(exp, initEnv), HaltKontAddress, Timestamp[Time].initial("")),
        GlobalStore.initial(store))
    }
    val (initialState, store) = inject(exp, sem.initialEnv, sem.initialStore)
    val initialInner = InnerLoopState(initialState.tid, Set(initialState), VisitedSet[VS].empty, VisitedSet[VS].empty, Option(G()), Set.empty, Set.empty, Set.empty)
    val res = outerLoop(OuterLoopState(Set(initialInner),
      Map[TID, Set[InnerLoopState]]().withDefaultValue(Set.empty) + (mainPid -> Set(initialInner)),
      Map[TID, Set[TID]]().withDefaultValue(Set.empty),
      store,
      Map[TID, Option[G]]().withDefaultValue(Option(G()))), 0)

//    reportRecorded()
    res
  }
}
