import scalaz.Scalaz._
import scalaz._

/* TODO: use Option for returned to distinguish non-termination and bottom */
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
    kstoreDelta: KStoreDelta, mainKStoreDelta: KStoreDelta) {
    def includeDelta(d: Option[Map[Addr, Abs]]): GlobalStore = d match {
      case Some(d) => this.copy(storeDelta = storeDelta |+| d, mainStoreDelta = mainStoreDelta |+| d)
      case None => throw new Exception("GlobalStore should be used with a store that supports delta!")
    }
    def push(a: KontAddr, kont: Kont[KontAddr]): GlobalStore =
      if (kstore.lookup(a).contains(kont)) { this } else {
        this.copy(kstoreDelta = kstoreDelta + (a -> (kstoreDelta(a) + kont)),
          mainKStoreDelta = mainKStoreDelta + (a -> (mainKStoreDelta(a) + kont)))
      }
    def isUnchanged = storeDelta.isEmpty && kstoreDelta.isEmpty
    def mainIsUnchanged = mainStoreDelta.isEmpty && mainKStoreDelta.isEmpty
    private def addKStoreDelta(kstore: TimestampedKontStore[KontAddr], kstoreDelta: KStoreDelta): TimestampedKontStore[KontAddr] =
      kstoreDelta.foldLeft(kstore)((kstore, toAdd) => toAdd match {
        case (k, vs) => vs.foldLeft(kstore)((kstore, v) => kstore.extend(k, v).asInstanceOf[TimestampedKontStore[KontAddr]])
      })
    def commit = if (isUnchanged) { this } else {
      this.copy(store = store.addDelta(storeDelta), storeDelta = StoreDelta.empty,
        kstore = addKStoreDelta(kstore, kstoreDelta), kstoreDelta = KStoreDelta.empty)
    }
    def commitMain = if (mainIsUnchanged) { this } else {
      val newStore = oldStore.addDelta(mainStoreDelta)
      val newKStore = addKStoreDelta(oldKStore, mainKStoreDelta)
      this.copy(store = newStore, oldStore = newStore, storeDelta = StoreDelta.empty, mainStoreDelta = StoreDelta.empty,
        kstore = newKStore, oldKStore = newKStore, kstoreDelta = KStoreDelta.empty, mainKStoreDelta = KStoreDelta.empty)
    }
    def restore =
      this.copy(store = oldStore, kstore = oldKStore, storeDelta = StoreDelta.empty, kstoreDelta = KStoreDelta.empty)
  }
  object GlobalStore {
    def initial(storeMappings: Iterable[(Addr, Abs)]): GlobalStore = {
      val store = DeltaStore[Addr, Abs](storeMappings.toMap, Map())
      val kstore = TimestampedKontStore[KontAddr](Map(), 0)
      new GlobalStore(store, store, StoreDelta.empty, StoreDelta.empty, kstore, kstore, KStoreDelta.empty, KStoreDelta.empty)
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
    def integrate(act: Act, store: GlobalStore, results: Map[TID, Abs]):
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
        (results.get(tid2).map(v => this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta), Option.empty, effs, Some(tid2))
    }
    def step(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, results: Map[TID, Abs]):
        (
          /* Successor states */
          Set[ThreadState],
          /* Created threads */
          Set[ThreadState],
          /* Effects */
          Set[Eff],
          /* Threads joined */
          Set[TID],
          /* Return value */
          Option[Abs],
          /* Resulting store */
          GlobalStore
        ) = {
      val init: (Set[ThreadState], Set[ThreadState], Set[Eff], Set[TID], Option[Abs], GlobalStore) = (Set.empty, Set.empty, Set.empty, Set.empty, Option.empty, store)
      control match {
        case ControlEval(e, env) => sem.stepEval(e, env, store.store, t).foldLeft(init)((acc, action) =>
          integrate(action, acc._6, results) match { case (s, store2, spawned, effs, joined) =>
            (acc._1 ++ s.toSet, acc._2 ++ spawned.toSet, acc._3 ++ effs, acc._4 ++ joined.toSet, acc._5, store2)
          })
        case ControlKont(v) if kont != HaltKontAddress =>
          store.kstore.lookup(kont).foldLeft(init)((acc, kont) => kont match {
            case Kont(frame, next) => sem.stepKont(v, frame, acc._6.store, t).foldLeft(acc)((acc, action) =>
              this.copy(kont = next).integrate(action, acc._6, results) match {
                case (s, store2, spawned, effs, joined) =>
                  (acc._1 ++ s.toSet, acc._2 ++ spawned.toSet, acc._3 ++ effs, acc._4 ++ joined.toSet, acc._5, store2)
              })
          })
        case ControlKont(v) if kont == HaltKontAddress =>
          (Set.empty, Set.empty, Set.empty, Set.empty, Some(v), store)
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
      joined: Set[TID],
      /* Return value */
      returned: Abs
    )
    implicit val innerLoopStateWithKey = new WithKey[InnerLoopState] {
      type K = TID
      def key(st: InnerLoopState) = st.tid
    }
    @scala.annotation.tailrec
    def innerLoop(st: InnerLoopState, store: GlobalStore, returnedValues: Map[TID, Abs]):
        (
          /* Final state */
          InnerLoopState,
          /* Resulting store */
          GlobalStore
        ) =
      if (st.todo.isEmpty || timeout.reached) {
        (st, store)
      } else {
        val (edges, store2, created, effs, joined, returned) = st.todo.foldLeft((Set[(ThreadState, Unit, ThreadState)](), store, Set[ThreadState](), Set[Eff](), Set[TID](), JoinLattice[Abs].bottom))((acc, state) =>
          state.step(sem, acc._2, returnedValues) match {
            case (succs, created, effs, joined, returned, store2) =>
              println(s"Explored $state, joined: $joined, returned $returned")
              (acc._1 ++ succs.map(state2 => (state, (), state2)), store2, acc._3 ++ created, acc._4 ++ effs, acc._5 ++ joined,
              JoinLattice[Abs].join(returned.getOrElse(JoinLattice[Abs].bottom), acc._6))
          })
        val newTodo = edges.map(_._3)
        val newGraph = st.graph.map(_.addEdges(edges))
        if (store2.mainIsUnchanged) {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            visited = VisitedSet[VS].append(st.visited, st.todo),
            reallyVisited = VisitedSet[VS].append(st.reallyVisited, st.todo),
            graph = newGraph,
            effs = st.effs ++ effs,
            created = st.created ++ created,
            joined = st.joined ++ joined,
            returned = JoinLattice[Abs].join(st.returned, returned)
          ), store2, returnedValues)
        } else {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            visited = VisitedSet[VS].empty,
            reallyVisited = VisitedSet[VS].append(st.reallyVisited, st.todo),
            graph = newGraph,
            effs = st.effs ++ effs,
            created = st.created ++ created,
            joined = st.joined ++ joined,
            returned = JoinLattice[Abs].join(st.returned, returned)
          ), store2.commitMain, returnedValues)
        }
      }
    case class OuterLoopState(
      /* What's still to visit */
      todo: WL[InnerLoopState],
      /* Which inner loop states corresponds to which tid */
      tids: Map[TID, Set[InnerLoopState]],
      /* Join dependencies: which threads depends on which other thread's value. t1 -> (t2, t3) mean that t2 and t3 depend on t1's return value */
      joinDeps: Map[TID, Set[TID]],
      /* Read dependencies: which thread reads from which addresses */
      readDeps: Map[TID, Set[Addr]],
      /* Write dependencies: which thread writes to which addresses */
      writeDeps: Map[TID, Set[Addr]],
      /* Returned values of each thread */
      returned: Map[TID, Abs],
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
          val inner = InnerLoopState(st.tid, Set(st), VisitedSet[VS].empty, VisitedSet[VS].empty, Option(G()), Set.empty, Set.empty, Set.empty, JoinLattice[Abs].bottom)
          (acc._1 + inner, acc._2 + (st.tid -> (acc._2(st.tid) + inner)))
        }
      })
    def fromJoined(tid: TID, joined: Set[TID], tids: Map[TID, Set[InnerLoopState]], joinDeps: Map[TID, Set[TID]]):
        Map[TID, Set[TID]] =
      joined.foldLeft(joinDeps)((acc, t) => acc + ((t -> (acc(t) + tid))))
    def  fromReturn(tid: TID, tids: Map[TID, Set[InnerLoopState]], joinDeps: Map[TID, Set[TID]]): Set[InnerLoopState] =
      /* Thread tid returned, need to explore all its dependencies */
      joinDeps(tid).foldLeft(Set[InnerLoopState]())((acc, t) => acc ++ tids(t))
    def fromEffects(tid: TID, effects: Set[Eff], readDeps: Map[TID, Set[Addr]], writeDeps: Map[TID, Set[Addr]], tids: Map[TID, Set[InnerLoopState]],
      oldStore: GlobalStore, newStore: GlobalStore):
        (Map[TID, Set[Addr]], Map[TID, Set[Addr]], Set[InnerLoopState]) = {
      /* TODO: currently limited to effects on variables, but we also have effects on car/cdr, vectors and locks */
      val newReadDeps = readDeps + ((tid -> (readDeps(tid) ++ effects.collect({ case EffectReadVariable(a) => a }))))
      val newWriteDeps = writeDeps + ((tid -> (writeDeps(tid) ++ effects.collect({ case EffectWriteVariable(a) => a }))))
      val rwConflicts = readDeps.keySet.foldLeft(Set[TID]())((acc, tid2) => {
        val intersection = newWriteDeps(tid).intersect(readDeps(tid2))
        /* If there is no conflict, or there is a conflict but the store hasn't been
         * updated, so no new information will be discovered, no need to re-run
         * analysis. Otherwise, add tid to the set of threads to revisit */
        if (tid != tid2 && intersection.exists(a => oldStore.store.lookup(a) != newStore.store.lookup(a))) {
          acc + tid2
        } else { acc }
      })
      val wwConflicts = writeDeps.keySet.foldLeft(Set[TID]())((acc, tid2) => {
        val intersection = newWriteDeps(tid).intersect(writeDeps(tid2))
        if (tid != tid2 && intersection.exists(a => oldStore.store.lookup(a) != newStore.store.lookup(a))) {
          acc + tid2
        } else { acc }
      })
      val toRevisit = (rwConflicts ++ wwConflicts).flatMap(t => tids(t))
      (newReadDeps, newWriteDeps, toRevisit)
    }

    @scala.annotation.tailrec
    def outerLoop(st: OuterLoopState, iteration: Int): Output = {
      println("---------------")
      println(s"Iteration: $iteration")
      if (st.todo.isEmpty || timeout.reached) {
        println(s"Number of iterations: $iteration")
        new ThreadModularOutput(timeout.time, st.graphs, timeout.reached)
      } else {
        val succ = st.todo.foldLeft((Set[InnerLoopState](), st.tids, st.joinDeps, st.readDeps, st.writeDeps, st.returned, st.store, st.graphs))((acc, threadState) => {
          println(s"Exploring thread ${threadState.tid}")
          val (ist, store2) = innerLoop(threadState, acc._7, acc._6)
          val (todoCreated, tidsCreated) = fromCreated(ist.created, acc._2)
          val joinDeps = fromJoined(threadState.tid, ist.joined, acc._2, acc._3)
          val oldReturned = acc._6(threadState.tid)
          val newReturned = JoinLattice[Abs].join(oldReturned, ist.returned)
          val todoJoined = if (oldReturned == newReturned) { Set.empty } else {
            fromReturn(threadState.tid, acc._2, acc._3)
          }
          val (readDeps, writeDeps, todoEffects) = fromEffects(threadState.tid, ist.effs, acc._4, acc._5, acc._2, acc._7, store2)
          println(s"Created tids: ${ist.created.map(_.tid)}")
          println(s"Joined on tids: ${ist.joined}")
          println(s"Join triggering evaluation of ${todoJoined.map(_.tid)}")
          println(s"Read deps of thread: ${readDeps(threadState.tid)}")
          println(s"Write deps of thread: ${writeDeps(threadState.tid)}")
          println(s"Effects triggered evaluation of ${todoEffects.map(_.tid)}")

          (acc._1 ++ todoCreated ++ todoJoined ++ todoEffects, tidsCreated, joinDeps, readDeps, writeDeps, acc._6 + ((threadState.tid -> newReturned)),
            store2, acc._8 + (ist.tid -> ist.graph))
        })
        val newOuter = OuterLoopState(succ._1, succ._2, succ._3, succ._4, succ._5, succ._6, succ._7, succ._8)
        newOuter.graphs.foreach({
          case (k, Some(g)) => GraphDOTOutput.toFile(g, ())(s"iteration-$iteration-$k.dot")
          case (_, None) => ()
        })
        outerLoop(newOuter, iteration+1)
      }
    }
    val bot = JoinLattice[Abs].bottom
    val mainPid = ThreadIdentifier[TID].initial
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): (ThreadState, GlobalStore) = {
      val initEnv = Environment.initial[Addr](env)
      (ThreadState(mainPid, ControlEval(exp, initEnv), HaltKontAddress, Timestamp[Time].initial("")),
        GlobalStore.initial(store))
    }
    val (initialState, store) = inject(exp, sem.initialEnv, sem.initialStore)
    val initialInner = InnerLoopState(initialState.tid, Set(initialState), VisitedSet[VS].empty, VisitedSet[VS].empty, Option(G()), Set.empty, Set.empty, Set.empty, bot)
    val res = outerLoop(OuterLoopState(Set(initialInner),
      Map[TID, Set[InnerLoopState]]().withDefaultValue(Set.empty) + (mainPid -> Set(initialInner)),
      Map[TID, Set[TID]]().withDefaultValue(Set.empty),
      Map[TID, Set[Addr]]().withDefaultValue(Set.empty),
      Map[TID, Set[Addr]]().withDefaultValue(Set.empty),
      Map[TID, Abs]().withDefaultValue(bot),
      store,
      Map[TID, Option[G]]().withDefaultValue(Option(G()))), 0)

//    reportRecorded()
    res
  }
}
