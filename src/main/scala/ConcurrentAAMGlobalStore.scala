import scalaz.Scalaz._
import scalaz._

/**
 * AAM machine for concurrent languages.
 * Assumptions:
 *   - Thread identifiers are maximally precise, i.e., each thread identifier
 *     corresponds to a single thread. This assumption could be easily relaxed,
 *     but it would impact none of the benchmarks.
 *   - The store used is monotonically growing. For that, we need the lattice supports to joins.
 */
class ConcurrentAAMGlobalStore[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier](exploration: ExplorationType)
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def thread = implicitly[ThreadIdentifier[TID]]
  def name = "ConcurrentAAMGlobalStore"

  trait KontAddr
  case class NormalKontAddress(exp: Exp, addr: Addr) extends KontAddr {
    override def toString = s"NormalKontAddr($exp)"
  }
  case object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }
  val primitives = new Primitives[Addr, Abs]()
  val initialStore: GlobalStore = GlobalStore(DeltaStore[Addr, Abs](primitives.forStore.toMap, Map()), Map())
  val emptyKStore: KontStore[KontAddr] = TimestampedKontStore[KontAddr](Map(), 0)

  case class GlobalStore(val store: DeltaStore[Addr, Abs], delta: Map[Addr, Abs]) {
    def includeDelta(d: Option[Map[Addr, Abs]]): GlobalStore = d match {
      case Some(d) => this.copy(delta = delta |+| d)
      case None => throw new Exception("AAMGlobalStore should be used with a store that supports delta!")
    }
    def isUnchanged = delta.isEmpty
    def commit = if (isUnchanged) { this } else { this.copy(store = store.addDelta(delta), delta = Map()) }
  }

  private def effectsToXml(effects: Set[Effect[Addr, Abs]]): List[scala.xml.Node] = effects.toList.map(eff => eff.kind match {
    case ReadEffect => <font color="forestgreen">{eff.toString}</font>
    case WriteEffect => <font color="red2">{eff.toString}</font>
  })

  type Effects = Set[Effect[Addr, Abs]]
  val noEffect: Effects = Set[Effect[Addr, Abs]]()
  def effectsToStr(effs: Effects): String = effs.map(_.toString).mkString(", ")

  case class ThreadResults(content: Map[TID, Abs]) {
    def isDone(tid: TID): Boolean = content.contains(tid)
    def get(tid: TID): Abs = content.getOrElse(tid, abs.bottom)
    def add(tid: TID, v: Abs): ThreadResults = ThreadResults(content + (tid -> abs.join(get(tid), v)))
  }

  case class ThreadMap(content: Map[TID, Context]) {
    def get(tid: TID): Option[Context] = content.get(tid)
    def tids: Set[TID] = content.keys.toSet
    def update(tid: TID, context: Context): ThreadMap =
      ThreadMap(content + (tid -> context))
    def add(tid: TID, context: Context): ThreadMap = {
      assert(content.get(tid) == None)
      ThreadMap(content + (tid -> context))
    }
    def remove(tid: TID): ThreadMap =
      ThreadMap(content - tid)
    def forall(f: ((TID, Context)) => Boolean): Boolean = content.forall(f)
  }

  case class Context(control: Control, a: KontAddr, t: Time) {
    def integrate(tid: TID, a: KontAddr, actions: Set[Action[Exp, Abs, Addr]], threads: ThreadMap, store: GlobalStore, kstore: KontStore[KontAddr], results: ThreadResults): (Set[(ThreadMap, ThreadResults, Effects)], GlobalStore, KontStore[KontAddr]) = {
      def integrate1(acc: (Set[(ThreadMap, ThreadResults, Effects)], GlobalStore, KontStore[KontAddr]))(action: Action[Exp, Abs, Addr], threads: ThreadMap, results: ThreadResults): (Set[(ThreadMap, ThreadResults, Effects)], GlobalStore, KontStore[KontAddr]) = action match {
        case ActionReachedValue(v, store2, effs) => (acc._1 + ((threads.update(tid, Context(ControlKont(v), a, time.tick(t))), results, effs)), acc._2.includeDelta(store2.delta), acc._3)
        case ActionPush(e, frame, env, store2, effs) =>
          val next = NormalKontAddress(e, addr.variable("__kont__", abs.bottom, t))
          (acc._1 + ((threads.update(tid, Context(ControlEval(e, env), next, time.tick(t))), results, effs)), acc._2.includeDelta(store2.delta), acc._3.extend(next, Kont(frame, a)))
        case ActionEval(e, env, store2, effs) => (acc._1 + ((threads.update(tid, Context(ControlEval(e, env), a, time.tick(t))), results, effs)), acc._2.includeDelta(store2.delta), acc._3)
        case ActionStepIn(fexp, _, e, env, store2, _, effs) => (acc._1 + ((threads.update(tid, Context(ControlEval(e, env), a, time.tick(t, fexp))), results, effs)), acc._2.includeDelta(store2.delta), acc._3)
        case ActionError(err) => (acc._1 + ((threads.update(tid, Context(ControlError(err), a, time.tick(t))), results, noEffect)), acc._2, acc._3)
        case ActionSpawn(tid2: TID @unchecked, e, env, act, effs) =>
          integrate1(acc)(act, threads.add(tid2, Context(ControlEval(e, env), HaltKontAddress, time.initial(tid2.toString))), results)
        case ActionJoin(v, store2, effs) =>
          (acc._1 ++ (abs.getTids(v).flatMap(tid2 =>
            if (results.isDone(tid2)) {
              Set((threads.update(tid, Context(ControlKont(results.get(tid2)), a, time.tick(t))), results, effs))
            } else {
              Set[(ThreadMap, ThreadResults, Effects)]()
            })), acc._2.includeDelta(store2.delta), acc._3)
      }
      actions.foldLeft((Set[(ThreadMap, ThreadResults, Effects)](), store, kstore))((acc, a) => integrate1(acc)(a, threads, results))
    }
    def step(sem: Semantics[Exp, Abs, Addr, Time], tid: TID, store: GlobalStore, kstore: KontStore[KontAddr], threads: ThreadMap, results: ThreadResults):
        (Set[(ThreadMap, ThreadResults, Effects)], GlobalStore, KontStore[KontAddr]) = control match {
      case ControlEval(e, env) => integrate(tid, a, sem.stepEval(e, env, store.store, t), threads, store, kstore, results)
      case ControlKont(v) if halted && tid != thread.initial => (Set((threads.remove(tid), results.add(tid, v), noEffect)), store, kstore)
      case ControlKont(v) if abs.isError(v) => (Set(), store, kstore)
      case ControlKont(v) => kstore.lookup(a).foldLeft((Set[(ThreadMap, ThreadResults, Effects)](), store, kstore))((acc, kont) => kont match {
        case Kont(frame, next) => integrate(tid, next, sem.stepKont(v, frame, store.store, t), threads, store, kstore, results) match {
          case (states, store2, kstore2) => (acc._1 ++ states, store2, kstore2)
        }
      })
      case ControlError(_) => (Set(), store, kstore)
    }

    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress || abs.isError(v)
      case ControlError(_) => true
    }
  }

  case class State(threads: ThreadMap, results: ThreadResults) {
    def halted: Boolean = threads.tids == Set(thread.initial) && (threads.get(thread.initial).get.halted)
    override def toString = threads.tids.map(tid =>
      s"$tid: " + (threads.get(tid).get.control.toString())).mkString("\n")

    def stepTid(sem: Semantics[Exp, Abs, Addr, Time], tid: TID, store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) = threads.get(tid) match {
      case Some(ctx) => ctx.step(sem, tid, store, kstore, threads, results) match {
        case (res, store2, kstore2) => (res.map({ case (tm, tr, effs) => (tid, effs, State(tm, tr)) }), store2, kstore2)
      }
      case None => (Set(), store, kstore)
    }
    def stepTids(sem: Semantics[Exp, Abs, Addr, Time], tids: Set[TID], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) =
      tids.foldLeft((Set[(TID, Effects, State)](), store, kstore))((acc, tid) =>
        stepTid(sem, tid, acc._2, acc._3) match {
          case (res, store2, kstore2) => (acc._1 ++ res, store2, kstore2)
        })
    def stepAll(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) =
      stepTids(sem, threads.tids, store, kstore)
    def stepAnyFrom(sem: Semantics[Exp, Abs, Addr, Time], tids: List[TID], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) = {
      val init: Option[(Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr])] = None
      tids.foldLeft(init)((acc, tid) => acc match {
        case None => stepTid(sem, tid, store, kstore) match {
          case (stepped, _, _) if stepped.isEmpty => None
          case res => Some(res)
        }
        case Some(_) => acc
      }) match {
        case Some(v) => v
        case None => (Set(), store, kstore)
      }
    }
    def stepAny(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) =
      stepAnyFrom(sem, threads.tids.toList, store, kstore)
    def stepAnyRandom(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) =
      stepAnyFrom(sem, scala.util.Random.shuffle(threads.tids.toList), store, kstore)
  }

  object State {
    def inject(exp: Exp): (State, GlobalStore, KontStore[KontAddr]) =
      (State(ThreadMap(Map[TID, Context](thread.initial -> Context(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)), HaltKontAddress, time.initial("__main_thread__")))),
        ThreadResults(Map[TID, Abs]())),
        initialStore, emptyKStore)
  }

  case class ConcurrentAAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[Graph[State, (TID, Effects)]], timedOut: Boolean)
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.threads.get(thread.initial).get.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString.take(40))),
        (s) => if (halted.contains(s)) {
          Colors.Yellow
        } else if (s.threads.content.values.exists(_.control.isInstanceOf[ControlError])) {
          Colors.Red
        } else {
          Colors.White
        }, {
          case (tid, eff) => scala.xml.Text(tid.toString) :: effectsToXml(eff)
        })
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  type Exploration = (State, GlobalStore, KontStore[KontAddr]) => (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr])

  @scala.annotation.tailrec
  private def loopOneByOne(todo: Set[State], visited: Set[State], reallyVisited: Set[State], store: GlobalStore, kstore: KontStore[KontAddr],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]])
    (step: Exploration): ConcurrentAAMOutput =
    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, reallyVisited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else {
      todo.headOption match {
        case Some(s) if visited.contains(s) =>
          loopOneByOne(todo.tail, visited, reallyVisited, store, kstore, halted, startingTime, timeout, graph)(step)
        case Some(s) if s.halted =>
          loopOneByOne(todo.tail, visited + s, reallyVisited +s, store, kstore, halted + s, startingTime, timeout, graph)(step)
        case Some(s) => {
          val (succs, store2, kstore2) = step(s, store, kstore)
          val newGraph = graph.map(_.addEdges(succs.map({ case (tid, eff, s2) => (s, (tid, eff), s2) })))
          if (store2.isUnchanged && kstore.fastEq(kstore2)) {
            loopOneByOne(todo ++ succs.map(_._3), visited + s, reallyVisited + s,store2, kstore2, halted, startingTime, timeout, newGraph)(step)
          } else {
            loopOneByOne(todo ++ succs.map(_._3), Set(), reallyVisited + s, store2.commit, kstore2, halted, startingTime, timeout, newGraph)(step)
          }
        }
        case None => ConcurrentAAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
      }
    }

  @scala.annotation.tailrec
  private def loopFrontier(todo: Set[State], visited: Set[State], reallyVisited: Set[State], store: GlobalStore, kstore: KontStore[KontAddr],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]])
    (step: Exploration): ConcurrentAAMOutput = {
    val timedOut = timeout.map(System.nanoTime - startingTime > _).getOrElse(false)
    if (todo.isEmpty || timedOut) {
      ConcurrentAAMOutput(halted, reallyVisited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, timedOut)
    } else {
      val (edges, store2, kstore2) = todo.foldLeft((Set[(State, (TID, Effects), State)](), store, kstore))((acc, s) =>
        step(s, acc._2, acc._3) match {
          case (next, store2, kstore2) =>
            (acc._1 ++ next.map({ case (tid, eff, s2) => (s, (tid, eff), s2) }), store2, kstore2)
        })
      if (store2.isUnchanged && kstore.fastEq(kstore2)) {
        loopFrontier(edges.map(_._3).diff(visited), visited ++ todo, reallyVisited ++ todo, store2, kstore2,
          halted ++ todo.filter(_.halted), startingTime, timeout, graph.map(_.addEdges(edges)))(step)
      } else {
        loopFrontier(edges.map(_._3), Set(), reallyVisited ++ todo, store2.commit, kstore2,
          halted ++ todo.filter(_.halted), startingTime, timeout, graph.map(_.addEdges(edges)))(step)
      }
    }
  }
  private def allInterleavings(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, store, kstore) => s.stepAll(sem, store, kstore)
  private def oneInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, store, kstore) => s.stepAny(sem, store, kstore)
  private def randomInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, store, kstore) => s.stepAnyRandom(sem, store, kstore)

  private def effectsOf(transitions: Set[(Effects, State)]): Effects =
    transitions.flatMap(_._1)
  private def dependent(eff1: Effect[Addr, Abs], eff2: Effect[Addr, Abs]): Boolean =
      (eff1.target == eff2.target && (eff1.kind |+| eff2.kind) == WriteEffect)
  private def dependent(effs1: Effects, effs2: Effects): Boolean =
    (effs1.foldLeft(false)((acc, eff1) => effs2.foldLeft(acc)((acc, eff2) => acc || dependent(eff1, eff2))))

  def id(graph: Option[Graph[State, (TID, Effects)]], s: State): Int = graph match {
    case Some(g) => g.nodeId(s)
    case None => -1
  }

  trait EffectsMap {
    def newTransition(graph: Option[Graph[State, (TID, Effects)]], s1: State, s2: State, tid: TID, effs: Effects): EffectsMap
    def newHaltedState(graph: Option[Graph[State, (TID, Effects)]], s: State): EffectsMap
    def findConflicts(graph: Option[Graph[State, (TID, Effects)]]): (EffectsMap, Set[(State, TID)])
    def findDeadlocks(graph: Option[Graph[State, (TID, Effects)]], s: State): (EffectsMap, Set[State])
  }

  object SetEffectsMap {
    def apply(): EffectsMap =
      SetEffectsMap(Map[State, Set[(State, TID, Effects)]]().withDefaultValue(Set()),
        Map[State, Set[(TID, State)]]().withDefaultValue(Set[(TID, State)]()),
        Set[State](),
        Set[State](),
        Set[(State, TID)](),
        Set[State]()
      )
  }
  case class SetEffectsMap(
    /* For each state, keeps track of the effects that might have happened before the state, grouped by address */
    effects: Map[State, Set[(State, TID, Effects)]],
    /* Keeps track of the transitions explored */
    trans: Map[State, Set[(TID, State)]],
    /* Keeps track of the halted states */
    halted: Set[State],
    /* Keeps track of the cycles */
    cycles: Set[State],
    /* Keeps track of the conflicts already deteted */
    conflicts: Set[(State, TID)],
    /* Keeps track of the deadlocks already detected */
    deadlocks: Set[State]
  ) extends EffectsMap {
    def newTransition(graph: Option[Graph[State, (TID, Effects)]], s1: State, s2: State, tid: TID, effs: Effects): EffectsMap = {
      /* propagate the given set of effects to all successors of s, computing a new effects map */
      def propagate(): (Map[State, Set[(State, TID, Effects)]], Set[State]) = {
        def rec(todo: Set[(State, TID, State)], visited: Set[(State, TID, State)], effects: Map[State, Set[(State, TID, Effects)]], cycles: Set[State]): (Map[State, Set[(State, TID, Effects)]], Set[State]) = todo.headOption match {
          case Some((s1, tid, s2)) if (visited.contains((s1, tid, s2))) =>
            /* detected a cycle, keep track of the cycle for conflict detection*/
            rec(todo.tail, visited, effects, cycles + s2)
          case Some((s1, tid, s2)) => /* effs needs to be propagated from s1 to s2 */
            val newEffs = effects(s2) ++ effects(s1)
            rec(todo.tail ++ trans(s2).map({ case (tid, s3) => (s2, tid, s3) }), visited + ((s1, tid, s2)),
              effects + (s2 -> newEffs),
              cycles)
          case None => /* propagation done */
            (effects, cycles)
        }
        rec(Set((s1, tid, s2)), Set(), effects + (s2 -> (effects(s2) ++ effects(s1) + ((s1, tid, effs)))), cycles)
      }
      val (newEffects, newCycles) = propagate
      this.copy(
        effects = newEffects,
        cycles = newCycles,
        trans = trans + (s1 -> (trans(s1) + ((tid, s2))))
      )
    }
    def newHaltedState(graph: Option[Graph[State, (TID, Effects)]], s: State): EffectsMap = {
      this.copy(halted = halted + s)
    }
    def findConflicts(graph: Option[Graph[State, (TID, Effects)]]): (EffectsMap, Set[(State, TID)]) = Profiler.logRes("findConflicts") {
      def findConflictsAt(s: State): Set[(State, TID)] = {
        effects(s).flatMap({
          case (s1, tid1, effs1) => effects(s).flatMap({
            case (s2, tid2, effs2) if (s1 != s2 && tid1 != tid2 && dependent(effs1, effs2)) =>
              (if (conflicts.contains((s1, tid2))) { Set() } else { Set((s1, tid2)) }) ++ (if (conflicts.contains((s2, tid1))) { Set() } else { Set((s2, tid1)) })
            case _ => Set[(State, TID)]()
          })
        })
      }
      val confls = (halted ++ cycles).flatMap(s => findConflictsAt(s))
      (this.copy(conflicts = conflicts ++ confls), confls)
    } { case (_, confls) => confls.map({ case (s, _) => id(graph, s)}).toList.sorted.mkString(", ") }
    def findDeadlocks(graph: Option[Graph[State, (TID, Effects)]], s: State): (EffectsMap, Set[State]) = {
      val dls: Set[State] = effects(s).collect({
        case (s, tid, effs) if (!deadlocks.contains(s) && effs.exists(eff => eff.isInstanceOf[EffectAcquire[Addr, Abs]])) => s })
      (this.copy(deadlocks = deadlocks ++ dls), dls)
    }
  }

  object NaiveEffectsMap {
    def apply(): EffectsMap =
      NaiveEffectsMap(Map[Addr, Set[(State, TID, Effects)]]().withDefaultValue(Set[(State, TID, Effects)]()),
        Set[(State, TID)](), Set[State](), Set[State]())
  }
  case class NaiveEffectsMap(
    /* Maps an address to the effects that affect it */
    effects: Map[Addr, Set[(State, TID, Effects)]],
    /* Records conflicts that have been already detected to avoid detecting them again */
    conflicts: Set[(State, TID)],
    acquires: Set[State],
    deadlocks: Set[State]
  ) extends EffectsMap {
    def newTransition(graph: Option[Graph[State, (TID, Effects)]], s1: State, s2: State, tid: TID, effs: Effects): EffectsMap = {
      val effs2 = effs.filterNot(x => x.isInstanceOf[EffectAcquire[Addr, Abs]] || x.isInstanceOf[EffectRelease[Addr, Abs]])
      this.copy(
        /* Records effects */
        effects = effs2.foldLeft(effects)((acc, eff) => acc + (eff.target -> (acc(eff.target) + ((s1, tid, effs2))))),
        acquires = if (effs.exists(x => x.isInstanceOf[EffectAcquire[Addr, Abs]])) { acquires + s1 } else { acquires }
      )
    }
    def newHaltedState(graph: Option[Graph[State, (TID, Effects)]], s: State) = this
    def findConflicts(graph: Option[Graph[State, (TID, Effects)]]): (EffectsMap, Set[(State, TID)]) = Profiler.logRes("findConflicts") {
      val confls = effects.keySet.foldLeft(Set[(State, TID)]())((acc, a) =>
        acc ++ (effects(a).flatMap({ case (s1, tid1, effs1) => effects(a).flatMap({
          case (s2, tid2, effs2) if (tid1 != tid2 && s1 != s2 && dependent(effs1, effs2) && !(conflicts.contains((s1, tid2)) && /* TODO: should be && */ conflicts.contains(s2, tid1))) =>
            Set((s1, tid2), (s2, tid1))
          case _ => Set[(State, TID)]()
      })})))
      (this.copy(conflicts = conflicts ++ confls), confls -- conflicts)
    } { case (_, confls) => confls.map({ case (s, _) => id(graph, s)}).toList.sorted.mkString(", ") }

    def findDeadlocks(graph: Option[Graph[State, (TID, Effects)]], s: State): (EffectsMap, Set[State]) =  Profiler.logRes(s"findDeadlocks(${id(graph, s)})") {
      (this.copy(deadlocks = acquires), acquires -- deadlocks)
    } { case (_, dls) => dls.map(x => id(graph, x)).toList.sorted.mkString(", ") }
  }

  object PathEffectsMap {
    def apply(): EffectsMap =
      PathEffectsMap(Map[Addr, (Set[(State, TID, Effect[Addr, Abs])], Set[(State, TID, Effect[Addr, Abs])])]().withDefaultValue((Set[(State, TID, Effect[Addr, Abs])](), Set[(State, TID, Effect[Addr, Abs])]())),
        Map[State, Set[(TID, State)]]().withDefaultValue(Set[(TID, State)]()),
        Map[(State, TID), Set[(State, TID)]]().withDefaultValue(Set[(State, TID)]()),
        Set[(State, TID)](),
        Set[State]()
      )
  }
  case class PathEffectsMap(
    /* Maps an address to the effects that affect it, separated as read effects and write effect */
    effects: Map[Addr, (Set[(State, TID, Effect[Addr, Abs])], Set[(State, TID, Effect[Addr, Abs])])],
    /* Records the transition to be able to compute paths between two transitions. From source state to destinations states */
    trans: Map[State, Set[(TID, State)]],
    /* Records conflicts that have been already detected to avoid detecting them again */
    conflicts: Map[(State, TID), Set[(State, TID)]],
    /* Track states performing an acquire */
    acquires: Set[(State, TID)],
    /* Records deadlocks that have already been detected to avoid detecting them again */
    deadlocks: Set[State]
  ) extends EffectsMap {
    def newTransition(graph: Option[Graph[State, (TID, Effects)]], s1: State, s2: State, tid: TID, effs: Effects): EffectsMap = {
      val effs2 = effs.filterNot(x => x.isInstanceOf[EffectAcquire[Addr, Abs]] || x.isInstanceOf[EffectRelease[Addr, Abs]])
      this.copy(
        /* Records effects */
        effects = effs2.foldLeft(effects)((acc, eff) => acc + (eff.target -> ((eff.kind, acc(eff.target)) match {
          case (WriteEffect, (reads, writes)) => (reads, writes + ((s1, tid, eff)))
          case (ReadEffect, (reads, writes)) => (reads + ((s1, tid, eff)), writes)
        }))),
        /* record transition */
        trans = trans + (s1 -> (trans(s1) + ((tid, s2)))),
        acquires = if (effs.exists(x => x.isInstanceOf[EffectAcquire[Addr, Abs]])) { acquires + ((s1, tid)) } else { acquires }
      )
    }
    def newHaltedState(graph: Option[Graph[State, (TID, Effects)]], s: State) = this /* no need to keep track of halted states */
    def findConflicts(graph: Option[Graph[State, (TID, Effects)]]): (EffectsMap, Set[(State, TID)]) = Profiler.logRes("findConflicts") {
      /* Find elements of dests for which there is a path from s */
      def findPaths(s: State, tid: TID, dests: Set[(State, TID)]): Set[(State, TID)] = {
        //println(s"Finding path between: ${id(graph, s)} and ${dests.map({ case (s, _) => id(graph, s) })}")
        val alreadySeen: Set[(State, TID)] = conflicts((s, tid)) /* conflicts that we already detected and don't need to detect again */
        def recNoBound(todo: Set[(State, TID, State)], visited: Set[(State, TID, State)], results: Set[(State, TID)]): Set[(State, TID)] = todo.headOption match {
          case Some(tr) if (visited.contains(tr)) => /* Already visited this state, discard it and continue exploring */
            //println("Already visited")
            recNoBound(todo.tail, visited, results)
          case Some((s1, tid1, s2)) if (s1 != s && !alreadySeen.contains((s1, tid1)) && dests.contains((s1, tid1))) =>
            //println(s"Conflict detected at ${id(graph, s1)}")
            /* This is a conflict that we haven't seen yet, add it and continue exploring */
            recNoBound(todo.tail ++ trans(s2).map({ case (tid2, s3) => (s2, tid2, s3) }), visited + ((s1, tid1, s2)), results + ((s1, tid1)))
          case Some((s1, tid1, s2)) =>
            //println(s"Continue exploring from ${id(graph, s1)}")
            /* No conflict, continue exploring */
            recNoBound(todo.tail ++ trans(s2).map({ case (tid2, s3) => (s2, tid2, s3) }), visited + ((s1, tid1, s2)), results)
          case None =>
            //println("Done.")
            /* Explored all successor states, return detected conflicts */
            results
        }
        def recBound(bound: Int, todo: Set[(State, TID, State, Int)], visited: Set[(State, TID, State, Int)], results: Set[(State, TID)]): Set[(State, TID)] = todo.headOption match {
          case Some(tr) if (visited.contains(tr)) => /* Already visited this state, discard it and continue exploring */
            //println("Already visited")
            recBound(bound, todo.tail, visited, results)
          case Some((_, _, _, n)) if (n > bound) =>
            /* exceeded bound, discard */
            recBound(bound, todo.tail, visited, results)
          case Some((s1, tid1, s2, n)) if (s1 != s && !alreadySeen.contains((s1, tid1)) && dests.contains((s1, tid1))) =>
            //println(s"Conflict detected at ${id(graph, s1)}")
            /* This is a conflict that we haven't seen yet, add it and continue exploring */
            recBound(bound, todo.tail ++ trans(s2).map({ case (tid2, s3) => (s2, tid2, s3, if (tid1 == tid2) { n } else { n + 1 }) }), visited + ((s1, tid1, s2, n)), results + ((s1, tid1)))
          case Some((s1, tid1, s2, n)) =>
            //println(s"Continue exploring from ${id(graph, s1)}")
            /* No conflict, continue exploring */
            recBound(bound, todo.tail ++ trans(s2).map({ case (tid2, s3) => (s2, tid2, s3, if (tid1 == tid2) { n } else { n + 1 }) }), visited + ((s1, tid1, s2, n)), results)
          case None =>
            //println("Done.")
            /* Explored all successor states, return detected conflicts */
            results
        }
        if (dests.isEmpty) {
          Set()
        } else {
          exploration match {
            case InterferenceTrackingPath(None) =>
              recNoBound(trans(s).collect({ case (tid1, s2) if (tid == tid1) => (s, tid, s2) }).flatMap({ case (s1, tid1, s2) => trans(s2).map({ case (tid2, s3) => (s2, tid2, s3) }) }),
                Set[(State, TID, State)](),
                Set[(State, TID)]())
            case InterferenceTrackingPath(Some(bound)) =>
              recBound(bound, trans(s).collect({ case (tid1, s2) if (tid == tid1) => (s, tid, s2) }).flatMap({ case (s1, tid1, s2) => trans(s2).map({ case (tid2, s3) => (s2, tid2, s3, if (tid1 == tid2) { 0 } else { 1 }) }) }),
                Set[(State, TID, State, Int)](),
                Set[(State, TID)]())
          }
        }
      }
      val (newConflicts, confls) = effects.keySet.foldLeft((conflicts, Set[(State, TID)]()))((acc, a) => /* Profiler.log(s"findConflicts on address $a")*/ {
        val (reads, writes) = effects(a)
        /* Find ww and wr conflicts */
        val newAcc = writes.foldLeft(acc)((acc, w) => w match {
          case (s1, tid1, effs1) =>
            //val readsstr = reads.map({ case (s, tid, eff) => s"${id(graph, s)}, $tid, $eff" })
            //println(s"write: ${id(graph, s1)}, $tid1, $effs1, reads: $readsstr")

            val dests = (reads ++ (writes - ((s1, tid1, effs1)))).collect({ case (s, tid, _) if (tid != tid1) => (s, tid) })
            val confls = if (dests.isEmpty) { Set() } else { findPaths(s1, tid1, dests) }
            //println(s"WW/RW: ${id(graph, s1)}, ${confls.map({ case (s, t) => id(graph, s) })}")
            /* (s, tid) conflicts with every element of confls */
            (acc._1 + ((s1, tid1) -> (acc._1((s1, tid1)) ++ confls)),
              acc._2 ++ confls.map({ case (s2, tid2) => (s1, tid2) }))
        })
        /* Find rw conflicts */
        reads.foldLeft(newAcc)((acc, r) => r match {
          case (s1, tid1, effs1) =>
            //val writestr = writes.map({ case (s, tid, eff) => s"${id(graph, s)}, $tid, $eff" })
            //println(s"read: ${id(graph, s1)}, $tid1, $effs1, writes, $writestr")
            val dests = writes.collect({ case (s, tid, _) if (tid != tid1) => (s, tid) })
            val confls = if (dests.isEmpty) { Set() } else { findPaths(s1, tid1, dests) }
            //println(s"WR: ${id(graph, s1)}, ${confls.map({ case (s, t) => id(graph, s) })}")
            (acc._1 + ((s1, tid1) -> (acc._1((s1, tid1)) ++ confls)),
              acc._2 ++ confls.map({ case (s2, tid2) => (s1, tid2) }))
        })
      })
      (this.copy(conflicts = newConflicts), confls)
    } { case (_, confls) => confls.map({ case (s, _) => id(graph, s)}).toList.sorted.mkString(", ") }

    def findDeadlocks(graph: Option[Graph[State, (TID, Effects)]], s: State): (EffectsMap, Set[State]) = Profiler.logRes(s"findDeadlocks(${id(graph, s)})") {
      def existsPath(source: State, tid: TID, dest: State): Boolean = {
        def recNoBound(todo: Set[State], visited: Set[State]): Boolean = todo.headOption match {
          case Some(s) if (visited.contains(s)) =>
            /* already visited, discard it and continue exploration */
            recNoBound(todo.tail, visited)
          case Some(s) if (s == dest) =>
            /* found path */
            true
          case Some(s) =>
            /* continue exploring with the successors of s */
            recNoBound(todo.tail ++ trans(s).map({ case (tid, s2) => s2 }), visited + s)
          case None =>
            /* explored every successor, no path to dest */
            false
        }
        def recBound(bound: Int, todo: Set[(State, TID, State, Int)], visited: Set[(State, TID, State, Int)]): Boolean = todo.headOption match {
          case Some(tr) if (visited.contains(tr)) => /* Already visited this state, discard it and continue exploring */
            recBound(bound, todo.tail, visited)
          case Some((s1, tid1, s2, n)) if (s1 == dest) =>
            /* found path */
            true
          case Some((_, _, _, n)) if (n > bound) =>
            /* exceeded bound, discard */
            recBound(bound, todo.tail, visited)
          case Some((s1, tid1, s2, n)) =>
            /* Continue exploring */
            recBound(bound, todo.tail ++ trans(s2).map({ case (tid2, s3) => (s2, tid2, s3, if (tid1 == tid2) { n } else { n + 1 }) }), visited + ((s1, tid1, s2, n)))
          case None =>
            /* Explored all successor states, no path to dest */
            false
        }
        exploration match {
          case InterferenceTrackingPath(None) => recNoBound(Set(source), Set[State]())
          case InterferenceTrackingPath(Some(bound)) =>
            recBound(bound, trans(s).collect({ case (tid1, s2) if (tid == tid1) => (s, tid, s2, 0) }), Set[(State, TID, State, Int)]())
        }
      }
      val (newDeadlocks, dls) = acquires.foldLeft((deadlocks, Set[State]()))((acc, x) => x match {
        case (s1, tid) if (!acc._1.contains(s1) && existsPath(s1, tid, s)) => (acc._1 + s1, acc._2 + s1)
        case _ => acc
      })
      (this.copy(deadlocks = newDeadlocks), dls)
    } { case (_, dls) => dls.map(x => id(graph, x)).toList.sorted.mkString(", ") }
  }

  trait PickResult
  case object Deadlock extends PickResult
  case object NoMore extends PickResult
  case class PickThread(t: TID) extends PickResult
  class ThreadPickMap(m: Map[State, (Set[TID], Int)]) {
    /** Transition (s, tid) leads to n new states */
    def explored(s: State, tid: TID, n: Int): ThreadPickMap = new ThreadPickMap(m + (s -> (m(s) match {
      case (tids, n2) => (tids + tid, n + n2)
    })))
    /** Pick a new tid to explore. */
    def pick(s: State): PickResult = m(s) match {
      case (tids, n) => s.threads.tids.filter(tid => !tids.contains(tid)).headOption match {
        case None if (n == 0) => Deadlock
        case None => NoMore
        case Some(tid) => PickThread(tid)
      }
    }
    def print(graph: Option[Graph[State, (TID, Effects)]]): Unit =
      m.foreach({ case (k, v) =>
        println(s"${id(graph, k)}: $v")
      })
  }

  object ThreadPickMap {
    def apply(): ThreadPickMap = new ThreadPickMap(Map[State, (Set[TID], Int)]().withDefaultValue((Set[TID](), 0)))
  }

  @scala.annotation.tailrec
  private def reducedLoopFrontier(todo: Set[(State, TID)], visited: Set[(State, TID)], deadlocks: Set[State], reallyVisited: Set[State], effectsMap: EffectsMap, threadPickMap: ThreadPickMap,
    store: GlobalStore, kstore: KontStore[KontAddr],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]],
    sem: Semantics[Exp, Abs, Addr, Time]): ConcurrentAAMOutput =
    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, reallyVisited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else if (todo.isEmpty) {
      if (deadlocks.isEmpty) {
        val (newEffectsMap, conflicts) = effectsMap.findConflicts(graph)
        if (conflicts.isEmpty) {
          ConcurrentAAMOutput(halted, reallyVisited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
        } else {
          reducedLoopFrontier(conflicts, visited, deadlocks, reallyVisited, newEffectsMap, threadPickMap, store, kstore, halted, startingTime, timeout, graph, sem)
        }
      } else {
        val (newEffectsMap, dls) = deadlocks.foldLeft((effectsMap, Set[State]()))((acc, s) => acc._1.findDeadlocks(graph, s) match {
          case (em, dls) => (em, acc._2 ++ dls)
        })
        val newTodo: Set[(State, TID)] = dls.flatMap(s => threadPickMap.pick(s) match {
          case PickThread(tid) => Set((s, tid))
          case NoMore => Set[(State, TID)]()
          case Deadlock => Set[(State, TID)]() /* Deadlock identification reached another deadlock, that's not possible, except due to imprecision in the deadlock detection */
        })
        reducedLoopFrontier(newTodo, visited, Set(), reallyVisited, newEffectsMap, threadPickMap, store, kstore, halted, startingTime, timeout, graph, sem)
      }
    } else {
      //println(s"todo: ${todo.size} states, visited: ${visited.size} states")
      val (newTodo, newDeadlocks, edges, newHalted, store2, kstore2, newEffectsMap, newThreadPickMap) = todo.foldLeft((Set[(State, TID)](), Set[State](), Set[(State, (TID, Effects), State)](), halted, store, kstore, effectsMap, threadPickMap))((acc, s) => (acc, s) match {
        case ((newTodo, newDeadlocks, edges, newHalted, store, kstore, effectsMap, threadPickMap), (s, tid)) => {
          if (visited.contains((s, tid))) {
            (newTodo, newDeadlocks, edges, newHalted, store, kstore, effectsMap, threadPickMap)
          } else if (s.halted) {
            (newTodo, newDeadlocks, edges, newHalted + s, store, kstore, effectsMap.newHaltedState(graph, s), threadPickMap.explored(s, tid, 0))
          } else {
            s.stepTid(sem, tid, store, kstore) match {
              case (succs, store2, kstore2) => {
                val newThreadPickMap = threadPickMap.explored(s, tid, succs.size)
                val newEdges = edges ++ succs.map({ case (tid, eff, s2) => (s, (tid, eff), s2) })
                val newEffectsMap = succs.foldLeft(effectsMap)((em, succ) => succ match {
                  case (tid, effects, s2) => em.newTransition(None, s, s2, tid, effects)
                })
                if (succs.isEmpty || succs.forall({ case(_, _, s2) => visited.contains((s2, tid)) })) {
                  newThreadPickMap.pick(s) match {
                    case NoMore => (newTodo, newDeadlocks, newEdges, newHalted, store2, kstore2, newEffectsMap, newThreadPickMap)
                    case PickThread(tid2) =>
                      (newTodo + ((s, tid2)), newDeadlocks, newEdges, newHalted, store2, kstore2, newEffectsMap, newThreadPickMap)
                    case Deadlock =>
                      (newTodo, newDeadlocks + s, newEdges, newHalted, store2, kstore2, newEffectsMap, newThreadPickMap)
                  }
                } else {
                  (newTodo ++ succs.map({ case (tid, _, s2) => (s2, tid) }), newDeadlocks, newEdges, newHalted, store2, kstore2,
                    newEffectsMap, newThreadPickMap)
                }
              }
            }
          }
        }
      })
      //println(s"new todo: ${newTodo.size} states")
      val newGraph = graph.map(_.addEdges(edges))
      if (store2.isUnchanged && kstore.fastEq(kstore2)) {
        //println("store unchanged")
        reducedLoopFrontier(newTodo, visited ++ todo, newDeadlocks, reallyVisited ++ todo.map(_._1), newEffectsMap, newThreadPickMap, store2, kstore2, newHalted, startingTime, timeout, newGraph, sem)
      } else {
        //println("store changed")
        reducedLoopFrontier(newTodo, Set(), newDeadlocks, reallyVisited ++ todo.map(_._1), newEffectsMap, newThreadPickMap, store2.commit, kstore2, newHalted, startingTime, timeout, newGraph, sem)
      }
    }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] = {
    def classicalLoop(exp: Exp, exploration: Exploration): Output[Abs] = {
      val (state, store, kstore) = State.inject(exp)
      loopFrontier(Set(state), Set(), Set(), store, kstore, Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(exploration)
    }
    def reducedLoop(exp: Exp, effectsMap: EffectsMap): Output[Abs] = {
      val (state, store, kstore) = State.inject(exp)
      reducedLoopFrontier(Set((state, thread.initial)), Set(), Set(), Set(), effectsMap, ThreadPickMap(), store, kstore, Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
    }
    exploration match {
      case AllInterleavings => classicalLoop(exp, allInterleavings(sem))
      case OneInterleaving => classicalLoop(exp, oneInterleaving(sem))
      case RandomInterleaving => classicalLoop(exp, randomInterleaving(sem))
      case InterferenceTrackingPath(_) => reducedLoop(exp, PathEffectsMap())
      case InterferenceTrackingSet => reducedLoop(exp, SetEffectsMap())
      case InterferenceTrackingNaive => reducedLoop(exp, NaiveEffectsMap())
      case _ => throw new Exception(s"Exploration not yet implemented: $exploration (TODO)")
    }
  }
}
