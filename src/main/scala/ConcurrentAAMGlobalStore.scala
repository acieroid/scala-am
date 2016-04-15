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

    def stepTid(sem: Semantics[Exp, Abs, Addr, Time], tid: TID, store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(Effects, State)], GlobalStore, KontStore[KontAddr]) = threads.get(tid) match {
      case Some(ctx) => ctx.step(sem, tid, store, kstore, threads, results) match {
        case (res, store2, kstore2) => (res.map({ case (tm, tr, effs) => (effs, State(tm, tr)) }), store2, kstore2)
      }
      case None => (Set(), store, kstore)
    }
    def stepTids(sem: Semantics[Exp, Abs, Addr, Time], tids: Set[TID], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) =
      tids.foldLeft((Set[(TID, Effects, State)](), store, kstore))((acc, tid) =>
        stepTid(sem, tid, acc._2, acc._3) match {
          case (res, store2, kstore2) => (acc._1 ++ res.map(r => (tid, r._1, r._2)), store2, kstore2)
        })
    def stepAll(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, kstore: KontStore[KontAddr]): (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr]) =
      stepTids(sem, threads.tids, store, kstore)
    def stepAnyFrom(sem: Semantics[Exp, Abs, Addr, Time], tids: List[TID], store: GlobalStore, kstore: KontStore[KontAddr]): Option[(TID, Set[(Effects, State)], GlobalStore, KontStore[KontAddr])] = {
      val init: Option[(TID, Set[(Effects, State)], GlobalStore, KontStore[KontAddr])] = None
      tids.foldLeft(init)((acc, tid) => acc match {
        case None => stepTid(sem, tid, store, kstore) match {
          case (stepped, _, _) if stepped.isEmpty => None
          case (stepped, store2, kstore2) => Some((tid, stepped, store2, kstore2))
        }
        case Some(_) => acc
      })
    }
    def stepAny(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, kstore: KontStore[KontAddr]): Option[(TID, Set[(Effects, State)], GlobalStore, KontStore[KontAddr])] =
      stepAnyFrom(sem, threads.tids.toList, store, kstore)
    def stepAnyRandom(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, kstore: KontStore[KontAddr]): Option[(TID, Set[(Effects, State)], GlobalStore, KontStore[KontAddr])] =
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
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString)),
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

    override def inspect(stateNumber: Int, query: String) = ???
  }

  /*
  type Exploration = (State, Set[State]) => Set[(TID, Effects, State)]

  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State], store: Store[Addr, Abs],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]])
    (step: Exploration): ConcurrentAAMOutput =
    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else {
      todo.headOption match {
        case Some(s) =>
          if (visited.contains(s)) {
            loop(todo.tail, visited, store, halted, startingTime, timeout, graph)(step)
          } else if (s.halted) {
            loop(todo.tail, visited + s, store, halted + s, startingTime, timeout, graph)(step)
          } else {
            val result: Either[String, Set[(TID, Effects, State)]] = try {
              Right(step(s, todo))
            } catch {
              case err: Exception =>
                println(s"Caught exception $err")
                err.printStackTrace
                Left(err.toString)
            }
            result match {
              case Left(err) =>
                ConcurrentAAMOutput(halted, visited.size,
                  (System.nanoTime - startingTime) / Math.pow(10, 9), graph.map(_.addEdge(s, (thread.initial, Set()),
                    new State(ThreadMap(Map(thread.initial -> Set(Context(ControlError(err), new KontStore[KontAddr](), HaltKontAddress, time.initial("err"))))),
                      s.results, s.store))), false)
              case Right(succs) =>
                val newGraph = graph.map(_.addEdges(succs.map({ case (tid, eff, s2) => (s, (tid, eff), s2) })))
                loop(todo.tail ++ succs.map(_._3), visited + s, halted, startingTime, timeout, newGraph)(step)
            }
          }
        case None => ConcurrentAAMOutput(halted, visited.size,
          (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
      }
    }

  private def allInterleavings(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, _) => s.stepAll(sem)
  private def oneInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, _) => s.stepAny(sem) match {
    case Some((tid, succs)) => succs.map(s2 => (tid, s2._1, s2._2))
    case None => Set()
  }
  private def randomInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, _) => s.stepAnyRandom(sem) match {
    case Some((tid, succs)) => succs.map(s2 => (tid, s2._1, s2._2))
    case None => Set()
  }

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
    } { case (_, confls) => confls.map({ case (s, _) => id(graph, s)}).mkString(", ") }
    def findDeadlocks(graph: Option[Graph[State, (TID, Effects)]], s: State): (EffectsMap, Set[State]) = {
      val dls: Set[State] = effects(s).collect({
        case (s, tid, effs) if (!deadlocks.contains(s) && effs.exists(eff => eff.isInstanceOf[EffectAcquire[Addr, Abs]])) => s })
      (this.copy(deadlocks = deadlocks ++ dls), dls)
    }
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
      this.copy(
        /* Records effects */
        effects = effs.foldLeft(effects)((acc, eff) => acc + (eff.target -> ((eff.kind, acc(eff.target)) match {
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

            val confls = findPaths(s1, tid1, (reads ++ (writes - ((s1, tid1, effs1)))).collect({ case (s, tid, _) if (tid != tid1) => (s, tid) }))
            //println(s"WW/RW: ${id(graph, s1)}, ${confls.map({ case (s, t) => id(graph, s) })}")
            /* (s, tid) conflicts with every element of confls */
            (acc._1 + ((s1, tid1) -> (acc._1((s1, tid1)) ++ confls)),
              acc._2 ++ confls.map({ case (s2, tid2) => (s1, tid2) }))
        })
        /* Find rw conflicts */
        reads.foldLeft(newAcc)((acc, r) => r match {
          case (s1, tid1, effs1) =>
            val writestr = writes.map({ case (s, tid, eff) => s"${id(graph, s)}, $tid, $eff" })
            //println(s"read: ${id(graph, s1)}, $tid1, $effs1, writes, $writestr")
            val confls = findPaths(s1, tid1, writes.collect({ case (s, tid, _) if (tid != tid1) => (s, tid) }))
            //println(s"WR: ${id(graph, s1)}, ${confls.map({ case (s, t) => id(graph, s) })}")
            (acc._1 + ((s1, tid1) -> (acc._1((s1, tid1)) ++ confls)),
              acc._2 ++ confls.map({ case (s2, tid2) => (s1, tid2) }))
        })
      })
      (this.copy(conflicts = newConflicts), confls)
    } { case (_, confls) => confls.map({ case (s, _) => id(graph, s)}).mkString(", ") }

    def findDeadlocks(graph: Option[Graph[State, (TID, Effects)]], s: State): (EffectsMap, Set[State]) = /* Profiler.log(s"findDeadlocks(${id(graph, s)})") */ {
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
    }
  }

  class ThreadPickMap(m: Map[State, Set[TID]]) {
    /** Pick a new tid to explore. If no more tid can be explored, return None (in
      * which case, either the program is halted, or in a deadlock). */
    def pick(s: State): Option[TID] = {
      m.get(s) match {
        case None => s.threads.tids.headOption
        case Some(tids) => s.threads.tids.filter(tid => !tids.contains(tid)).headOption
      }
    }
    def explored(s: State, tid: TID): ThreadPickMap = new ThreadPickMap(m + (s -> (m.getOrElse(s, Set()) + tid)))
    def print(graph: Option[Graph[State, (TID, Effects)]]): Unit =
      m.foreach({ case (k, v) =>
        println(s"${id(graph, k)}: $v")
      })
  }
  object ThreadPickMap {
    def apply(): ThreadPickMap = new ThreadPickMap(Map[State, Set[TID]]())
  }

  @scala.annotation.tailrec
  private def reducedLoop(todo: scala.collection.immutable.Vector[(State, TID)], visited: Set[(State, TID)], effectsMap: EffectsMap, threadPickMap: ThreadPickMap,
    halted: Set[State], startingTime: Long, timeout: Option[Long], reallyVisited: Set[State], graph: Option[Graph[State, (TID, Effects)]],
    sem: Semantics[Exp, Abs, Addr, Time]): ConcurrentAAMOutput =
    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, reallyVisited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else {
      todo.headOption match {
        case Some((s, tid)) =>
          val newThreadPickMap = threadPickMap.explored(s, tid) /* TODO: record number of transitions explored for s, tid. If this number is 0, and .pick(s) returns None, we know we are in a deadlock, otherwise we just explored all of the enabled transitions */
          if (visited.contains((s, tid))) {
            //println("Already visited")
            reducedLoop(todo.tail, visited, effectsMap, newThreadPickMap,
              halted, startingTime, timeout, reallyVisited, graph, sem)
          } else if (s.halted) {
            // println("Halted")
            val newEffectsMap = effectsMap.newHaltedState(graph, s)
            reducedLoop(todo.tail, visited + ((s, tid)), newEffectsMap, newThreadPickMap,
              halted + s, startingTime, timeout, reallyVisited + s, graph, sem)
          } else {
            // println("New state")
            val succs = s.stepTid(sem, tid)
            val newGraph = graph.map(_.addEdges(succs.map({ case (eff, s2) => (s, (tid, eff), s2) })))
            val newEffectsMap = succs.foldLeft(effectsMap)((em, succ) => succ match {
              case (effects, s2) => em.newTransition(newGraph, s, s2, tid, effects)
            })
            if (succs.isEmpty || succs.forall({ case (_, s2) => visited.exists(_ == (s2, tid)) })) {
              /* No successor, even though this is not a halt state: the current thread is blocked.
               All successors states already visited: the current thread is in a loop.
               In both cases, explore a different thread */
              /* In case of loop, some states probably have to be visited again! (if the effectsMap changed) */
              newThreadPickMap.pick(s) match {
                case Some(tid2) =>
                  reducedLoop((((s, tid2)) +: todo.tail), visited + ((s, tid)), newEffectsMap, newThreadPickMap,
                    halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
                case None => {
                  // println(s"Detecting deadlocks at state ${id(graph, s)}")
                  val (newEffectsMap2, dls) = newEffectsMap.findDeadlocks(graph, s)
                  // println("Done")
                  val deadlocks: Set[(State, TID)] = dls.flatMap(s => newThreadPickMap.pick(s).map(tid => (s, tid)))
                  reducedLoop(todo.tail ++ deadlocks, visited + ((s, tid)), newEffectsMap2, newThreadPickMap,
                    halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
                }
              }
            } else {
              reducedLoop(succs.map(succ => (succ._2, tid)).toVector ++ todo.tail, visited + ((s, tid)), newEffectsMap, newThreadPickMap,
                halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
            }
          }
        case None =>
          val (newEffectsMap, conflicts) = effectsMap.findConflicts(graph)
          if (conflicts.isEmpty) {
            /* exploration finished */
            ConcurrentAAMOutput(halted, reallyVisited.size,
              (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
          } else {
            /* Still have to explore conflicts */
            reducedLoop(conflicts.toVector, visited, newEffectsMap, threadPickMap,
              halted, startingTime, timeout, reallyVisited, graph, sem)
          }
      }
    }


  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] =
    exploration match {
      case AllInterleavings => loop(Set(State.inject(exp)), Set(), Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(allInterleavings(sem))
      case OneInterleaving => loop(Set(State.inject(exp)), Set(), Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(oneInterleaving(sem))
      case RandomInterleaving => loop(Set(State.inject(exp)), Set(), Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(randomInterleaving(sem))
      case InterferenceTrackingSet => reducedLoop(scala.collection.immutable.Vector((State.inject(exp), thread.initial)), Set(), SetEffectsMap(), ThreadPickMap(),
        Set(), System.nanoTime, timeout, Set(),
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
      case _: InterferenceTrackingPath => reducedLoop(scala.collection.immutable.Vector((State.inject(exp), thread.initial)), Set(), PathEffectsMap(), ThreadPickMap(),
        Set(), System.nanoTime, timeout, Set(),
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
      case DPOR => throw new Exception("DPOR does not support global store (use ConcurrentAAM instead)")
    }
 */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] = ???
}
