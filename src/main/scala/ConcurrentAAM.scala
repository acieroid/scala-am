import scalaz.Scalaz._
import scalaz._

trait ExplorationType
case object AllInterleavings extends ExplorationType
case object OneInterleaving extends ExplorationType
case object RandomInterleaving extends ExplorationType
case object DPOR extends ExplorationType
case object InterferenceTrackingSet extends ExplorationType
case class InterferenceTrackingPath(bound: Option[Int]) extends ExplorationType
object ExplorationTypeParser extends scala.util.parsing.combinator.RegexParsers {
  val all = "AllInterleavings".r ^^ (_ => AllInterleavings)
  val one = "OneInterleaving".r ^^ (_ => OneInterleaving)
  val random = "RandomInterleaving".r ^^ (_ => RandomInterleaving)
  val dpor = "DPOR".r ^^ (_ => DPOR)
  val interferenceset = "InterferenceTrackingSet".r ^^ (_ => InterferenceTrackingSet)
  def interferencepath: Parser[ExplorationType] =
    (("InterferenceTrackingPath(" ~> "[0-9]+".r <~ ")") ^^ ((s => InterferenceTrackingPath(Some(s.toInt)))) |
      "InterferenceTrackingPath" ^^ (_ => InterferenceTrackingPath(None)))
  def expl: Parser[ExplorationType] = all | one | random | dpor | interferenceset | interferencepath
  def parse(s: String): ExplorationType = parseAll(expl, s) match {
    case Success(res, _) => res
    case Failure(msg, _) => throw new Exception(s"cannot parse exploration type: $msg")
    case Error(msg, _) => throw new Exception(s"cannot parse exploration type: $msg")
  }
}

class ConcurrentAAM[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier](exploration: ExplorationType)
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def abs = implicitly[AbstractValue[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
  def thread = implicitly[ThreadIdentifier[TID]]

  def name = "ConcurrentAAM"
  val aam = new AAM[Exp, Abs, Addr, Time]
  import aam._

  type KontAddr = aam.KontAddr

  private def effectsToXml(effects: Set[Effect[Addr, Abs]]): List[scala.xml.Node] = effects.toList.map(eff => eff.kind match {
    case ReadEffect => <font color="forestgreen">{eff.toString}</font>
    case WriteEffect => <font color="red2">{eff.toString}</font>
  })

  type Effects = Set[Effect[Addr, Abs]]
  val noEffect: Effects = Set[Effect[Addr, Abs]]()
  def effectsToStr(effs: Effects): String = effs.map(_.toString).mkString(", ")

  case class Context(control: Control, kstore: KontStore[KontAddr], a: KontAddr, t: Time) {
    def integrate1(tid: TID, a: KontAddr, action: Action[Exp, Abs, Addr])(threads: ThreadMap, oldstore: Store[Addr, Abs], results: ThreadResults):
        Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)] = action match {
      case ActionReachedValue(v, σ, effs) => Set((threads.update(tid, Context(ControlKont(v), kstore, a, time.tick(t))), results, σ, effs))
      case ActionPush(e, frame, ρ, σ, effs) => {
        val next = NormalKontAddress(e, addr.variable("__kont__", abs.bottom, t))
        Set((threads.update(tid, Context(ControlEval(e, ρ), kstore.extend(next, Kont(frame, a)), next, time.tick(t))), results, σ, effs))
      }
      case ActionEval(e, ρ, σ, effs) => Set((threads.update(tid, Context(ControlEval(e, ρ), kstore, a, time.tick(t))), results, σ, effs))
      case ActionStepIn(fexp, _, e, ρ, σ, _, effs) => Set((threads.update(tid, Context(ControlEval(e, ρ), kstore, a, time.tick(t, fexp))), results, σ, effs))
      case ActionError(err) => Set((threads.update(tid, Context(ControlError(err), kstore, a, time.tick(t))), results, oldstore, noEffect))
      case ActionSpawn(tid2: TID @unchecked, e, ρ, act, effs) => {
        assert(effs.isEmpty) /* TODO */
        integrate1(tid, a, act)(threads.add(tid2, Context(ControlEval(e, ρ), KontStore.empty[KontAddr], HaltKontAddress, time.initial(tid2.toString))), oldstore, results)
      }
      case ActionJoin(v, σ, effs) => {
        abs.getTids(v).flatMap(tid2 =>
        if (results.isDone(tid2)) {
          Set((threads.update(tid, Context(ControlKont(results.get(tid2)), kstore, a, time.tick(t))), results, σ, effs))
        } else {
          Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)]()
        })
      }
    }

    def integrate(tid: TID, a: KontAddr, actions: Set[Action[Exp, Abs, Addr]], threads: ThreadMap, store: Store[Addr, Abs], results: ThreadResults):
        (Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)]) =
      actions.flatMap(action => integrate1(tid, a, action)(threads, store, results))

    def step(sem: Semantics[Exp, Abs, Addr, Time], tid: TID, store: Store[Addr, Abs], threads: ThreadMap, results: ThreadResults):
        (Set[(ThreadMap, ThreadResults, Store[Addr, Abs], Effects)]) = control match {
      case ControlEval(e, ρ) => integrate(tid, a, sem.stepEval(e, ρ, store, t), threads, store, results)
      case ControlKont(v) if halted && tid != thread.initial =>
        /* TODO: we could avoid distinguishing the initial thread, and just get the
         * final results at its location in results */
        Set((threads.remove(tid), results.add(tid, v), store, noEffect))
      case ControlKont(v) if abs.isError(v) => Set()
      case ControlKont(v) => kstore.lookup(a).flatMap({
        case Kont(frame, next) => integrate(tid, next, sem.stepKont(v, frame, store, t), threads, store, results)
      })
      case ControlError(_) => Set()
    }

    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress || abs.isError(v)
      case ControlError(_) => true
    }
  }

  case class ThreadMap(content: Map[TID, Set[Context]]) {
    def get(tid: TID): Set[Context] = content.getOrElse(tid, Set())
    def tids: Set[TID] = content.keys.toSet
    def update(tid: TID, context: Context): ThreadMap =
      ThreadMap(content + (tid -> Set(context))) /* TODO: abstract thread counting, join */
    def add(tid: TID, context: Context): ThreadMap =
      ThreadMap(content + (tid -> (get(tid) + context)))
    def remove(tid: TID): ThreadMap =
      ThreadMap(content - tid)
    def join(that: ThreadMap): ThreadMap = ThreadMap(this.content |+| that.content) /* TODO: does this correctly joins sets? */
    def forall(f: ((TID, Set[Context])) => Boolean): Boolean = content.forall(f)
  }

  case class ThreadResults(content: Map[TID, Abs]) {
    /* TODO: what if two threads share tid, one is done but not the other? -> use thread counting to know more*/
    def isDone(tid: TID): Boolean = content.contains(tid)
    def get(tid: TID): Abs = content.getOrElse(tid, abs.bottom)
    def add(tid: TID, v: Abs): ThreadResults = ThreadResults(content + (tid -> abs.join(get(tid), v)))
  }

  case class State(threads: ThreadMap, results: ThreadResults, store: Store[Addr, Abs]) {
    def step(sem: Semantics[Exp, Abs, Addr, Time], tid: TID): Set[(Effects, State)] =
      threads.get(tid).flatMap(ctx => ctx.step(sem, tid, store, threads, results).map({
        case (threads, results, store, effects) => (effects, State(threads, results, store))
      }))
    def stepTids(sem: Semantics[Exp, Abs, Addr, Time], tids: Set[TID]): Set[(TID, Effects, State)] =
      tids.foldLeft(Set[(TID, Effects, State)]())((acc, tid) => step(sem, tid).foldLeft(acc)((acc, stepped) => acc + ((tid, stepped._1, stepped._2))))
    def stepTid(sem: Semantics[Exp, Abs, Addr, Time], tid: TID): Set[(Effects, State)] =
      stepTids(sem, Set(tid)).map(x => (x._2, x._3))
    def stepAll(sem: Semantics[Exp, Abs, Addr, Time]): Set[(TID, Effects, State)] =
      stepTids(sem, threads.tids)
    def stepAny(sem: Semantics[Exp, Abs, Addr, Time]): Option[(TID, Set[(Effects, State)])] = {
      val init: Option[(TID, Set[(Effects, State)])] = None
      threads.tids.foldLeft(init)((acc, tid) => acc match {
        case None =>
          val stepped = step(sem, tid)
          if (stepped.isEmpty) {
            None
          } else {
            Some((tid, stepped))
          }
        case Some(_) => acc
      })
    }
    def stepAnyRandom(sem: Semantics[Exp, Abs, Addr, Time]): Option[(TID, Set[(Effects, State)])] = {
      val init: Option[(TID, Set[(Effects, State)])] = None
      scala.util.Random.shuffle(threads.tids.toList).foldLeft(init)((acc, tid) => acc match {
        case None =>
          val stepped = step(sem, tid)
          if (stepped.isEmpty) {
            None
          } else {
            Some((tid, stepped))
          }
        case Some(_) => acc
      })
    }

    def halted: Boolean = threads.tids == Set(thread.initial) && threads.get(thread.initial).forall(_.halted)

    override def toString = threads.tids.map(tid =>
      s"$tid: " + threads.get(tid).map(ctx => ctx.control.toString().take(40)).mkString(", ")
    ).mkString("\n")
  }

  object State {
    def inject(exp: Exp) = {
      val st = new aam.State(exp)
      State(ThreadMap(Map[TID, Set[Context]](thread.initial -> Set(Context(st.control, st.kstore, st.a, st.t)))),
        ThreadResults(Map[TID, Abs]()), st.σ)
    }
  }

  case class ConcurrentAAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[Graph[State, (TID, Effects)]], timedOut: Boolean)
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.threads.get(thread.initial).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString)),
        (s) => if (halted.contains(s)) {
          Colors.Yellow
        } else if (s.threads.content.values.exists(xs => xs.exists(x => x.control.isInstanceOf[ControlError] ))) {
          Colors.Red
        } else {
          Colors.White
        }, {
          case (tid, eff) => scala.xml.Text(tid.toString) :: effectsToXml(eff)
        })
      case None =>
        println("Not generating graph because no graph was computed")
    }

    import scala.util.{Try,Success,Failure}
    override def inspect(stateNumber: Int, query: String) = graph.flatMap(_.getNode(stateNumber)) match {
      case Some(state) => query.split('.') match {
        case Array("store") => println(state.store)
        case Array("hashCode") => println(state.hashCode)
        case Array("equals", s) => Try(s.toInt) match {
          case Success(state2Number) => graph.flatMap(_.getNode(state2Number)) match {
            case Some(state2) => {
              println(s"$stateNumber == $state2Number is ${state == state2}")
              if (state != state2) {
                println(s"$stateNumber.store == $state2Number.store is ${state.store == state2.store}")
                println(s"$stateNumber.threads == $state2Number.threads is ${state.threads == state2.threads}")
                if (state.threads != state2.threads) {
                  println(s"$stateNumber.threads.keys == $state2Number.threads.keys is ${state.threads.content.keys == state2.threads.content.keys}")
                  println(s"$stateNumber.threads.values == $state2Number.threads.values is ${state.threads.content.values == state2.threads.content.values}")
                  println(s"$stateNumber.threads - $state2Number.threads == ${state.threads.content.toSet.diff(state2.threads.content.toSet).toMap}")
                  println(s"$state2Number.threads - $stateNumber.threads == ${state2.threads.content.toSet.diff(state.threads.content.toSet).toMap}")
                  val diff1 = state.threads.content.toSet.diff(state2.threads.content.toSet).toMap
                  val diff2 = state2.threads.content.toSet.diff(state.threads.content.toSet).toMap
                  val ctx1 = diff1.values.head.head
                  val ctx2 = diff2.values.head.head
                  println(s"ctx1 == ctx2 is ${ctx1 == ctx2}")
                  println(s"ctx1.control == ctx2.control is ${ctx1.control == ctx2.control}")
                  println(s"ctx1.kstore == ctx2.kstore is ${ctx1.kstore == ctx2.kstore}")
                  println(s"ctx1.a == ctx2.a is ${ctx1.a == ctx2.a}")
                  println(s"ctx1.t == ctx2.t is ${ctx1.t == ctx2.t}")
                  val c1 = ctx1.control.asInstanceOf[ControlEval]
                  val c2 = ctx2.control.asInstanceOf[ControlEval]
                  println(s"c1.exp == c2.exp is ${c1.exp == c2.exp}")
                  println(s"${c1.exp} -- ${c2.exp}")
                  val e1 = c1.exp.asInstanceOf[ParSimpleThreadCode]
                  val e2 = c2.exp.asInstanceOf[ParSimpleThreadCode]
                  println(s"e1 == e2 is ${e1 == e2}")
                  println(s"e1.pos == e2.pos is ${e1.pos == e2.pos}")
                  println(s"e1.code == e2.code is ${e1.code == e2.code}")
                }
                println(s"$stateNumber.results == $state2Number.results is ${state.results == state2.results}")
              }
            }
            case None => println(s"Graph doesn't contain state ${state2Number}")
          }
          case Failure(e) => println(s"Cannot parse state number ($s): $e")
        }
        case v => println(s"Unknown inspection query on state $stateNumber: $query")
      }
      case None =>
        println(s"Graph was either not generated, or doesn't contain state $stateNumber. I cannot query it.")
    }
  }

  type Exploration = (State, Set[State]) => Set[(TID, Effects, State)]

  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]])
    (step: Exploration): ConcurrentAAMOutput =
    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else {
      todo.headOption match {
        case Some(s) =>
          if (visited.contains(s)) {
            loop(todo.tail, visited, halted, startingTime, timeout, graph)(step)
          } else if (s.halted) {
            loop(todo.tail, visited + s, halted + s, startingTime, timeout, graph)(step)
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
                    new State(ThreadMap(Map(thread.initial -> Set(Context(ControlError(err), KontStore.empty[KontAddr], HaltKontAddress, time.initial("err"))))),
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

  case object CannotHandle extends Exception
  private def dporExplore(s0: State, start: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]], sem: Semantics[Exp, Abs, Addr, Time]) = {
    import scala.util.control.Breaks._
    type Transition = (State, TID, Effects)
    type Stack = (scala.collection.immutable.Vector[Transition], State)
    def last(stack: Stack): State = stack._2
    def dom(stack: Stack): List[Int] = (0 to (stack._1.size-1)).toList
    def proc(stack: Stack, i: Int): TID = stack._1(i)._2
    def pre(stack: Stack, i: Int): State = stack._1(i)._1
    def size(stack: Stack): Int = stack._1.size - 1
    def enabled(state: State): Set[TID] = state.stepAll(sem).map(_._1)
    def append(stack: Stack, s: State, p: TID, effs: Effects, next: State): Stack = (stack._1 :+ (s, p, effs), next)
    val backtrack = scala.collection.mutable.Map[State, Set[TID]]().withDefaultValue(Set[TID]())
    def isDependent(tr: (Effects, State), stack: Stack, i: Int): Boolean = dependent(tr._1, stack._1(i)._3)
    def isDependentAndCoEnabled(tr: (Effects, State), stack: Stack, i: Int) = {
      isDependent(tr, stack, i) && true /* TODO: co-enabledness can be improved */
    }

    type ClockVector = Map[TID, Int]
    def emptyClockVector: ClockVector = Map[TID, Int]().withDefaultValue(0)
    type ClockVectors = Map[Either[TID, Int], ClockVector]
    def emptyClockVectors: ClockVectors = Map[Either[TID, Int], ClockVector]().withDefaultValue(emptyClockVector)
    def maxClockVector(c1: ClockVector, c2: ClockVector): ClockVector = c1.keySet.union(c2.keySet).map(k =>
      (k, scala.math.max(c1(k), c2(k)))).toMap.withDefaultValue(0)
    def happensBefore(clocks: ClockVectors, stack: Stack, i: Int, p: TID): Boolean = i <= clocks(Left(p))(proc(stack, i))
    var halted: Set[State] = Set[State]()
    var timedOut: Boolean = false
    var g: Option[Graph[State, (TID, Effects)]] = graph
    var visited: Set[State] = Set[State]()
    /* DPOR: Explore(S, C) { */
    def explore(stack: Stack, clocks: ClockVectors): Unit = {
      if (timeout.map(System.nanoTime - start > _).getOrElse(false)) {
        timedOut = true
      } else {
        /* DPOR: let s = last(S) */
        val s: State = last(stack)
        /* DPOR: for all precesses p { */
        for (p <- s.threads.tids) {
          /* dpor makes use of next(s, p), it might be empty so we compute next(s, p) first, giving 'succs' */
          val succs = s.stepTid(sem, p)
          succs.size match {
            case 0 => () /* do nothing (no existing i) */
            case 1 => { /* at least one successor */
              val next: (Effects, State) = succs.head /* we only handle one successor, so take it */
              /* DPOR: \exists i = max({i \in dom(S) | S_i is dependent and may be co-enabled with next(s, p) and i \not\happensBefore p}) */
              dom(stack).sortWith(_ > _).find(i => isDependentAndCoEnabled(next, stack, i) && !happensBefore(clocks, stack, i, p)) match {
                case Some(i: Int) => /* found an exesting i */
                  val preSi: State = pre(stack, i) /* computes pre(S,i) in 'preSi' */
                  val enabledTr: Set[TID] = enabled(preSi) /* computes enabled(pre(S,i)) in 'enabledTr' */
                  /* DPOR: if (p \in enabled(pre(S,i))) { */
                  if (enabledTr.contains(p)) {
                    /* DPOR: then add p to backtrack(pre(S,i)); */
                    backtrack += preSi -> (backtrack(preSi) + p)
                  } else {
                    /* DPOR: else add enabled(pre(S,i)) to backtrack(pre(S,i)) */
                    backtrack += preSi -> (backtrack(preSi) ++ enabledTr)
                  }
                case None => () /* do nothing (no existing i) */
              }
            }
            case _ => throw CannotHandle /* more than one successor */
          }
        }
        /* DPOR: if (\exists p \in enabled(s)) */
        /* a transition is enabled if it can perform a step */
        s.stepAny(sem) match {
          case None => halted += s /* no enabled transition, we reached a halted state */
          case Some((p: TID @unchecked, results)) => /* at least one transition enabled */
            if (results.size > 1) {
              throw CannotHandle /* more than one successor */
          } else {
              results.headOption match {
                case Some(_) => {
                  /* DPOR: backtrack(s) := {p}; */
                  backtrack += s -> Set(p)
                  /* DPOR: let done = \emptyset; */
                  var done: Set[TID] = Set[TID]()
                  /* DPOR: while (\exists p \in (backtrack(s) \ done)) { */
                  breakable { while(true) {
                    (backtrack(s) -- done).headOption match {
                      case Some(p: TID @unchecked) =>
                        /* DPOR: add p to done */
                        done = done + p
                        /* DPOR: let t = next(s,p); */
                        s.stepTid(sem, p).headOption match {
                          case Some(t @ (effs: Effects, next: State)) =>
                            visited += next
                            g = g.map(_.addEdge(s, (p, effs), next))
                            /* DPOR: let S' = S.t */
                            /* in our case, a transition is described by the predecessor state (s), the tid
                             * (p), and the effects (effs). 'next' is needed to
                             * compute last(S) */
                            val stack2: Stack = append(stack, s, p, effs, next)
                            /* DPOR: let cv = max{C(i) | i \in 1..|S| and S_i dependent with t}; */
                            val cv: ClockVector = (0 to size(stack)).filter(i => isDependent(t, stack, i)).map(i => clocks(Right(i))).foldLeft(emptyClockVector)((l, r) => maxClockVector(l, r))
                            /* DPOR: let cv2 = cv[p := |S'|]; */
                            val cv2: ClockVector = cv + (p -> size(stack2))
                            /* DPOR: let C' = C[p := cv2, |S'| := cv2]; */
                            val clocks2: ClockVectors = clocks + (Left(p) -> cv2) + (Right(size(stack2)) -> cv2)
                            /* DPOR: Explore(S', C') */
                            explore(stack2, clocks2)
                          case None => halted += s /* no successor */
                        }
                      case None => break /* stops while loop */
                    }
                  }}
                }
                case None => halted += s /* no successor (should not happen) */
              }
            }
        }
      }
    }
    try {
      visited += s0
      /* DPOR: Initially: Explore(\emptyset, \lambda x. \bottom) */
      explore((scala.collection.immutable.Vector[Transition](), s0), emptyClockVectors)
      ConcurrentAAMOutput(halted, visited.size, (System.nanoTime - start) / Math.pow(10, 9), g, timedOut)
    } catch {
      case CannotHandle => ConcurrentAAMOutput(Set(), 0, 0, None, true)
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
      case DPOR => dporExplore(State.inject(exp), System.nanoTime, timeout, if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
    }
}
