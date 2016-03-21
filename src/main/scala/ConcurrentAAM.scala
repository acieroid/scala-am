import scalaz.Scalaz._
import scalaz._

trait ExplorationType
case object AllInterleavings extends ExplorationType
case object OneInterleaving extends ExplorationType
case object RandomInterleaving extends ExplorationType
case object DPOR extends ExplorationType
case class InterferenceTracking(bound: Option[Int]) extends ExplorationType
object ExplorationTypeParser extends scala.util.parsing.combinator.RegexParsers {
  val all = "AllInterleavings".r ^^ (_ => AllInterleavings)
  val one = "OneInterleaving".r ^^ (_ => OneInterleaving)
  val random = "RandomInterleaving".r ^^ (_ => RandomInterleaving)
  val dpor = "DPOR".r ^^ (_ => DPOR)
  def interference: Parser[ExplorationType] =
    (("InterferenceTracking(" ~> "[0-9]+".r <~ ")") ^^ ((s => InterferenceTracking(Some(s.toInt)))) |
      "InterferenceTracking" ^^ (_ => InterferenceTracking(None)))
  def expl: Parser[ExplorationType] = all | one | random | dpor | interference
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
      case ActionSpawn(tid2: TID, e, ρ, act, effs) => {
        assert(effs.isEmpty) /* TODO */
        integrate1(tid, a, act)(threads.add(tid2, Context(ControlEval(e, ρ), new KontStore[KontAddr](), HaltKontAddress, time.initial(tid2.toString))), oldstore, results)
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
    def hasEnabledTransitions(sem: Semantics[Exp, Abs, Addr, Time]): Boolean = stepAny(sem) match {
      case Some(_) => true
      case None => false
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

  case class EffectsMap(m: Map[State, Map[State, (TID, Effects)]]) {
    def newTransition(graph: Option[Graph[State, (TID, Effects)]], s1: State, s2: State, tid: TID, effects: Effects): (EffectsMap, Boolean) = {
      val ms1 = m(s1)
      /* If s2 is an unencountered state, m(s2) is empty and we just add the new
         information. If s2 was previously encountered, this is a merge point
         (and might be a cycle), then either s1 is not present in m(s2) and can
         be safely added, or s1 is already present in m(s2) and contains the
         same information as the one we get here (because a transition does not
         change) */
      val local = m(s2) + (s1 -> (tid, effects))
      /* if s2 was previously encountered, we have to perform conflict detection due to possible new conflicts */
      /* TODO: Conflict detection will be done at halted states, but what if we have a
       * cycle with no corresponding final state? We need to perform conflict
       * detection at the cycle */
      (EffectsMap(m + (s2 -> local)), m.contains(s2))
    }
    private def containsLoop(s: State, path: List[(State, TID, Effects)]): Option[List[(State, TID, Effects)]] =
      path.indexWhere({ case (s2, _, _) => s2 == s }) match {
        case -1 => None
        case 0 => None
        case n => Some(path.take(n + 1))
      }

    private def reachedBound(path: List[(State, TID, Effects)]): Boolean = exploration match {
      case InterferenceTracking(bound) => bound match {
        case Some(bound) => path.filter({ case (_, _, effs) => !effs.isEmpty }).size >= bound
        case None => false
      }
      case _ => false
    }

    @scala.annotation.tailrec
    private def findPossibleEffects(graph: Option[Graph[State, (TID, Effects)]], todo: Set[(State, List[(State, TID, Effects)])], loops: Map[State, Set[List[(State, TID, Effects)]]], results: Set[List[(State, TID, Effects)]]): Set[List[(State, TID, Effects)]] =
      todo.headOption match {
        case None => {
          /* Done exploring the graph, replace states that contain a loop by the effects on the loop. If more than one possible loops can happen, concatenate them. */
          //println("Done...")
          results.map(seq => seq.flatMap({ case (s, tid, eff) =>
            loops.get(s) match {
              case Some(loops) =>  (s, tid, eff) :: loops.foldLeft(List[(State, TID, Effects)]())(_ ++ _)
              case None => List((s, tid, eff))
            }
          }))
        }
        case Some((s, path)) => {
          //println(s"Find possible effects at state ${id(graph, s)}, ${pathToStr(graph, path)}")
          containsLoop(s, path) match {
            case Some(loop) => findPossibleEffects(graph, todo.tail, loops + (s -> (loops(s) + loop)), results)
            case None =>
              if (reachedBound(path) || m(s).isEmpty) {
                /* reached a start state or reached bound */
                findPossibleEffects(graph, todo.tail, loops, results + path)
              } else {
                findPossibleEffects(graph, todo.tail ++ m(s).toList.map({ case (pred, (tid, effs)) => {
                  if (pred == s) { println("pred == s !!!! ${id(s)}") }
                  (pred, (pred, tid, effs) :: path)
                }}), loops, results)
              }
          }
        }
      }

    private def detectConflicts(graph: Option[Graph[State, (TID, Effects)]], effects: List[(State, TID, Effects)]): Set[(State, TID)] =
      // TODO: first group by target to avoid O(n²) over the entire sequence. Also, skip part of the list?
      effects.zipWithIndex.foldLeft(Set[(State, TID)]())((acc, x) => x match {
        /* for each effect */
        case ((s1, t1, effs1), idx) => effects.drop(idx + 1).foldLeft(acc)((acc, y) => y match {
          /* find an effect from a different state, with a different tid, which is dependent on the first effect */
          case (s2, t2, effs2) if (s1 != s2 && t1 != t2 && dependent(effs1, effs2)) => {
            acc + ((s1, t2))
          }
          case _ => acc
        }
        )})
    private def pathToStr(graph: Option[Graph[State, (TID, Effects)]], path: List[(State, TID, Effects)]): String =
      path.collect({ case (s, tid, effs) if (!effs.isEmpty) => s"${id(graph, s)}" }).mkString(":")

    /* TODO: cache cycles that have been detected */
    def findConflicts(graph: Option[Graph[State, (TID, Effects)]], s: State): Set[(State, TID)] = {
      /* step 1: compute the possible effects that happen to reach this state. It is a
       * set of list of (set of) effects. Each element of the set correspond to
       * a path through the graph. Sets of effects correspond to the multiple
       * effects that are generated by one transition. They are grouped together
       * because effects on a variable made during the same transition are not
       * dependent (they are atomic, e.g. a cas reads and writes to a variable
       * at the same time) */
      //println(s"Computing effects at state ${id(graph, s)}")
      val possibleEffects: Set[List[(State, TID, Effects)]] = findPossibleEffects(graph, Set((s, List[(State, TID, Effects)]())), Map[State, Set[List[(State, TID, Effects)]]]().withDefaultValue(Set[List[(State, TID, Effects)]]()), Set())
      //possibleEffects.foreach(effs => println(pathToStr(graph, effs)))
      /* step 2: for each possible list of effects, find conflicts */
      //println("Detecting conflicts")
      possibleEffects.flatMap(effs => detectConflicts(graph, effs))
    }
  }
  object EffectsMap {
    def apply(): EffectsMap = EffectsMap(Map[State, Map[State, (TID, Effects)]]().withDefaultValue(Map[State, (TID, Effects)]()))
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
          if (false && id(graph, s) > 456) {
            ConcurrentAAMOutput(halted, reallyVisited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
          } else {
          val newThreadPickMap = threadPickMap.explored(s, tid)
          if (visited.contains((s, tid))) {
            //println("Already visited")
            reducedLoop(todo.tail, visited, effectsMap, newThreadPickMap,
              halted, startingTime, timeout, reallyVisited, graph, sem)
          } else if (s.halted) {
            //println("Halted")
            val conflicts = effectsMap.findConflicts(graph, s)
            //println("Computed conflicts")
            reducedLoop(todo.tail ++ conflicts, visited + ((s, tid)), effectsMap, newThreadPickMap,
              halted + s, startingTime, timeout, reallyVisited + s, graph, sem)
          } else {
            //println("New state")
            val succs = s.stepTid(sem, tid)
            val newGraph = graph.map(_.addEdges(succs.map({ case (eff, s2) => (s, (tid, eff), s2) })))
            val (newEffectsMap, detection) = succs.foldLeft((effectsMap, false))((acc, succ) => succ match {
              case (effects, s2) => acc._1.newTransition(newGraph, s, s2, tid, effects) match {
                case (m, detection) => (m, acc._2 && detection)
              }
            })
            //println("Added new transitions")
            val conflicts = if (detection) { halted.flatMap(s => newEffectsMap.findConflicts(graph, s)) } else { Set() }
            //println("Computed conflicts")
            if (succs.isEmpty) {
              /* No successor, even though this is not a halt state: the current thread is blocked.
               All successors states already visited: the current thread is in a loop.
               In both cases, explore a different thread */
              /* In case of loop, some states probably have to be visited again! (if the effectsMap changed) */
              newThreadPickMap.pick(s) match {
                case Some(tid2) =>
                  reducedLoop((((s, tid2)) +: todo.tail) ++ conflicts,
                    if (conflicts.isEmpty) {
                      visited + ((s, tid))
                    } else {
                      Set()
                    }, newEffectsMap, newThreadPickMap,
                    halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
                case None => {
                  /* TODO: deadlocks. Was:
                  val deadlocks: Set[(State, TID)] = newEffectsMap(s).findDeadlocks.flatMap(s => newThreadPickMap.pick(s).map(tid => (s, tid)))
                  reducedLoop(todo.tail ++ deadlocks ++ conflicts,
                    if (conflicts.isEmpty) {
                      visited + ((s, tid))
                    } else {
                      Set()
                    }, newEffectsMap, newThreadPickMap,
                      halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
                   */
                  reducedLoop(todo.tail ++ conflicts, visited + ((s, tid)), newEffectsMap, newThreadPickMap, halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
                }
              }
            } else {
              reducedLoop(succs.map(succ => (succ._2, tid)).toVector ++ todo.tail ++ conflicts, visited + ((s, tid)), newEffectsMap, newThreadPickMap,
                halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
            }
          }
          }
        case None => {
          ConcurrentAAMOutput(halted, reallyVisited.size,
            (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
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
          case Some((p: TID, results: Set[(Effects, State)])) => /* at least one transition enabled */
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
                      case Some(p: TID) =>
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
      case CannotHandle => ConcurrentAAMOutput(halted, visited.size, 0, None, true)
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
      case _: InterferenceTracking => reducedLoop(scala.collection.immutable.Vector((State.inject(exp), thread.initial)), Set(), EffectsMap(), ThreadPickMap(),
        Set(), System.nanoTime, timeout, Set(),
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
      case DPOR => dporExplore(State.inject(exp), System.nanoTime, timeout, if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
    }
}
