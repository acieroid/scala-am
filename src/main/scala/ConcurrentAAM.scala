import scalaz.Scalaz._

object ExplorationType extends Enumeration {
  type ExplorationType = Value
  val AllInterleavings, OneInterleaving, RandomInterleaving, InterferenceTracking, DPOR = Value
}
import ExplorationType._

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
    case EffectKind.ReadEffect => <font color="forestgreen">{eff.toString}</font>
    case EffectKind.WriteEffect => <font color="red2">{eff.toString}</font>
  })

  type Effects = Set[Effect[Addr, Abs]]
  val noEffect: Effects = Set[Effect[Addr, Abs]]()

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

    def halted: Boolean = threads.forall({
      case (_, ctxs) => ctxs.forall(_.halted)
    })

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

  type Ample = (Semantics[Exp, Abs, Addr, Time], State, Set[State]) => Set[TID]
  private def partialOrderReduced(sem: Semantics[Exp, Abs, Addr, Time], ample: Ample): Exploration =
    (s, todo) => s.stepTids(sem, ample(sem, s, todo))

  private def noReduction: Ample = (sem, s, _) => s.threads.tids

  private def effectsOf(transitions: Set[(Effects, State)]): Effects =
    transitions.flatMap(_._1)
  private def dependent(eff1: Effects, eff2: Effects) = {
    val groups1 = eff1.groupBy(_.kind).withDefaultValue(Set())
    val groups2 = eff2.groupBy(_.kind).withDefaultValue(Set())
    /* This is our dependency relation. One effect is dependent on another if they
     * act on the same variable and at least one of them is a write. This is
     * extended to sets of effects, and to transitions */
    groups1(EffectKind.WriteEffect).foldLeft(false)((acc, a) =>
      /* Check write-write and read-write dependencies */
      acc || groups2(EffectKind.WriteEffect).contains(a) || groups2(EffectKind.ReadEffect).contains(a)) ||
    groups2(EffectKind.WriteEffect).foldLeft(false)((acc, a) =>
      /* Check write-read dependencies */
      acc || groups1(EffectKind.ReadEffect).contains(a))
  }

  private def checkAmpleConditions(s: State, tid: TID, stepped: Set[(Effects, State)], sem: Semantics[Exp, Abs, Addr, Time], todo: Set[State]): Boolean = {
    /* C0: ample(s) is empty iff enabled(s) is empty: discard empty ample to satisfy this */
    !stepped.isEmpty &&
    /* C1: a transitiion dependent on one in ample(ss cannot be executed without a transition in ample(s) occuring first.
     "Checking this condition is at least as hard as cheacking reachability for the full state space."
     But we can construct ample(s) in a way that C1 satisfied, by taking as candidate the set of transitions Ti(s) of thread i, and checking:
     1) no transition of another process are dependent on these transitions
        Our dependency relation is the following: two transitions are dependent if they share a variable and at least one writes to it.
        Here, we only check between transitions of tid and transitions of tid2 from state s. For correctness, we should check transitions of tid2 from any state, but we can't do that because we don't have this information statically (i.e., we can check for dependency of transitions, but can't generate a set of dependent transitions and then check if this set contains a transition of another thread).
     */
    s.threads.tids.foldLeft(true)((acc, tid2) =>
      acc && (if (tid == tid2) {
        /* don't look at transitions of the same thread */
        true
      } else {
        /* for other threads, if a transition that is dependent on this one is possible, C1 is violated */
        val effects = effectsOf(stepped)
        val effects2 = effectsOf(s.stepTid(sem, tid2))
        // println(s"$s: $effects, $effects2 -> dependent(effects, effects2)")
        !dependent(effects, effects2)
      })) &&
    /*
     2) If there is a transition from another thread that enables a transition
     for tid from a different state, but with the same control component, then
     C1 is violated. TODO: we ignore this condition for now and therefore do
     not support locks (the only transitions that can enable a transition on a
     different thread)
     */
    true &&
    /* C2: this is about invisibility of transitions. In our case we don't have
     * something similar to states labelled with atomic propositions, so we
     * consider all states invisible for now. If we need to verify properties,
     * this can be added, and the condition states that the ample set can't
     * contain a visible transition, i.e., a transition that changes the value of
     * an atomic proposition */
    true &&
    /* C3: a cycle cannot be closed if it contains a state for which a transition is
     * enabled but never included in ample(s) for any s on the cycle. This is
     * avoided by not computing an ample set for states that lead to states that
     * are on the todo list (they will be fully expanded) */
    stepped.foldLeft(true)((acc, s2) => acc && !todo.contains(s2._2))
  }

  /** Implements classical POR reduction, as described in chapter 10 of "Model Checking" (Clarke, Grumberg, Peled) */
  private def classicalReduction: Ample = (sem, s, todo) => s.threads.tids.foldLeft(None: Option[TID])((acc, tid) => acc match {
    case None =>
      val stepped = s.stepTid(sem, tid)
      if (checkAmpleConditions(s, tid, stepped, sem, todo)) {
        /* conditions are satisfied, ample set is found */
        Some(tid)
      } else {
        None
      }
    case Some(_) =>
      /* we already found an ample set */
      acc
  }) match {
    case None => s.threads.tids /* falls back on all interleavings for this state */
    case Some(tid) => Set[TID](tid) /* ample set found */
  }

  trait EffectTarget
  case class Variable(addr: Addr) extends EffectTarget
  case class Vector(addr: Addr) extends EffectTarget
  object EffectTarget {
    def apply(eff: Effect[Addr, Abs]): EffectTarget = eff match {
      case EffectReadVariable(a) => Variable(a)
      case EffectWriteVariable(a) => Variable(a)
      case EffectReadVector(a) => Vector(a)
      case EffectWriteVector(a) => Vector(a)
      case EffectAcquire(a) => Variable(a)
      case EffectRelease(a) => Variable(a)
    }
  }

  case class LocalEffectsMap(val m: Map[EffectTarget, Set[(Effect[Addr, Abs], TID, State)]]) {
    def newTransition(s1: State, s2: State, tid: TID, effect: Effect[Addr, Abs]): LocalEffectsMap = {
      val target: EffectTarget = EffectTarget(effect)
      new LocalEffectsMap(m.get(target) match {
        case None => m + (target -> (Set((effect, tid, s1))))
        case Some(s) => m + (target -> (s + ((effect, tid, s1))))
      })
    }
    def join(that: LocalEffectsMap) = new LocalEffectsMap(m |+| that.m)
    /** Find conflicts in this effect map. Returns the set of states in conflict,
      * along with the tid that needs to be explored */
    def findConflicts(graph: Option[Graph[State, (TID, Effects)]]): Set[(State, TID)] = {
      /* Examples:
       @x -> [(Write, s2, tid2), (Write, s1, tid1)]
       ==> [(s1, tid2), (s2, tid1)]
       @x -> [(Write, s2, tid2), (Read, s1, tid1)]
       ==> [(s1, tid2), (s2, tid1)]
       @x -> [(Read, s2, tid2), (Write, s1, tid1)]
       ==> [(s1, tid2), (s2, tid1)]
       @x -> [(Read, s1, tid1), (Read, s2, tid2)]
       ==> []
       @x -> [(Write, s3, tid3), (Read, s2, tid2), (Read, s1, tid1)]
       ==> [(s1, tid3), (s2, tid3), (s3, tid2), (s3, tid1)]
       @x -> [(Write, s4, tid4), (Write, s3, tid3), (Read, s2, tid2), (Read, s1, tid1)]
       ==> [(s1, tid4), (s2, tid4), (s1, tid3), (s2, tid3), ...]
       @x -> [(Read, s4, tid4), (Read, s3, tid3), (Write, s2, tid2), (Write, s1, tid1)]
       ==> [(s1, tid4), (s1, tid3), (s2, tid4), (s2, tid3), ...]
       */
      /* for every write on s1, tid1, find conflicting effects from state s2 on a
       * different thread tid2, and explore from s1, tid2, and from s2, tid1 */
      m.keySet.flatMap({ case target => m(target) match {
        case effects => effects.flatMap({
          case (effect1, tid1, state1) if (effect1.kind == EffectKind.WriteEffect) =>
            effects.flatMap({
              case (effect2, tid2, state2) if (tid1 != tid2) =>
                if ((effect1.isInstanceOf[EffectAcquire[Addr, Abs]] && effect2.isInstanceOf[EffectRelease[Addr, Abs]]) ||
                  (effect1.isInstanceOf[EffectRelease[Addr, Abs]] && effect2.isInstanceOf[EffectAcquire[Addr, Abs]])) {
                  Set[(State, TID)]()
                } else {
                  println(s"Conflict between ${id(graph, state1)}, $tid1, and ${id(graph, state2)}, $tid2 on target: $target")
                  Set[(State, TID)]((state1, tid2), (state2, tid1))
                }
              case _ => Set[(State, TID)]()
            })
          case _ => Set[(State, TID)]()
        })
      }})
    }
    /** If this is the local effects map of a deadlock state, find states from which
      * another path could avoid the deadlock. Do so by looking at previous
      * states that performed an acquire. TODO: results could be improved by
      * taking into account which lock we are trying to acquire in this state,
      * and only looking at these acquires. But this would require to reason
      * about the semantics in the abstract machine. */
    def findDeadlocks: Set[State] =
      m.values.flatMap({
        case effects => effects.foldRight(Set[State]())((eff, acc) => eff match {
          case (effect, _, state) if (effect.isInstanceOf[EffectAcquire[Addr, Abs]]) =>
            acc + state
          case _ => acc
        })
      }).toSet
    def print(graph: Option[Graph[State, (TID, Effects)]]): Unit = {
      println("====")
      graph match {
        case Some(g) => m.foreach({ case (target, l) =>
          val lstr = l.map({ case (eff, tid, s) => s"${id(graph, s)}: $tid, $eff" })
          println(s"$target: $lstr")
        })
        case None => ()
      }
      println("====")
    }
  }
  object LocalEffectsMap {
    val empty: LocalEffectsMap = new LocalEffectsMap(Map[EffectTarget, Set[(Effect[Addr, Abs], TID, State)]]())
    def apply(): LocalEffectsMap = empty
  }

  case class EffectsMap(m: Map[State, LocalEffectsMap]) {
    /** Called when a transition has been explored, from s1 to s2 by stepping tid,
      * generating a set of effect. Returns the set of conflicts that have to be
      * explored again, as well as the new effects map. */
    def newTransition(graph: Option[Graph[State, (TID, Effects)]], s1: State, s2: State, tid: TID, effects: Set[Effect[Addr, Abs]]): (Set[(State, TID)], EffectsMap) = {
      val ms1 = m.getOrElse(s1, LocalEffectsMap())
      val oldLocal = m.getOrElse(s2, LocalEffectsMap())
      val newLocal = oldLocal.join(effects.foldLeft(ms1)((acc, eff) => acc.newTransition(s1, s2, tid, eff)))
      val newEffectsMap = new EffectsMap(m + (s2 -> newLocal))
      (if (m.contains(s2) && oldLocal != newLocal) newLocal.findConflicts(graph) else Set(), newEffectsMap)
    }
    def apply(s: State): LocalEffectsMap = m(s)
  }
  object EffectsMap {
    def apply(): EffectsMap = new EffectsMap(Map[State, LocalEffectsMap]())
  }

  def id(graph: Option[Graph[State, (TID, Effects)]], s: State): Int = graph match {
    case Some(g) => g.nodeId(s)
    case None => -1
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
          val newThreadPickMap = threadPickMap.explored(s, tid)
          if (visited.contains((s, tid))) {
            reducedLoop(todo.tail, visited, effectsMap, newThreadPickMap,
              halted, startingTime, timeout, reallyVisited, graph, sem)
          } else if (s.halted) {
            val conflicts = effectsMap(s).findConflicts(graph)
            reducedLoop(todo.tail ++ conflicts, visited + ((s, tid)), effectsMap, newThreadPickMap,
              halted + s, startingTime, timeout, reallyVisited + s, graph, sem)
          } else {
            val succs = s.stepTid(sem, tid)
            val newGraph = graph.map(_.addEdges(succs.map({ case (eff, s2) => (s, (tid, eff), s2) })))
            val (newEffectsMap, conflicts) = succs.foldLeft((effectsMap, Set[(State, TID)]()))((acc, succ) => succ match {
              case (effects, s2) => acc._1.newTransition(newGraph, s, s2, tid, effects) match {
                case (conflicts, m) => (m, acc._2 ++ conflicts)
              }
            })
            if (succs.isEmpty /* || succs.forall({ case (_, s2) => visited.exists(_ == (s2, tid)) }) */) {
              /* No successor, even though this is not a halt state: the current thread is blocked.
               All successors states already visited: the current thread is in a loop.
               In both cases, explore a different thread */
              /* In case of loop, some states probably have to be visited again! (if the effectsMap changed) */
              // require(conflicts.isEmpty)
              newThreadPickMap.pick(s) match {
                case Some(tid2) =>
                  reducedLoop(((s, tid2)) +: todo.tail, visited + ((s, tid)), newEffectsMap, newThreadPickMap,
                    halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
                case None => {
                  val deadlocks: Set[(State, TID)] = newEffectsMap(s).findDeadlocks.flatMap(s => newThreadPickMap.pick(s).map(tid => (s, tid)))
                  reducedLoop(todo.tail ++ deadlocks, visited + ((s, tid)), newEffectsMap, newThreadPickMap,
                    halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
                }
              }
            } else {
              reducedLoop(succs.map(succ => (succ._2, tid)).toVector ++ todo.tail ++ conflicts,
                if (conflicts.isEmpty) {
                  visited + ((s, tid))
                } else {
                  Set()
                }, newEffectsMap, newThreadPickMap,
                halted, startingTime, timeout, reallyVisited + s, newGraph, sem)
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
    def explore(stack: Stack, clocks: ClockVectors): Unit = {
      if (timeout.map(System.nanoTime - start > _).getOrElse(false)) {
        timedOut = true
      } else {
        val s: State = last(stack)
        for (p <- s.threads.tids) {
          val succs = s.stepTid(sem, p)
          succs.size match {
            case 0 => () /* do nothing (no existing i) */
            case 1 => {
              val next: (Effects, State) = succs.head
              dom(stack).sortWith(_ > _).find(i => isDependentAndCoEnabled(next, stack, i) && !happensBefore(clocks, stack, i, p)) match {
                case Some(i: Int) =>
                  val preSi: State = pre(stack, i)
                  val enabledTr: Set[TID] = enabled(preSi)
                  if (enabledTr.contains(p)) {
                    backtrack += preSi -> (backtrack(preSi) + p)
                  } else {
                    backtrack += preSi -> (backtrack(preSi) ++ enabledTr)
                  }
                  /*val e: Set[TID] = enabledTr.filter(q => q == p || dom(stack).contains((j: Int) => j > i && q == proc(stack, j) && happensBefore(stack, j, p)))
                  if (!e.isEmpty) {
                    backtrack += preSi -> (backtrack(preSi) + e.head)
                  } else {
                    backtrack += preSi -> (backtrack(preSi) ++ enabledTr)
                  }*/
                case None => () /* do nothing (no existing i) */
              }
            }
            case _ => throw CannotHandle /* more than one successor */
          }
        }
        s.stepAny(sem) match {
          case None => halted += s /* no enabled transition */
          case Some((p: TID, results: Set[(Effects, State)])) =>
            if (results.size > 1) {
              throw CannotHandle /* more than one successor */
          } else {
              results.headOption match {
                case Some(_) =>
                backtrack += s -> (backtrack(s) + p)
                  var done: Set[TID] = Set[TID]()
                  breakable { while(true) {
                    (backtrack(s) -- done).headOption match {
                      case Some(p: TID) =>
                        done = done + p
                        s.stepTid(sem, p).headOption match {
                          case Some(t @ (effs: Effects, next: State)) =>
                            visited += next
                            g = g.map(_.addEdge(s, (p, effs), next))
                            val stack2: Stack = append(stack, s, p, effs, next)
                            val cv: ClockVector = (0 to (stack._1.size - 1)).filter(i => isDependent(t, stack, i)).map(i => clocks(Right(i))).foldLeft(emptyClockVector)((l, r) => maxClockVector(l, r))
                            val cv2: ClockVector = cv + (p -> (stack2._1.size - 1))
                            val clocks2: ClockVectors = clocks + (Left(p) -> cv2) + (Right((stack2._1.size - 1)) -> cv2)
                            explore(stack2, clocks2)
                          case None => halted += s /* no successor */
                        }
                      case None => break
                    }
                  }}
                case None => halted += s /* no successor */
              }
            }
        }
      }
    }
    try {
      visited += s0
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
      case InterferenceTracking => reducedLoop(scala.collection.immutable.Vector((State.inject(exp), thread.initial)), Set(), EffectsMap(), ThreadPickMap(),
        Set(), System.nanoTime, timeout, Set(),
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
      case DPOR => dporExplore(State.inject(exp), System.nanoTime, timeout, if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
    }
}
