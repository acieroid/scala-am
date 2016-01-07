import scalaz.Scalaz._


object ExplorationType extends Enumeration {
  type ExplorationType = Value
  val AllInterleavings, OneInterleaving, InterferenceTracking = Value
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
        val next = NormalKontAddress(e, addr.variable("__kont__", t))
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
    def hasEnabledTransitions(sem: Semantics[Exp, Abs, Addr, Time]): Boolean = stepAny(sem) match {
      case Some(_) => true
      case None => false
    }

    def halted: Boolean = threads.forall({
      case (_, ctxs) => ctxs.forall(_.halted)
    })

    /* TODO: have a different type for HTML-like strings */
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

  type Ample = (Semantics[Exp, Abs, Addr, Time], State, Set[State]) => Set[TID]
  private def partialOrderReduced(sem: Semantics[Exp, Abs, Addr, Time], ample: Ample): Exploration =
    (s, todo) => s.stepTids(sem, ample(sem, s, todo))

  private def noReduction: Ample = (sem, s, _) => s.threads.tids

  private def effectsOf(transitions: Set[(Effects, State)]): Effects =
    transitions.flatMap(_._1)
  private def dependent(eff1: Effects, eff2: Effects) = {
    val groups1 = eff1.groupBy(_.kind)
    val groups2 = eff2.groupBy(_.kind)
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
  case class Vector(addr: Addr, index: Abs) extends EffectTarget
  object EffectTarget {
    def apply(eff: Effect[Addr, Abs]): EffectTarget = eff match {
      case EffectReadVariable(a) => Variable(a)
      case EffectWriteVariable(a) => Variable(a)
      case EffectReadVector(a, idx) => Vector(a, idx)
      case EffectWriteVector(a, idx) => Vector(a, idx)
      /* TODO: handling locks should be done differently */
      case EffectAcquire(a) => Variable(a)
      case EffectRelease(a) => Variable(a)
    }
  }

  /* TODO: what happens when more than one effect is generated by transition? */
  /* Why a list? Because we want to know which effect appeared first, to reexplore
   * from the first state in the conflict */
  class LocalEffectsMap(m: Map[EffectTarget, List[(Effect[Addr, Abs], TID, State)]]) {
    def newTransition(s1: State, s2: State, tid: TID, effect: Effect[Addr, Abs]): LocalEffectsMap = {
      val target: EffectTarget = EffectTarget(effect)
      new LocalEffectsMap(m.get(target) match {
        case None => m + (target -> List((effect, tid, s1)))
        case Some(l) => m + (target -> ((effect, tid, s1) :: l))
      })
    }
    /** Find conflicts in this effect map. Returns the set of states in conflict,
      * along with the tid that needs to be explored */
    def findConflicts: Set[(State, TID)] = {
      /* Examples:
       @x -> [(Write, s2, tid2), (Write, s1, tid1)]
       ==> [(s1, tid2)]
       @x -> [(Write, s2, tid2), (Read, s1, tid1)]
       ==> [(s1, tid2)]
       @x -> [(Read, s2, tid2), (Write, s1, tid1)]
       ==> [(s1, tid2)]
       @x -> [(Read, s1, tid1), (Read, s2, tid2)]
       ==> []
       @x -> [(Write, s3, tid3), (Read, s2, tid2), (Read, s1, tid1)]
       ==> [(s1, tid3), (s2, tid3)]
       @x -> [(Write, s4, tid4), (Write, s3, tid3), (Read, s2, tid2), (Read, s1, tid1)]
       ==> [(s1, tid4), (s2, tid4), (s1, tid3), (s2, tid3)]
       @x -> [(Read, s4, tid4), (Read, s3, tid3), (Write, s2, tid2), (Write, s1, tid1)]
       ==> [(s1, tid4), (s1, tid3), (s2, tid4), (s2, tid3)]
       */
      /* for every write on s1, find subsequent effects on a different thread tid2, and explore from s1, tid2 */
      m.values.flatMap({
        case effects => effects.zipWithIndex.foldRight(Set[(State, TID)]())((eff, acc: Set[(State, TID)]) => eff match {
          case ((effect1, tid1, state1), idx) if (effect1.kind == EffectKind.WriteEffect) =>
            effects.splitAt(idx)._1.foldLeft(acc)((acc: Set[(State, TID)], eff) => eff match {
              case (effect2, tid2, state2) if (tid1 != tid2) =>
                acc + ((state1, tid2))
              case _ => acc
            })
          case _ => acc
        })
      }).toSet
    }
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
    def apply(): LocalEffectsMap = new LocalEffectsMap(Map[EffectTarget, List[(Effect[Addr, Abs], TID, State)]]())
  }

  class EffectsMap(m: Map[State, LocalEffectsMap]) {
    /** Called when a transition has been explored, from s1 to s2 by stepping tid, generating a set of effect */
    def newTransition(graph: Option[Graph[State, (TID, Effects)]], s1: State, s2: State, tid: TID, effects: Set[Effect[Addr, Abs]]): Either[Set[(State, TID)], EffectsMap] = {
      val ms1 = m.getOrElse(s1, LocalEffectsMap())
      if (m.contains(s2)) {
        Left(effects.foldLeft(ms1)((acc, eff) => acc.newTransition(s1, s2, tid, eff)).findConflicts)
      } else {
        Right(new EffectsMap(m + (s2 -> effects.foldLeft(ms1)((acc, eff) => acc.newTransition(s1, s2, tid, eff)))))
      }
    }
    def apply(s: State): LocalEffectsMap = m(s)
  }
  object EffectsMap {
    def apply(): EffectsMap = new EffectsMap(Map[State, LocalEffectsMap]())
  }

  def pickTid(s: State, notthisone: TID): Option[TID] = {
    s.threads.tids.filter(_ != notthisone).headOption
  }

  def id(graph: Option[Graph[State, (TID, Effects)]], s: State): Int = graph match {
    case Some(g) => g.nodeId(s)
    case None => -1
  }

  @scala.annotation.tailrec
  private def reducedLoop(todo: List[(State, TID)], visited: Set[(State, TID)], effectsMap: EffectsMap,
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]],
    sem: Semantics[Exp, Abs, Addr, Time]): ConcurrentAAMOutput =
    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else {
      todo.headOption match {
        case Some((s, tid)) =>
          if (visited.contains((s, tid))) {
            reducedLoop(todo.tail, visited, effectsMap, halted, startingTime, timeout, graph, sem)
          } else if (s.halted) {
            val conflicts = effectsMap(s).findConflicts
            reducedLoop(todo.tail ++ conflicts, visited + ((s, tid)), effectsMap, halted + s, startingTime, timeout, graph, sem)
          } else {
            val succs = s.stepTid(sem, tid)
            val newGraph = graph.map(_.addEdges(succs.map({ case (eff, s2) => (s, (tid, eff), s2) })))
            val (newEffectsMap, conflicts) = succs.foldLeft((effectsMap, Set[(State, TID)]()))((acc, succ) => succ match {
              case (effects, s2) => acc._1.newTransition(newGraph, s, s2, tid, effects) match {
                case Left(conflicts2) => (acc._1, acc._2 ++ conflicts2)
                case Right(m) => (m, acc._2)
              }
            })
            if (succs.isEmpty || succs.forall({ case (_, s2) => visited.exists(_ == (s2, tid)) })) {
              /* No successor, even though this is not a halt state: the current thread is blocked.
               All successors states already visited: the current thread is in a loop.
               In both cases, explore a different thread */
              /* In case of loop, some states probably have to be visited again! (if the effectsMap changed) */
              pickTid(s, tid) match {
                case Some(tid2) => reducedLoop(((s, tid2)) :: todo.tail ++ conflicts, visited + ((s, tid)), newEffectsMap, halted, startingTime, timeout, newGraph, sem)
                case None => reducedLoop(todo.tail ++ conflicts, visited + ((s, tid)), newEffectsMap, halted, startingTime, timeout, newGraph, sem)
              }
            } else {
              reducedLoop(succs.map(succ => (succ._2, tid)).toList ++ todo.tail ++ conflicts, visited + ((s, tid)), newEffectsMap, halted, startingTime, timeout, newGraph, sem)
            }
          }
        case None => ConcurrentAAMOutput(halted, visited.size,
          (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
      }
    }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] =
    exploration match {
      case AllInterleavings => loop(Set(State.inject(exp)), Set(), Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(allInterleavings(sem))
      case OneInterleaving => loop(Set(State.inject(exp)), Set(), Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(oneInterleaving(sem))
      case InterferenceTracking => reducedLoop(List((State.inject(exp), thread.initial)), Set(), EffectsMap(), Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None }, sem)
    }
}
