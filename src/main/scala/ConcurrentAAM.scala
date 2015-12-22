import scalaz.Scalaz._

class ConcurrentAAM[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
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

  private def effectsToString(effects: Set[Effect[Addr, Abs]]) = effects.map(eff => eff.kind match {
    case EffectKind.ReadEffect => s"""<font color="forestgreen">$eff</font>"""
    case EffectKind.WriteEffect => s"""<font color="red2">$eff</font>"""
  }).mkString(", ")

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
        integrate1(tid, a, act)(threads.add(tid2, Context(ControlEval(e, ρ), new KontStore[KontAddr](), HaltKontAddress, time.tick(t))), oldstore, results)
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

  case class ConcurrentAAMOutput(halted: Set[State], count: Int, t: Double, graph: Option[Graph[State, (TID, Effects)]])
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.threads.get(thread.initial).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def numberOfStates = count
    def time = t
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, node => HTMLString(HTMLString.escape(node.toString)),
        (s) => if (halted.contains(s)) { HTMLString("#FFFFDD") } else { HTMLString("#FFFFFF") }, {
          case (tid, eff) => HTMLString(s"$tid ${effectsToString(eff)}")
        })
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  type Exploration = (State, Set[State]) => Set[(TID, Effects, State)]

  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], startingTime: Long, graph: Option[Graph[State, (TID, Effects)]])
    (step: Exploration): ConcurrentAAMOutput =
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s)) {
          loop(todo.tail, visited, halted, startingTime, graph)(step)
        } else if (s.halted) {
          loop(todo.tail, visited + s, halted + s, startingTime, graph)(step)
        } else {
          val succs = step(s, todo)
          val newGraph = graph.map(_.addEdges(succs.map({ case (tid, eff, s2) => (s, (tid, eff), s2) })))
          loop(todo.tail ++ succs.map(_._3), visited + s, halted, startingTime, newGraph)(step)
        }
      case None => ConcurrentAAMOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
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

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean): Output[Abs] =
    loop(Set(State.inject(exp)), Set(), Set(), System.nanoTime,
      if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(partialOrderReduced(sem, noReduction))
}
