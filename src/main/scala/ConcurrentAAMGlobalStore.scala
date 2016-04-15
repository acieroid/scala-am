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
  }

  type Exploration = (State, GlobalStore, KontStore[KontAddr]) => (Set[(TID, Effects, State)], GlobalStore, KontStore[KontAddr])

  @scala.annotation.tailrec
  private def loopOneByOne(todo: Set[State], visited: Set[State], store: GlobalStore, kstore: KontStore[KontAddr],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]])
    (step: Exploration): ConcurrentAAMOutput =
    if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, graph.map(g => g.nodes.size).getOrElse(-1), (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else {
      todo.headOption match {
        case Some(s) if visited.contains(s) =>
          loopOneByOne(todo.tail, visited, store, kstore, halted, startingTime, timeout, graph)(step)
        case Some(s) if s.halted =>
          loopOneByOne(todo.tail, visited + s, store, kstore, halted + s, startingTime, timeout, graph)(step)
        case Some(s) => {
          val (succs, store2, kstore2) = step(s, store, kstore)
          val newGraph = graph.map(_.addEdges(succs.map({ case (tid, eff, s2) => (s, (tid, eff), s2) })))
          if (store2.isUnchanged && kstore.fastEq(kstore2)) {
            loopOneByOne(todo ++ succs.map(_._3), visited + s, store2, kstore2, halted, startingTime, timeout, newGraph)(step)
          } else {
            loopOneByOne(todo ++ succs.map(_._3), Set(), store2.commit, kstore2, halted, startingTime, timeout, newGraph)(step)
          }
        }
        case None => ConcurrentAAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
      }
    }

  @scala.annotation.tailrec
  private def loopFrontier(todo: Set[State], visited: Set[State], store: GlobalStore, kstore: KontStore[KontAddr],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, (TID, Effects)]])
    (step: Exploration): ConcurrentAAMOutput =
    if (todo.isEmpty || timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      ConcurrentAAMOutput(halted, graph.map(g => g.nodes.size).getOrElse(-1), (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
    } else {
      val (edges, store2, kstore2) = todo.foldLeft((Set[(State, (TID, Effects), State)](), store, kstore))((acc, s) =>
        step(s, acc._2, acc._3) match {
          case (next, store2, kstore2) =>
            (acc._1 ++ next.map({ case (tid, eff, s2) => (s, (tid, eff), s2) }), store2, kstore2)
        })
      if (store2.isUnchanged && kstore.fastEq(kstore2)) {
        loopFrontier(edges.map(_._3).diff(visited), visited ++ todo, store2, kstore2,
          halted ++ todo.filter(_.halted), startingTime, timeout, graph.map(_.addEdges(edges)))(step)
      } else {
        loopFrontier(edges.map(_._3), Set(), store2.commit, kstore2,
          halted ++ todo.filter(_.halted), startingTime, timeout, graph.map(_.addEdges(edges)))(step)
      }
    }
  private def allInterleavings(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, store, kstore) => s.stepAll(sem, store, kstore)
  private def oneInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, store, kstore) => s.stepAny(sem, store, kstore)
  private def randomInterleaving(sem: Semantics[Exp, Abs, Addr, Time]): Exploration = (s, store, kstore) => s.stepAnyRandom(sem, store, kstore)


  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] = {
    def classicalLoop(exp: Exp, exploration: Exploration): Output[Abs] = {
      val (state, store, kstore) = State.inject(exp)
      loopFrontier(Set(state), Set(), store, kstore, Set(), System.nanoTime, timeout,
        if (graph) { Some (new Graph[State, (TID, Effects)]()) } else { None })(exploration)
    }
    exploration match {
      case AllInterleavings => classicalLoop(exp, allInterleavings(sem))
      case OneInterleaving => classicalLoop(exp, oneInterleaving(sem))
      case RandomInterleaving => classicalLoop(exp, randomInterleaving(sem))
      case _ => throw new Exception(s"Exploration not yet implemented: $exploration (TODO)")
    }
  }
}
