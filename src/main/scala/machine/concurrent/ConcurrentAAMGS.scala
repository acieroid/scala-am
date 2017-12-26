import scalaz.Scalaz._
import scalaz._

trait ExplorationType
case object AllInterleavings extends ExplorationType
case object Macrostepping extends ExplorationType
object ExplorationTypeParser extends scala.util.parsing.combinator.RegexParsers {
  val all = "AllInterleavings".r ^^ (_ => AllInterleavings)
  val one = "Macrostepping".r ^^ (_ => Macrostepping)
  def expl: Parser[ExplorationType] = all | one
  def parse(s: String): ExplorationType = parseAll(expl, s) match {
    case Success(res, _) => res
    case Failure(msg, _) => throw new Exception(s"cannot parse exploration type: $msg")
    case Error(msg, _) => throw new Exception(s"cannot parse exploration type: $msg")
  }
}

/* TODO: record write, reads etc. */
class ConcurrentAAMGS[Exp : Expression, Abs : IsCSchemeLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier](exploration: ExplorationType, recordValues: Boolean)
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
  def thread = implicitly[ThreadIdentifier[PID]]

  def name = s"ConcurrentAAMGS($exploration)"

  class Recorded[V](val recordName: String, val toStr: V => String = (v: V) => v.toString) {
    var content: Map[PID, Set[V]] = Map.empty.withDefaultValue(Set.empty)
    def record(t: PID, v: V) = if (recordValues) {
      content = content + (t -> (content(t) + v))
    }
    def keys: Set[PID] = content.keySet
    def report(t: PID): String = content(t).map({
      case v =>
        s"($recordName ${toStr(v)})"
    }).mkString(" ")
  }
  object Recorded {
    def empty[V](recordName: String): Recorded[V] = new Recorded(recordName)
    def emptyF[V](recordName: String, toStr: V => String): Recorded[V] = new Recorded(recordName, toStr)
  }

  val recordedCreate: Recorded[PID] = Recorded.empty[PID]("create")
  val recordedJoin: Recorded[PID] = Recorded.empty[PID]("join")
  val recordedRead: Recorded[(Addr, Abs)] = Recorded.emptyF[(Addr, Abs)]("read", { case (a, v) => s"$a $v" })
  val recordedWrite: Recorded[(Addr, Abs)] = Recorded.emptyF[(Addr, Abs)]("write", { case (a, v) => s"$a $v" })
  val recordedAcquire: Recorded[Addr] = Recorded.empty[Addr]("acquire")
  val recordedRelease: Recorded[Addr] = Recorded.empty[Addr]("release")

  trait KontAddr
  case class NormalKontAddress(tid: PID, exp: Exp, t: Time) extends KontAddr
  case object HaltKontAddress extends KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  trait Control
  case class ControlEval(e: Exp, env: Environment[Addr]) extends Control {
    override def toString = s"ev($e)"
  }
  case class ControlKont(v: Abs) extends Control {
    override def toString = s"ko($v)"
  }
  case class ControlTerminated(v: Abs) extends Control {
    override def toString = s"done($v)"
  }
  case class ControlError(err: SemanticError) extends Control {
    override def toString = s"err($err)"
  }

  type KStoreDelta = Map[KontAddr, Set[Kont[KontAddr]]]
  object KStoreDelta {
    def empty: KStoreDelta = Map().withDefaultValue(Set.empty)
  }
  type StoreDelta = Map[Addr, Abs]
  object StoreDelta {
    def empty: StoreDelta = Map()
  }
  case class GlobalStore(store: DeltaStore[Addr, Abs], oldStore: DeltaStore[Addr, Abs],
    storeDelta: StoreDelta, mainStoreDelta: StoreDelta,
    kstore: TimestampedKontStore[KontAddr], oldKStore: TimestampedKontStore[KontAddr],
    kstoreDelta: KStoreDelta, mainKStoreDelta: KStoreDelta,
    results: Map[PID, Abs]) {
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
    def isDone(p: PID): Boolean = results.contains(p)
    def getResult(p: PID): Abs = results(p)
    def setResult(p: PID, v: Abs): GlobalStore = if (results.contains(p)) {
      this.copy(results = results + ((p -> JoinLattice[Abs].join(results(p), v))))
    } else {
      this.copy(results = results + ((p -> v)))
    }
  }
  object GlobalStore {
    def initial(storeMappings: Iterable[(Addr, Abs)]): GlobalStore = {
      val store = DeltaStore[Addr, Abs](storeMappings.toMap, Map())
      val kstore = TimestampedKontStore[KontAddr](Map(), 0)
      new GlobalStore(store, store, StoreDelta.empty, StoreDelta.empty, kstore, kstore, KStoreDelta.empty, KStoreDelta.empty, Map.empty)
    }
  }

  case class ThreadResults(content: Map[PID, Abs]) {
    def isDone(tid: PID): Boolean = content.contains(tid)
    def get(tid: PID): Abs = content.getOrElse(tid, abs.bottom)
    def add(tid: PID, v: Abs): ThreadResults = ThreadResults(content + (tid -> abs.join(get(tid), v)))
    def join(that: ThreadResults): ThreadResults = ThreadResults(this.content |+| that.content)
  }

  object ActionHelpers extends ActorActionHelpers[Exp, Abs, Addr, Time, PID]
  import ActionHelpers._

  case class Context(control: Control, kont: KontAddr, t: Time) {
    def toXml: List[scala.xml.Node] = control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
      case ControlTerminated(v) => List(<font color="black">{v.toString.take(40)}</font>)
    }
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(_) => kont == HaltKontAddress
      case ControlTerminated(_) => true
      case ControlError(_) => true
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }

    type CreatedProcesses = Set[(PID, Context)]
    object CreatedProcesses {
      def empty: CreatedProcesses = Set.empty
      def apply(x: (PID, Context)): CreatedProcesses = Set(x)
    }

    def recordEffects(p: PID, effs: Set[Effect[Addr]]): Unit = {
      effs.foreach({
        case EffectWriteReference(a, v : Abs @unchecked) =>
          recordedWrite.record(p, (a, v))
        case EffectReadReference(a, v : Abs @unchecked) =>
          recordedRead.record(p, (a, v))
        case EffectAcquire(a) =>
          recordedAcquire.record(p, a)
        case EffectRelease(a) =>
          recordedRelease.record(p, a)
        case _ => ()
      })
    }

    /** Returns: the new context (or None if it terminated), the processes created,
      * and the updated global store */
    def integrate(p: PID, act: Act, store: GlobalStore):
        (MacrostepState, GlobalStore) = act match {
      case ActionReachedValue(v, store2, effs) =>
        recordEffects(p, effs)
        ((Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          CreatedProcesses.empty, effs),
          store.includeDelta(store2.delta))
      case ActionPush(frame, e, env, store2, effs) =>
        recordEffects(p, effs)
        val next = NormalKontAddress(p, e, t)
        ((Some(this.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(t))),
          CreatedProcesses.empty, effs),
          store.includeDelta(store2.delta).push(next, Kont(frame, kont)))
      case ActionEval(e, env, store2, effs) =>
        recordEffects(p, effs)
        ((Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t))),
          CreatedProcesses.empty, effs),
          store.includeDelta(store2.delta))
      case ActionStepIn(fexp, clo, e, env, store2, argsv, effs) =>
        recordEffects(p, effs)
        ((Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t, fexp))),
          CreatedProcesses.empty, effs),
          store.includeDelta(store2.delta))
      case ActionError(err) =>
        ((Some(this.copy(control = ControlError(err))),
          CreatedProcesses.empty, Set.empty),
          store)
      case ActionSpawn(tid2: PID @unchecked, e, env, store2, v, effs) =>
        recordEffects(p, effs)
        recordedCreate.record(p, tid2)
        ((Some(this.copy(control = ControlKont(IsCSchemeLattice[Abs].injectTid(tid2)))),
          CreatedProcesses((tid2, Context(ControlEval(e, env), HaltKontAddress, Timestamp[Time].initial(tid2.toString)))),
          effs),
          store.includeDelta(store2.delta))
      case ActionJoin(tid2: PID @unchecked, store2, effs) =>
        recordEffects(p, effs)
        recordedJoin.record(p, tid2)
        if (store.isDone(tid2)) {
          ((Some(this.copy(control = ControlKont(store.getResult(tid2)))),
            CreatedProcesses.empty, effs),
            store.includeDelta(store2.delta))
        } else {
          ((None, CreatedProcesses.empty, Set.empty), store)
        }
    }
    def step(p: PID, sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore):
        (Set[MacrostepState], GlobalStore) = {
      val init: (Set[MacrostepState], GlobalStore) = (Set.empty, store)
      control match {
        case ControlEval(e, env) => sem.stepEval(e, env, store.store, t).foldLeft(init)((acc, action) =>
          integrate(p, action, acc._2) match { case (s, store2) => (acc._1 + s, store2) }
        )
        case ControlKont(v) if kont != HaltKontAddress =>
          store.kstore.lookup(kont).foldLeft(init)((acc, kont) => kont match {
            case Kont(frame, next) => sem.stepKont(v, frame, acc._2.store, t).foldLeft(acc)((acc, action) =>
              this.copy(kont = next).integrate(p, action, acc._2) match { case (s, store2) => (acc._1 + s, store2) })
          })
        case ControlKont(v) if kont == HaltKontAddress =>
          (Set((Some(this.copy(control = ControlTerminated(v))), CreatedProcesses.empty, Set.empty)), store.setResult(p, v))
        case ControlError(_) => (Set.empty, store)
        case ControlTerminated(v) =>
          (Set((None, CreatedProcesses.empty, Set.empty)), store)
      }
    }

    /* New context, created processes */
    type MacrostepState = (Option[Context], CreatedProcesses, Set[Effect[Addr]])
    def macrostep(p: PID, store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time], timeout: Timeout):
        (Set[MacrostepState], GlobalStore) = {
      def loop(todo: Set[MacrostepState], visited: Set[MacrostepState], finals: Set[MacrostepState], store: GlobalStore):
          (Set[MacrostepState], GlobalStore) = {
        if (todo.isEmpty || timeout.reached) {
          (finals, store)
        } else {
          val (next, newFinals, store2) = todo.foldLeft((Set[MacrostepState](), Set[MacrostepState](), store))((acc, s) => s match {
            case (None, _, _) => throw new RuntimeException("Terminated context in frontier of macrostep!")
            case (Some(ctx), cr, effs) =>
              recordEffects(p, effs)
              val (next, store2) = ctx.step(p, sem, acc._3)
              if (next.isEmpty) {
                (acc._1, acc._2 + ((Some(ctx), cr, effs)), acc._3)
              } else {
                val (newTodo, newFinals) = next.partition({
                  case (Some(ctx2), cr2, effs2) =>
                    !ctx2.halted && cr2.isEmpty && effs.isEmpty
                  case (None, _, _) =>
                    false
                })
                (acc._1 ++ newTodo.map({
                  case (Some(ctx2), cr2, effs2) =>
                    (Some(ctx2), cr ++ cr2, effs ++ effs2)
                  case (None, _, _) => throw new RuntimeException("Should not happen")
                }),
                  acc._2 ++ newFinals.map({
                    case (ctx2, cr2, effs2) if cr.isEmpty && cr2.isEmpty && effs.isEmpty && effs2.isEmpty =>
                      (ctx2, cr, effs)
                    case (_, cr2, effs2) if !cr.isEmpty || !effs.isEmpty =>
                      (Some(ctx), cr ++ cr2, effs ++ effs2)
                    case (ctx2, cr2, effs2) if (cr.isEmpty && !cr2.isEmpty) || (effs.isEmpty && !effs2.isEmpty) =>
                      (ctx2, cr2, effs2)
                  }), store2)
              }
          })
          if (store2.isUnchanged) {
            loop(next.diff(visited), visited ++ todo, finals ++ newFinals, store2)
          } else {
            loop(next, Set(), finals ++ newFinals, store2.commit)
          }
        }
      }
      this.step(p, sem, store) match {
        case (next, store2) if !next.isEmpty && next.forall({ case (ctx, cr, effs) => !ctx.isEmpty }) =>
          val res = loop(next, Set(), Set(), store2)
          // println(s"Macrostepping $this: ${res._1}")
          res
        case _ =>
          (Set(), store)
      }
    }
  }

  case class Procs(content: CountingMap[PID, Context]) {
    def toXml: List[scala.xml.Node] = content.keys.toList.map(p => {
      val pid: scala.xml.Node = scala.xml.Text(s"$p: ")
      val entries: List[List[scala.xml.Node]] = content.lookup(p).toList.map(_.toXml)
      val tail: List[scala.xml.Node] = entries.reduceLeft({ (acc, l) => acc ++ (scala.xml.Text(", ") :: l) })
      if (entries.length > 1) {
        pid :: scala.xml.Text("*") :: tail
      } else {
        pid :: tail
      }
    }).reduceLeft({ (acc, l) => acc ++ (<br/> :: l) })
    def get(p: PID): Set[Context] = content.lookup(p)
    def update(v: (PID, Context)): Procs =
      Procs(content = content.update(v._1, v._2))
    def extend(v: (PID, Context)): Procs =
      Procs(content = content.extend(v._1, v._2))
    def terminate(p: PID): Procs = this /* to ensure soundness */
    def pids: Set[PID] = content.keys
    def exists(p: (PID, Context) => Boolean): Boolean = content.exists(p)
    def forall(p: (PID, Context) => Boolean): Boolean = content.forall(p)
    def foreach(p: (PID, Context) => Unit): Unit = content.foreach(p)
  }
  object Procs {
    def empty: Procs = Procs(CountingMap.empty[PID, Context])
  }


  case class State(procs: Procs) {
    def toXml = procs.toXml
    def halted: Boolean = procs.forall((p, ctx) => ctx.halted)
    def hasError: Boolean = procs.exists((pid, ctx) => ctx.hasError)

    def macrostepPid(p: PID, store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time], timeout: Timeout):
        Option[(Set[State], GlobalStore)] = {
      val init: (Set[State], GlobalStore) = (Set.empty, store)
      val res = procs.get(p).foldLeft(init)((acc, ctx) => {
        val (res, store2) = ctx.macrostep(p, acc._2, sem, timeout)
        val next = res.map({
          case (ctx2, created, effs) =>
            val withCtx = ctx2 match {
              case Some(newCtx) => this.copy(procs = procs.update(p -> newCtx))
              case None => this.copy(procs = procs.terminate(p))
            }
            val withCreated = if (created.isEmpty) { withCtx } else {
              withCtx.copy(procs = created.foldLeft(withCtx.procs)((procs, cr) => procs.extend(cr)))
            }
            withCreated
        })
        (acc._1 ++ next, store2)
      })
      if (res._1.isEmpty) {
        None
      } else {
        Some(res)
      }
    }
    def stepPid(p: PID, store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time], timeout: Timeout):
        Option[(Set[State], GlobalStore)] = {
      val init: (Set[State], GlobalStore) = (Set.empty, store)
      val res = procs.get(p).foldLeft(init)((acc, ctx) => {
        val (res, store2) = ctx.step(p, sem, acc._2)
        val next = res.map({
          case (ctx2, created, effs) =>
            val withCtx = ctx2 match {
              case Some(newCtx) => this.copy(procs = procs.update(p -> newCtx))
              case None => this.copy(procs = procs.terminate(p))
            }
            val withCreated = if (created.isEmpty) { withCtx } else {
              withCtx.copy(procs = created.foldLeft(withCtx.procs)((procs, cr) => procs.extend(cr)))
            }
            withCreated
        })
        (acc._1 ++ next, store2)
      })
      if (res._1.isEmpty) {
        None
      } else {
        Some(res)
      }
    }
    def macrostepAll(store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time], timeout: Timeout):
        (Set[(Set[State], PID)], GlobalStore) =
      procs.pids.foldLeft((Set[(Set[State], PID)](), store))((acc, p) => {
        macrostepPid(p, acc._2, sem, timeout) match {
          case Some((states, store2)) => {
            (acc._1 + ((states, p)), store2)
          }
          case None =>
            acc
        }
      })
    def stepAll(store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time], timeout: Timeout):
        (Set[(Set[State], PID)], GlobalStore) =
      procs.pids.foldLeft((Set[(Set[State], PID)](), store))((acc, p) => {
        stepPid(p, acc._2, sem, timeout) match {
          case Some((states, store2)) => {
            (acc._1 + ((states, p)), store2)
          }
          case None =>
            acc
        }
      })
  }

  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): (State, GlobalStore) = {
      (State(Procs.empty.extend(ThreadIdentifier[PID].initial -> Context(ControlEval(exp, Environment.initial[Addr](env)), HaltKontAddress, Timestamp[Time].initial("main")))),
        GlobalStore.initial(store))
    }
  }

  type Annot = PID
  type G = Graph[State, Annot, Unit]
  implicit val annot = new GraphAnnotation[Annot, Unit] {
    override def labelXml(annot: PID) = List(scala.xml.Text(annot.toString))
  }
  implicit val graphNode = new GraphNode[State, Unit] {
    override def labelXml(n: State) = n.toXml
    override def color(n: State) = if (n.halted) {
      Colors.Yellow
    } else if (n.procs.exists((pid, ctx) => ctx.control.isInstanceOf[ControlError])) {
      Colors.Red
    } else {
      Colors.White
    }
  }

  case class ConcurrentAAMGSOutput(halted: Set[State], store: Store[Addr, Abs],
    numberOfStates: Int, time: Double, graph: Option[G], timedOut: Boolean)
      extends Output {
    def finalValues = halted.flatMap(st => st.procs.get(thread.initial).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def toFile(path: String)(output: GraphOutput) = graph match {
      case Some(g) => output.toFile(g, ())(path)
      case None => println("Not generating graph because no graph was computed")
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    val startingTime = System.nanoTime
    @scala.annotation.tailrec
    def loopMacrostep(todo: Set[State], visited: Set[State], reallyVisited: Set[State], halted: Set[State],
      store: GlobalStore, graph: Option[G]): ConcurrentAAMGSOutput = {
      if (todo.isEmpty || timeout.reached) {
        ConcurrentAAMGSOutput(halted, store.store, reallyVisited.size, timeout.time, graph, !todo.isEmpty)
      } else {
        val (edges, store2) = todo.foldLeft((Set[(State, PID, State)](), store))((acc, s) => {
          val (next, store2) = s.macrostepAll(acc._2.restore, sem, timeout)
          (acc._1 ++ next.flatMap({ case (ss, p) =>
            ss.map({ case s2 => (s, p, s2) })
          }), store2)
        })
        val newTodo = edges.map(_._3)
        val newGraph = graph.map(_.addEdges(edges))
        if (store2.mainIsUnchanged) {
          loopMacrostep(newTodo.diff(visited), visited ++ todo, reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2, newGraph)
        } else {
          loopMacrostep(newTodo, Set(), reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2.commitMain, newGraph)
        }
      }
    }
    @scala.annotation.tailrec
    def loopAll(todo: Set[State], visited: Set[State], reallyVisited: Set[State], halted: Set[State],
      store: GlobalStore, graph: Option[G]): ConcurrentAAMGSOutput = {
      if (todo.isEmpty || timeout.reached) {
        ConcurrentAAMGSOutput(halted, store.store, reallyVisited.size, timeout.time, graph, !todo.isEmpty)
      } else {
        val (edges, store2) = todo.foldLeft((Set[(State, PID, State)](), store))((acc, s) => {
          val (next, store2) = s.stepAll(acc._2.restore, sem, timeout)
          (acc._1 ++ next.flatMap({ case (ss, p) =>
            ss.map({ case s2 => (s, p, s2) })
          }), store2)
        })
        val newTodo = edges.map(_._3)
        val newGraph = graph.map(_.addEdges(edges))
        if (store2.mainIsUnchanged) {
          loopAll(newTodo.diff(visited), visited ++ todo, reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2, newGraph)
        } else {
          loopAll(newTodo, Set(), reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2.commitMain, newGraph)
        }
      }
    }
    val (initialState, store) = State.inject(exp, sem.initialEnv, sem.initialStore)
    val g = if (graph) { Some(Graph.empty[State, Annot, Unit]) } else { None }
    val res = exploration match {
      case Macrostepping => loopMacrostep(Set(initialState), Set(), Set(), Set(), store, g)
      case AllInterleavings => loopAll(Set(initialState), Set(), Set(), Set(), store, g)
    }
    reportRecorded()
    res
  }
  def reportRecorded(): Unit = {
    def keys = recordedCreate.keys ++ recordedJoin.keys ++ recordedRead.keys ++ recordedWrite.keys ++ recordedAcquire.keys ++ recordedRelease.keys
    println("=========")
    keys.foreach(k =>
      println(s"($k (${recordedCreate.report(k)} ${recordedJoin.report(k)} ${recordedRead.report(k)} ${recordedWrite.report(k)} ${recordedAcquire.report(k)} ${recordedRelease.report(k)}))")
    )
  }

}
