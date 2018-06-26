import scalaz.Scalaz._
import scalaz.{Ordering => _, _}

class ModularAAM[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def name = "ModularAAM"

  type G = Graph[FunctionState, Unit, Unit]
  implicit val graphNode = new GraphNode[FunctionState, Unit] {
    override def labelXml(n: FunctionState) = n.toXml
    override def color(n: FunctionState) = if (n.hasError) { Colors.Red } else if (n.halted) { Colors.Yellow } else { Colors.White }
  }
  object G {
    def apply(): G = Graph.empty[FunctionState, Unit, Unit]
    def apply(s: FunctionState): G = Graph.node[FunctionState, Unit, Unit](s)
  }

  trait KontAddr
  case class NormalKontAddress(exp: Exp, time: Time) extends KontAddr
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
  case class GlobalStore(store: DeltaStore[Addr, Abs], kstore: TimestampedKontStore[KontAddr]) {
    def push(a: KontAddr, kont: Kont[KontAddr]): GlobalStore =
      if (kstore.lookup(a).contains(kont)) { this } else {
        this.copy(kstore = kstore.extend(a, kont).asInstanceOf[TimestampedKontStore[KontAddr]])
      }
    def include(s2: GlobalStore): GlobalStore =
      this.copy(
        store = store.addDelta(s2.store.d),
        kstore = kstore.join(s2.kstore).asInstanceOf[TimestampedKontStore[KontAddr]])
  }
  object GlobalStore {
    def initial(storeMappings: Iterable[(Addr, Abs)]): GlobalStore = {
      val store = DeltaStore[Addr, Abs](storeMappings.toMap, Map())
      val kstore = TimestampedKontStore[KontAddr](Map(), 0)
      new GlobalStore(store, kstore)
    }
  }

  object ActionHelpers extends ActionHelpers[Exp, Abs, Addr]
  import ActionHelpers._

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

  implicit val stateWithKey = new WithKey[FunctionState] {
    type K = KontAddr
    def key(st: FunctionState) = st.kont
  }

  type Clo = (String, Exp, Environment[Addr])
  def fromEffs(effs: Set[Effect[Addr]], store: Store[Addr, Abs], store2: Store[Addr, Abs]): Set[ModEffect] =
    effs.collect({
      case e: Effect[Addr] if e.kind == WriteEffect && store.lookup(e.target) != store2.lookup(e.target) => ModEffectWrite(e.target)
      case e: Effect[Addr] if e.kind == ReadEffect => ModEffectRead(e.target)
    })

  case class FunctionState(clo: Clo, control: Control, kont: KontAddr, t: Time) {
    def toXml: List[scala.xml.Node] = control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
    }
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(_) => kont == HaltKontAddress
      case ControlError(_) => true
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }
    def integrate(act: Act, store: GlobalStore):
        (/* successor state */
          Option[FunctionState],
          /* successor store */
          GlobalStore,
          /* effects */
          Set[ModEffect]) = Profiler.profile("integrate") { act match {
            case ActionReachedValue(v, store2, effs) =>
              (Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
                store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)),
                fromEffs(effs, store.store, store2))
            case ActionPush(frame, e, env, store2, effs) =>
              val next = NormalKontAddress(e, t)
              (Some(this.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(t))),
                store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)).push(next, Kont(frame, kont)),
                fromEffs(effs, store.store, store2))
            case ActionEval(e, env, store2, effs) =>
              (Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t))),
                store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)),
                fromEffs(effs, store.store, store2))
            case ActionStepIn(fexp, clo, e, env, store2, argsv, effs) =>
              val retaddr = RetAddr(("clo", e, env))
              val v = store.store.lookupBot(retaddr)
              (Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t, fexp))),
                store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)),
                /* A call to a function is effectively performing the call followed by reading the value at its return address */
                fromEffs(effs, store.store, store2) + ModEffectCall(("clo", e, env)) + ModEffectRead(RetAddr(("clo", e, env))))
            case ActionError(err) =>
              (Some(this.copy(control = ControlError(err))),
                store,
                Set.empty)
          }
    }
    def step(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore):
        (/* successor states */
          Set[FunctionState],
          /* resulting store */
          GlobalStore,
          /* effects */
          Set[ModEffect]
        ) = Profiler.profile("step") {
      val init: (Set[FunctionState], GlobalStore, Set[ModEffect]) = (Set.empty, store, Set.empty)
      control match {
        case ControlEval(e, env) =>
          sem.stepEval(e, env, store.store, t).foldLeft(init)((acc, action) =>
            integrate(action, acc._2) match { case (s, store2, effs) =>
              (acc._1 ++ s.toSet, store2, acc._3 ++ effs)
            })
        case ControlKont(v) if kont != HaltKontAddress =>
          store.kstore.lookup(kont).foldLeft(init)((acc, kont) => kont match {
            case Kont(frame, next) => sem.stepKont(v, frame, acc._2.store, t).foldLeft(acc)((acc, action) =>
              this.copy(kont = next).integrate(action, acc._2) match {
                case (s, store2, effs) =>
                  (acc._1 ++ s.toSet, store2, acc._3 ++ effs)
              })
          })
        case ControlKont(v) if kont == HaltKontAddress =>
          /* function has returned, write effect */
          val retaddr = RetAddr(clo)
          Recorder.funRet(clo._2, JoinLattice[Abs].typesOf(v))
          val store2 = store.store.extend(retaddr, v)
          (Set.empty, store.copy(store = store2.asInstanceOf[DeltaStore[Addr, Abs]]), fromEffs(Set(Effect.writeVariable(retaddr)), store.store, store2))
        case ControlError(_) =>
          (Set.empty, store, Set.empty)
      }
    }
  }

  trait ModEffect
  case class ModEffectRead(addr: Addr) extends ModEffect
  case class ModEffectWrite(addr: Addr) extends ModEffect
  case class ModEffectCall(clo: Clo) extends ModEffect
  def RetAddr(clo: Clo): Addr = Address[Addr].cell(clo._2, Timestamp[Time].initial("")) /* should be cached, but doesn't really matter in the end */

  val graphId = new java.util.concurrent.atomic.AtomicInteger(0)
  case class ModularAAMOutput(n: Int, v: Abs, time: Double, graphs: Map[Clo, Option[G]], timedOut: Boolean) extends Output {
    def numberOfStates = n
    def finalValues: Set[Abs] = Set(v)
    def toFile(path: String)(output: GraphOutput) =
      graphs.foreach({
        case (k, Some(g)) => output.toFile(g, ())(path + "-" + graphId.getAndIncrement + ".dot")
        case (_, None) => ()
      })
  }


  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    type WL[A] = Set[A]
    val startingTime = System.nanoTime

    def inject(e: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): (Clo, FunctionState, GlobalStore) = {
      val initEnv = Environment.initial[Addr](env)
      (("main", e, initEnv),
        FunctionState(("main", e, initEnv), ControlEval(e, initEnv), HaltKontAddress, Timestamp[Time].initial("")),
        GlobalStore.initial(store))
    }

    val (initialClo, initialState, initialStore) = inject(exp, sem.initialEnv, sem.initialStore)
    val MainRetAddr: Addr = RetAddr(initialClo)

    @scala.annotation.tailrec
    def intraAnalysis(clo: Clo, todo: WL[FunctionState], graph: Option[G], store: GlobalStore, effs: Set[ModEffect]): (Option[G], GlobalStore, Set[ModEffect]) = {
      if (todo.isEmpty || timeout.reached) {
        (graph, store, effs)
      } else {
        val init = (Set[(FunctionState, Unit, FunctionState)](), store, effs)
        val (edges, store2, effs2) = todo.foldLeft(init)((acc, state) => {
          state.step(sem, acc._2) match { /* step : sem, store -> succs, store, effs */
            case (succs, store2, effs) =>
              (acc._1 ++ succs.map(state2 => (state, (), state2)), store2, acc._3 ++ effs)
          }
        })
        val newTodo = edges.map(_._3)
        val newGraph = graph.map(_.addEdges(edges))
        intraAnalysis(clo, newTodo, newGraph, store2, effs2)
      }
    }

    import scala.collection.mutable.PriorityQueue
    implicit val ordering: Ordering[Clo] = new Ordering[Clo] {
      def compare(x: Clo, y: Clo): Int = {
        val a = Expression[Exp].pos(x._2)
        val b = Expression[Exp].pos(y._2)
        if (a < b) { 1 } else if (a == b) { 0 } else { -1 }
      }
    }

    @scala.annotation.tailrec
    def interAnalysis(todo: PriorityQueue[Clo], store: GlobalStore, readDeps: Map[Addr, Set[Clo]], graphs: Map[Clo, G]): (Map[Clo, G], Abs) = {
      if (todo.isEmpty || timeout.reached) {
        (graphs, store.store.lookupBot(MainRetAddr))
      } else {
        val clo = todo.dequeue
        val fstate = FunctionState(clo, ControlEval(clo._2, clo._3), HaltKontAddress, Timestamp[Time].initial(""))
        intraAnalysis(clo, Set(fstate), Option(G()), store, Set()) match {
          case (g, store2, effs) =>
            /* Note: we could avoid adding to the worklist elements that are already in it (because it's a vector and not a set) */
            val todo2: Set[Clo] = effs.flatMap({
              /* !! we look up retval in the old store, because f could call itself */
              case ModEffectCall(clo) =>
                if (store.store.lookup(RetAddr(clo)).isDefined) {
                  Set[Clo]()
                } else {
                  Set[Clo](clo)
                }
              case ModEffectWrite(a) =>
                readDeps(a)
              case _ => Set[Clo]()
            })
            val readDeps2 = effs.foldLeft(readDeps)((acc, eff) => eff match {
              case ModEffectRead(a) =>
                acc + (a -> (acc(a) + clo))
              case _ => acc
            })
            val graphs2 = g.fold(graphs)(gv => graphs + (clo -> gv))
            todo ++= todo2
            interAnalysis(todo, store2, readDeps2, graphs2)
        }
      }
    }

    val initTodo = new PriorityQueue[Clo]()
    initTodo.enqueue(initialClo)
    val (graphs, v) = interAnalysis(initTodo, initialStore, Map[Addr, Set[Clo]]().withDefaultValue(Set.empty), Map[Clo, G]())
    ModularAAMOutput(graphs.keySet.foldLeft(0)((acc, clo) => acc + graphs(clo).size), v,
      timeout.time, graphs.map({ case (k, v) => (k, Option(v)) }), timeout.reached)
  }
}
