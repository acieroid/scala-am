import scalaz.Scalaz._
import scalaz._

class ActorsAAMGlobalStore[Exp : Expression, Abs : IsASchemeLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier](val M: MboxImpl[PID, Abs])
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def name = "ActorsAAMGlobalStore"

  type G = Graph[State, (PID, Option[ActorEffect])]
  object G {
    def apply(): G = new Graph()
    def apply(s: State): G = new Graph(s)
  }

  trait KontAddr
  case class NormalKontAddress(pid: PID, exp: Exp, time: Time) extends KontAddr
  case object HaltKontAddress extends KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  /* TODO: do this on two levels, because we have two fixpoints loops */
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
    kstoreDelta: KStoreDelta, mainKStoreDelta: KStoreDelta) {
    def includeDelta(d: Option[Map[Addr, Abs]]): GlobalStore = d match {
      case Some(d) => this.copy(storeDelta = storeDelta |+| d, mainStoreDelta = mainStoreDelta |+| d)
      case None => throw new Exception("GlobalStore should be used with a store that supports delta!")
    }
    def push(a: KontAddr, kont: Kont[KontAddr]): GlobalStore =
      this.copy(kstoreDelta = kstoreDelta + (a -> (kstoreDelta(a) + kont)),
        mainKStoreDelta = mainKStoreDelta + (a -> (mainKStoreDelta(a) + kont)))
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
    def restore = this.copy(store = oldStore, kstore = oldKStore, storeDelta = StoreDelta.empty, kstoreDelta = KStoreDelta.empty)
  }
  object GlobalStore {
    def initial(storeMappings: Iterable[(Addr, Abs)]): GlobalStore = {
      val store = DeltaStore[Addr, Abs](storeMappings.toMap, Map())
      val kstore = TimestampedKontStore[KontAddr](Map(), 0)
      new GlobalStore(store, store, StoreDelta.empty, StoreDelta.empty, kstore, kstore, KStoreDelta.empty, KStoreDelta.empty)
    }
  }

  object ActionHelpers extends ActorActionHelpers[Exp, Abs, Addr, Time, PID]
  import ActionHelpers._

  trait ActorInstance
  case class ActorInstanceActor(actd: Exp, env: Environment[Addr]) extends ActorInstance
  case object ActorInstanceMain extends ActorInstance

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
  case object ControlWait extends Control {
    override def toString = "wait"
  }

  case class Context(control: Control, kont: KontAddr, inst: ActorInstance, mbox: M.T, t: Time) {
    def toXml: List[scala.xml.Node] = (control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
      case ControlWait => List(<font color="skyblue">wait</font>)
    }) ++ List(scala.xml.Text(mbox.toString.take(40)))
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => inst == ActorInstanceMain && kont == HaltKontAddress
      case ControlError(_) => true
      case ControlWait => mbox.isEmpty
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }
  }
  object Context {
    def create(p: PID, actd: Exp, env: Environment[Addr]): Context =
      Context(ControlWait, HaltKontAddress, ActorInstanceActor(actd, env), M.empty, Timestamp[Time].initial(p.toString))
    def createMain(e: Exp, env: Environment[Addr]): Context =
      Context(ControlEval(e, env), HaltKontAddress, ActorInstanceMain, M.empty, Timestamp[Time].initial("main"))
  }

  case class Procs(content: CountingMap[PID, Context]) {
    def toXml: List[scala.xml.Node] = content.keys.toList.map(p => {
      val pid: scala.xml.Node = scala.xml.Text(s"$p: ")
      val entries: List[List[scala.xml.Node]] = content.lookup(p).toList.map(_.toXml)
      pid :: entries.reduceLeft({ (acc, l) => acc ++ (scala.xml.Text(", ") :: l) })
    }).reduceLeft({ (acc, l) => acc ++ (<br/> :: l) })
    def get(p: PID): Set[Context] = content.lookup(p)
    def update(v: (PID, Context)): Procs =
      Procs(content = content.update(v._1, v._2))
    def extend(v: (PID, Context)): Procs =
      Procs(content = content.extend(v._1, v._2))
    def terminate(p: PID): Procs =
      Procs(content = content.remove(p))
    def pids: Set[PID] = content.keys
    def exists(p: (PID, Context) => Boolean): Boolean = content.exists(p)
    def forall(p: (PID, Context) => Boolean): Boolean = content.forall(p)
  }
  object Procs {
    def empty: Procs = Procs(CountingMap.empty[PID, Context])
  }

  trait ActorEffect {
    def macrostepStopper: Boolean
  }
  case class ActorEffectSend(target: PID, name: String) extends ActorEffect {
    def macrostepStopper = true
    override def toString = s"$target ! $name"
  }
  case class ActorEffectSendSelf(target: PID, name: String) extends ActorEffect {
    def macrostepStopper = false
    override def toString = s"self ! $name"
  }
  case class ActorEffectTerminate(p: PID) extends ActorEffect {
    def macrostepStopper = true
    override def toString = s"x"
  }
  case class ActorEffectCreate(p: PID, name: String) extends ActorEffect {
    def macrostepStopper = false
    override def toString = s"create $name"
  }
  case class ActorEffectBecome(p: PID, name: String) extends ActorEffect {
    def macrostepStopper = true
    override def toString = s"become $name"
  }

  case class State(procs: Procs) {
    def toXml = procs.toXml
    def halted: Boolean = procs.forall((p, ctx) => ctx.halted)
    def hasError: Boolean = procs.exists((pid, ctx) => ctx.hasError)

    def integrate(p: PID, ctx: Context, act: Act, store: GlobalStore):
        (Set[(State, PID, Option[ActorEffect])], GlobalStore) = act match {
      case ActionReachedValue(v, store2, effs) =>
        (Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlKont(v), t = Timestamp[Time].tick(ctx.t)))), p, None)),
          store.includeDelta(store2.delta))
      case ActionPush(frame, e, env, store2, effs) =>
        val next = NormalKontAddress(p, e, ctx.t)
        (Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(ctx.t)))), p, None)),
          store.includeDelta(store2.delta).push(next, Kont(frame, ctx.kont)))
      case ActionEval(e, env, store2, effs) =>
        (Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(ctx.t)))), p, None)),
          store.includeDelta(store2.delta))
      case ActionStepIn(fexp, _, e, env, store2, _, effs) =>
        (Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(ctx.t, fexp)))), p, None)),
          store.includeDelta(store2.delta))
      case ActionError(err) =>
        (Set((this.copy(procs = procs.update(p, ctx.copy(control = ControlError(err)))), p, None)),
          store)
      case ActorActionSend(ptarget : PID @unchecked, name, msg, vres, effs) if ptarget != p =>
        (procs.get(ptarget).map(ctxtarget =>
          (this.copy(procs = procs
            .update(p -> ctx.copy(control = ControlKont(vres), t = Timestamp[Time].tick(ctx.t)))
            .update(ptarget -> ctxtarget.copy(mbox = ctxtarget.mbox.push((p, name, msg))))),
            p, Some(ActorEffectSend(ptarget, name)))),
          store)
      case ActorActionSend(ptarget, name, msg, vres, effs) if ptarget == p =>
        /* TODO: special care need to be taken if p maps to more than a single actor */
        (Set((this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(vres), mbox = ctx.mbox.push((p, name, msg)), t = Timestamp[Time].tick(ctx.t)))),
          p, Some(ActorEffectSendSelf(p, name)))),
          store)
      case ActorActionCreate(name, actd, exp, env2, store2, fres : (PID => Abs), effs) =>
        val p2 = ThreadIdentifier[PID].thread(exp, ctx.t)
        (Set((this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(fres(p2)), t = Timestamp[Time].tick(ctx.t)))
          .extend(p2 -> Context.create(p2, actd, env2))), p, Some(ActorEffectCreate(p, name)))),
          store.includeDelta(store2.delta))
      case ActorActionBecome(name, actd, env2, store2, vres, effs) =>
        (Set((this.copy(procs = procs
          .update(p -> Context.create(p, actd, env2).copy(t = Timestamp[Time].tick(ctx.t), mbox = ctx.mbox))), p, Some(ActorEffectBecome(p, name)))),
          store.includeDelta(store2.delta))
      case ActorActionTerminate(_) =>
        (Set((this.copy(procs = procs.terminate(p)), p, Some(ActorEffectTerminate(p)))),
          store)
    }

    def stepPid(p: PID, sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore):
        (Set[(State, PID, Option[ActorEffect])], GlobalStore) = {
      val init: (Set[(State, PID, Option[ActorEffect])], GlobalStore) = (Set.empty, store)
      procs.get(p).foldLeft(init)((acc, ctx) => ctx.control match {
        case ControlEval(e, env) => /* call semantics */
          sem.stepEval(e, env, acc._2.store, ctx.t).foldLeft(acc)((acc, action) =>
            integrate(p, ctx, action, acc._2) match {
              case (s, store2) => (acc._1 ++ s, store2)
            })
        case ControlKont(v) if ctx.kont != HaltKontAddress => /* apply continuation */
          acc._2.kstore.lookup(ctx.kont).foldLeft(acc)((acc, kont) => kont match {
            case Kont(frame, next) => sem.stepKont(v, frame, acc._2.store, ctx.t).foldLeft(acc)((acc, action) =>
              integrate(p, ctx.copy(kont = next), action, acc._2) match {
                case (s, store2) => (acc._1 ++ s, store2)
              })
          })
        case ControlKont(v) if ctx.kont == HaltKontAddress && ctx.inst != ActorInstanceMain => /* go to wait */
          (acc._1 + ((this.copy(procs = procs.update(p -> ctx.copy(control = ControlWait, t = Timestamp[Time].tick(ctx.t)))), p, None)),
            acc._2)
        case ControlKont(v) if ctx.kont == HaltKontAddress && ctx.inst == ActorInstanceMain =>
          acc /* main is stuck ath this point */
        case ControlError(_) =>
          acc /* no successor */
        case ControlWait => /* receive a message */
          ctx.inst match {
            case ActorInstanceActor(actd, env) =>
              ctx.mbox.pop.foldLeft(acc)((acc, m) => m match {
                case ((sender, name, values), mbox2) =>
                  sem.stepReceive(p, name, values, actd, env, acc._2.store, ctx.t)
                    .foldLeft(acc)((acc, action) => integrate(p, ctx.copy(mbox = mbox2), action, acc._2) match {
                      case (s, store2) => (acc._1 ++ s, store2)
                    })
              })
            case ActorInstanceMain =>
              acc /* main cannot receive messages */
          }
      })
    }
    /**
     * Performs a macrostep for a given PID. If the state is stuck, returns
     * None. Otherwise, returns the graph explored for this macrostep, as well
     * as every final state and the effect that stopped the macrostep for that
     * state. The final states *are* in the graph (unlike macrostepTrace),
     * because we need to keep track of the edge. */
    /* TODO: computing the graph can be disabled (it's mainly there for debugging purposes). */
    /* TODO: commits done to the store here should not be the same kind of commit as from the general loop */
    def macrostepPid(p: PID, store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time]):
        Option[(G, Set[(State, Option[ActorEffect])], GlobalStore)] = {
      def id(g: G, s: State): Int = g.nodeId(s)
      def loop(todo: Set[State], visited: Set[State], finals: Set[(State, Option[ActorEffect])], store: GlobalStore, graph: G):
          (G, Set[(State, Option[ActorEffect])], GlobalStore) = {
        if (todo.isEmpty) {
          (graph, finals, store)
        } else {
          println(s"Stepping frontier of ${todo.length} states")
          val (edges, newFinals, store2) = todo.foldLeft((Set[(State, (PID, Option[ActorEffect]), State)](), Set[(State, Option[ActorEffect])](), store))((acc, s) => {
            println(s"Stepping state ${id(graph, s)}")
            s.stepPid(p, sem, acc._3) match {
              case (next, _) if next.isEmpty =>
                println("No successor")
                (acc._1, acc._2 + ((s, None)), acc._3)
              case (next, store2) =>
                println(s"${next.size} successors")
                (acc._1 ++ next.map({ case (s2, p, eff) => (s, (p, eff), s2) }), acc._2 ++ next.collect({
                  case (s2, _, _) if s2.halted => (s2, None)
                  case (s2, _, Some(eff)) if eff.macrostepStopper => (s2, Some(eff))
                }), store2)
            }
          })
          val newTodo = edges.collect({
            case (_, (_, None), s2) if !s2.halted => s2
            case (_, (_, Some(eff)), s2) if !eff.macrostepStopper && !s2.halted => s2
          })
          if (store2.isUnchanged) {
            loop(newTodo.diff(visited), visited ++ todo, finals ++ newFinals, store2, graph.addEdges(edges))
          } else {
            loop(newTodo, Set(), finals ++ newFinals, store2.commit, graph.addEdges(edges))
          }
        }
      }
      loop(Set(this), Set(), Set(), store, G(this)) match {
        case (_, s, _) if s.isEmpty || (s.length == 1 && s.contains((this, None))) => None
        case (g, s, store2) => Some((g, s, store2))
      }
    }
    def macrostepAll(store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time]):
        (Set[(Set[(State, Option[ActorEffect])], PID, G)], GlobalStore) =
      procs.pids.foldLeft((Set[(Set[(State, Option[ActorEffect])], PID, G)](), store))((acc, p) => {
        println(s"macrostepping $p")
        macrostepPid(p, acc._2, sem) match {
          case Some((graph, states, store2)) => {
            graph.toDotFile("foo.dot", _.toXml,
              (s) => if (s.hasError) {
                Colors.Red
              } else {
                Colors.White
              }, {
                case (p, None) => List(scala.xml.Text(p.toString))
                case (p, Some(eff)) => List(scala.xml.Text(p.toString), <font color="red">{eff.toString}</font>)
              })
            scala.io.StdIn.readLine()
            (acc._1 + ((states, p, graph)), store2)
          }
          case None => acc
        }
      })
  }

  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): (State, GlobalStore) =
      (State(Procs.empty.extend(ThreadIdentifier[PID].initial -> Context.createMain(exp, Environment.initial[Addr](env)))),
        GlobalStore.initial(store))
  }

  case class ActorsAAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[G], timedOut: Boolean)
      extends Output {
    def finalValues: Set[Abs] = halted.flatMap(st => st.procs.get(ThreadIdentifier[PID].initial).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def containsFinalValue(v: Abs): Boolean =
      finalValues.exists(v2 => JoinLattice[Abs].subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toXml,
        (s) => if (s.hasError) {
          Colors.Red
        } else if (halted.contains(s)) {
          Colors.Yellow
        } else {
          Colors.White
        }, {
          case (p, None) => List(scala.xml.Text(p.toString))
          case (p, Some(eff)) => List(scala.xml.Text(p.toString), <font color="red">{eff.toString}</font>)
        })
      case None =>
        println("Not generating graph because no graph was computed")
    }
    import scala.util.{Try,Success,Failure}
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output = {
    val startingTime = System.nanoTime
    def id(g: Option[G], s: State): Int = g.map(_.nodeId(s)).getOrElse(-1)
    @scala.annotation.tailrec
    def loopMacrostep(todo: Set[State], visited: Set[State], reallyVisited: Set[State], halted: Set[State],
      store: GlobalStore, graph: Option[G]): ActorsAAMOutput = {
      if (todo.isEmpty || Util.timeoutReached(timeout, startingTime)) {
        ActorsAAMOutput(halted, reallyVisited.size, Util.timeElapsed(startingTime), graph, !todo.isEmpty)
      } else {
        val (edges, store2) = todo.foldLeft((Set[(State, (PID, Option[ActorEffect]), State)](), store))((acc, s) => {
          println(s"Macrostepping from state ${id(graph, s)}")
          s.macrostepAll(acc._2.restore, sem) match {
            case (next, store2) =>
              (acc._1 ++ next.flatMap({ case (ss, p, _) =>
                println(s"Results in ${ss.length} states")
                ss.map({ case (s2, eff) => (s, (p, eff), s2) })
              }), store2)
          }
        })
        if (store2.isUnchanged) {
          loopMacrostep(edges.map(_._3).diff(visited), visited ++ todo, reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2, graph.map(_.addEdges(edges)))
        } else {
          loopMacrostep(edges.map(_._3), Set(), reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2.commitMain, graph.map(_.addEdges(edges)))
        }
      }
    }
    val (initialState, store) = State.inject(exp, sem.initialEnv, sem.initialStore)
    val g = if (graph) { Some(G()) } else { None }
    loopMacrostep(Set(initialState), Set(), Set(), Set(), store, g)
  }
}
