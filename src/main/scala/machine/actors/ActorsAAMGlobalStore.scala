import scalaz.Scalaz._
import scalaz._

object Logger {
  var enabled = false
  def enable = enabled = true
  def disable = enabled = false
  def withLog[A](body: => A): A = {
    enable
    val res = body
    disable
    res
  }
  def withLog[A](cond: => Boolean)(body: => A): A =
    if (cond) { withLog { body } } else { body }
  def log(m: String) =
    if (enabled) {
      println(m)
    }
}

class ActorsAAMGlobalStore[Exp : Expression, Abs : IsASchemeLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier](val M: MboxImpl[PID, Abs])
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def name = "ActorsAAMGlobalStore"

  type G = Graph[State, (PID, ActorEffect)]
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
    def trimMbox: Context = this.copy(mbox = M.empty)
    /** Returns: the new context (or None if it terminated), the processes created,
      * an optinoal effect, an optional message sent, and the updated global store */
    def integrate(p: PID, act: Act, store: GlobalStore):
        (MacrostepState, GlobalStore) = act match {
      case ActionReachedValue(v, store2, effs) =>
        ((Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          Set.empty, ActorEffect.empty, None, None),
          store.includeDelta(store2.delta))
      case ActionPush(frame, e, env, store2, effs) =>
        val next = NormalKontAddress(p, e, t)
        ((Some(this.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(t))),
          Set.empty, ActorEffect.empty, None, None),
          store.includeDelta(store2.delta).push(next, Kont(frame, kont)))
      case ActionEval(e, env, store2, effs) =>
        ((Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t))),
          Set.empty, ActorEffect.empty, None, None),
          store.includeDelta(store2.delta))
      case ActionStepIn(fexp, clo, e, env, store2, argsv, effs) =>
        ((Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t, fexp))),
          Set.empty, ActorEffect.empty, None, None),
          store.includeDelta(store2.delta))
      case ActionError(err) =>
        ((Some(this.copy(control = ControlError(err))),
          Set.empty, ActorEffect.empty, None, None),
          store)
      case ActorActionBecome(name, actd, env2, store2, vres, effs) =>
        ((Some(Context.create(p, actd, env2).copy(t = Timestamp[Time].tick(t), mbox = mbox)),
          Set.empty, ActorEffect.become(p, name), None, None),
          store.includeDelta(store2.delta))
      case ActorActionTerminate(_) =>
        ((None, Set.empty, ActorEffect.terminate(p), None, None),
          store)
      case ActorActionCreate(name, actd, exp, env2, store2, fres : (PID => Abs), effs) =>
        val p2 = ThreadIdentifier[PID].thread(exp, t)
        ((Some(this.copy(control = ControlKont(fres(p2)), t = Timestamp[Time].tick(t))),
          Set(p2 -> Context.create(p2, actd, env2)), ActorEffect.create(p, name), None, None),
          store.includeDelta(store2.delta))
      case ActorActionSend(ptarget : PID @unchecked, name, msg, vres, effs) =>
        ((Some(this.copy(control = ControlKont(vres), t = Timestamp[Time].tick(t))),
          Set.empty, ActorEffect.send(ptarget, name, msg), Some((ptarget, name, msg)), None),
          store)
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
        case ControlKont(v) if kont == HaltKontAddress && inst != ActorInstanceMain =>
          (Set((Some(this.copy(control = ControlWait, t = Timestamp[Time].tick(t))), Set.empty, ActorEffect.empty, None, None)), store)
        case ControlKont(v) if kont == HaltKontAddress && inst == ActorInstanceMain =>
          (Set.empty, store)
        case ControlError(_) => (Set.empty, store)
        case ControlWait => inst match {
          case ActorInstanceActor(actd, env) =>
            mbox.pop.foldLeft(init)((acc, m) => m match {
              case (message @ (sender, name, values), mbox2) =>
                Logger.log(s"Mailbox has message $message")
                sem.stepReceive(p, name, values, actd, env, acc._2.store, t).foldLeft(acc)((acc, action) =>
                  this.copy(mbox = mbox2).integrate(p, action, acc._2) match {
                    case ((s, n, eff, sent, recv), store2) =>
                      assert(recv == None)
                      (acc._1 + ((s, n, ActorEffect.combine(eff, ActorEffect.receive(p, name, values)), sent, Some(message))), store2)
                  })
            })
          case ActorInstanceMain =>
            (Set.empty, store) /* main cannot receive messages */
        }
      }
    }

    /* New context, created actors, effect, message sent, message received */
    type MacrostepState = (Option[Context], Set[(PID, Context)], ActorEffect, Option[M.Message], Option[M.Message])
    def macrostep(p: PID, store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time]):
        (Set[MacrostepState], GlobalStore) = {
      def loop(todo: Set[MacrostepState], visited: Set[MacrostepState], finals: Set[MacrostepState], store: GlobalStore):
          (Set[MacrostepState], GlobalStore) = {
        if (todo.isEmpty) {
          (finals, store)
        } else {
          val (next, newFinals, store2) = todo.foldLeft((Set[MacrostepState](), Set[MacrostepState](), store))((acc, s) => s match {
            case (None, _, _, _, _) => throw new RuntimeException("Terminated context in frontier of macrostep!")
            case st @ (Some(ctx), cr, eff, sent, recv) =>
              val (next, store2) = ctx.step(p, sem, acc._3)
              Logger.log(s"Stepping ${ctx.control}")
              Logger.log(s"next is: ${next.length}")
              if (next.isEmpty) {
                (acc._1, acc._2 + ((Some(ctx), cr, eff, sent, recv)), store)
              } else {
                val (newTodo, newFinals) = next.partition({
                  case (Some(ctx2), cr2, eff2, sent2, recv2) =>
                    !ctx2.halted && !(sent.isDefined && sent2.isDefined) && !(recv.isDefined && recv2.isDefined)
                  case (None, _, _, _, _) =>
                    false
                })
                (acc._1 ++ newTodo.map({
                  case (Some(ctx2), cr2, eff2, sent2, recv2) =>
                    (Some(ctx2), cr ++ cr2, ActorEffect.combine(eff, eff2), sent.orElse(sent2), recv.orElse(recv2))
                  case (None, _, _, _, _) => throw new RuntimeException("Should not happen")
                }),
                  acc._2 ++ newFinals.map({
                    case (ctx2, cr2, eff2, sent2, recv2) if !(sent.isDefined && sent2.isDefined) && !(recv.isDefined && recv2.isDefined) =>
                      (ctx2, cr ++ cr2, ActorEffect.combine(eff, eff2), sent.orElse(sent2), recv.orElse(recv2))
                    case (_, _, _, sent2, recv2) if (sent.isDefined && sent2.isDefined) || (recv.isDefined && recv2.isDefined) =>
                      (Some(ctx), cr, eff, sent, recv)
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
        case (next, store2) if !next.isEmpty =>
          loop(next, Set(), Set(), store2)
        case _ =>
          (Set(), store)
      }
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

  type ActorEffect = List[String]
  object ActorEffect {
    def empty: ActorEffect = List.empty
    def send(target: PID, name: String, args: List[Abs]): ActorEffect =
      List(if (args.isEmpty) { s"$target ! $name" } else {
        val argss = args.mkString(", ")
        s"$target ! $name $argss"
      })
    def terminate(p: PID): ActorEffect = List("x")
    def create(p: PID, name: String): ActorEffect = List(s"create $name")
    def become(p: PID, name: String): ActorEffect = List(s"become $name")
    def receive(p: PID, name: String, args: List[Abs]): ActorEffect =
      List(if (args.isEmpty) { s"? $name" } else {
        val argss = args.mkString(", ")
        s"$p ? $name $argss"
      })
    def combine(eff1: ActorEffect, eff2: ActorEffect): ActorEffect = eff1 ++ eff2
  }

  case class State(procs: Procs) {
    def toXml = procs.toXml
    def halted: Boolean = procs.forall((p, ctx) => ctx.halted)
    def hasError: Boolean = procs.exists((pid, ctx) => ctx.hasError)

    /**
     * Performs a macrostep for a given PID. If the state is stuck, returns
     * None. Otherwise, returns the graph explored for this macrostep, as well
     * as every final state and the effect that stopped the macrostep for that
     * state. The final states *are* in the graph (unlike macrostepTrace),
     * because we need to keep track of the edge. */
    /* TODO: commits done to the store here should not be the same kind of commit as from the general loop */
    def macrostepPid(p: PID, store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time]):
        Option[(Set[(State, ActorEffect)], GlobalStore)] = {
      val init: (Set[(State, ActorEffect)], GlobalStore) = (Set.empty, store)
      val res = procs.get(p).foldLeft(init)((acc, ctx) => {
        val (res, store2) = ctx.macrostep(p, acc._2, sem)
        val next = res.map({
          case (ctx2, created, eff, sent, recv) =>
            val withCtx = ctx2 match {
              case Some(newCtx) => this.copy(procs = procs.update(p -> newCtx))
                // TODO: could be improved by terminating only ctx?
              case None => this.copy(procs = procs.terminate(p))
            }
            val withCreated = if (created.isEmpty) { withCtx } else {
              withCtx.copy(procs = created.foldLeft(withCtx.procs)((procs, cr) => procs.extend(cr)))
            }
            /* precision could be improved by merging this with newCtx */
            val withMessage = sent match {
              case None => withCreated
              case Some(message) =>
                val ptarget = message._1
                withCreated.copy(procs =
                  withCreated.procs.get(ptarget).map(ctx =>
                    ctx.copy(mbox = ctx.mbox.push(message))).foldLeft(withCreated.procs)((procs, ctx) =>
                    procs.update(ptarget -> ctx)))
            }
            (withMessage, eff)
        })
        /* TODO: store should be reset between different contexts? */
        (acc._1 ++ next, store2)
      })
      if (res._1.isEmpty) {
        None
      } else {
        Some(res)
      }
    }
    def macrostepAll(store: GlobalStore, sem: Semantics[Exp, Abs, Addr, Time]):
        (Set[(Set[(State, ActorEffect)], PID)], GlobalStore) =
      procs.pids.foldLeft((Set[(Set[(State, ActorEffect)], PID)](), store))((acc, p) => {
        macrostepPid(p, acc._2, sem) match {
          case Some((states, store2)) => {
            (acc._1 + ((states, p)), store2)
          }
          case None =>
            acc
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
          case (p, eff) if eff.isEmpty => List(scala.xml.Text(p.toString))
          case (p, eff) => List(scala.xml.Text(p.toString), <font color="red">{eff.mkString(", ")}</font>)
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
      store: GlobalStore, graph: Option[G], graphs: Option[Map[PID, Graph[Set[Context], ActorEffect]]]): ActorsAAMOutput = {
      if (todo.isEmpty || Util.timeoutReached(timeout, startingTime)) {
        var i = 0;
        graphs.map(_.foreach({ case (p, g) =>
          i += 1
          println(s"$i is $p")
          g.toDotFile(s"subgraph-$i.dot", ctxs =>
            if (ctxs.isEmpty) { List(scala.xml.Text("x")) } else {
              ctxs.map(_.toXml).toList.reduceLeft((acc, l) => acc ++ (scala.xml.Text(", ") :: l))
            },
            ctxs => Colors.Yellow, eff =>
            if (eff.isEmpty) {
              List()
            } else {
              List(<font color="red">{eff.mkString(", ")}</font>)
            })
        }))
        ActorsAAMOutput(halted, reallyVisited.size, Util.timeElapsed(startingTime), graph, !todo.isEmpty)
      } else {
        val (edges, store2) = todo.foldLeft((Set[(State, (PID, ActorEffect), State)](), store))((acc, s) => {
          val (next, store2) = s.macrostepAll(acc._2.restore, sem)
          (acc._1 ++ next.flatMap({ case (ss, p) =>
            ss.map({ case (s2, eff) => (s, (p, eff), s2) })
          }), store2)
        })
        val newTodo = edges.map(_._3)
        val newGraph = graph.map(_.addEdges(edges))
        val newGraphs = graphs.map(gs =>
          edges.foldLeft(gs)((gs, edge) => edge match {
            case (s1, (p, eff), s2) =>
              (gs + (p -> gs(p).addEdge(s1.procs.get(p).map(_.trimMbox), eff, s2.procs.get(p).map(_.trimMbox))))
          }))
        if (store2.mainIsUnchanged) {
          loopMacrostep(newTodo.diff(visited), visited ++ todo, reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2, newGraph, newGraphs)
        } else {
          loopMacrostep(newTodo, Set(), reallyVisited ++ todo, halted ++ todo.filter(_.halted),
            store2.commitMain, newGraph, newGraphs)
        }
      }
    }
    val (initialState, store) = State.inject(exp, sem.initialEnv, sem.initialStore)
    val g = if (graph) { Some(G()) } else { None }
    val gs = if (graph) { Some(Map[PID, Graph[Set[Context], ActorEffect]]().withDefaultValue(new Graph[Set[Context], ActorEffect]())) } else { None }
    loopMacrostep(Set(initialState), Set(), Set(), Set(), store, g, gs)
  }
}
