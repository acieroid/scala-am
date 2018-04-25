import scalaz.Scalaz._
import scalaz._

/*
 * TODO: copy structures (clo, vec, cons)
 * TODO: print state of the actor in wait nodes.
 * TODO: print name of the actor better
 * TODO: bring back annotations
 * TODO: just have to restart computation from wait states
 */
class ActorsModular[Exp : Expression, Abs : IsASchemeLattice, Addr : Address, Time : ActorTimestamp, PID : ThreadIdentifier](recordValues: Boolean)
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def name = "ActorsModular"
  var recordedBecome: Map[PID, Set[(String, List[Abs])]] = Map.empty.withDefaultValue(Set.empty)
  def recordBecome(pid: PID, name: String, args: List[Abs]): Unit = if (recordValues) {
    val existing = recordedBecome(pid)
    val subsumed = existing.exists({ case (name2, args2) =>
      name == name2 && args.zip(args2).forall({ case (v, v2) => name == name2 && JoinLattice[Abs].subsumes(v2, v) })
    })
    val updated = if (subsumed) { existing } else {
      existing.filter({ case (name2, args2) =>
        /* remove all subsumed by new element */
        !(name == name2 && args.zip(args2).forall({ case (v, v2) => JoinLattice[Abs].subsumes(v, v2) }))
      }) + ((name, args))
    }
    recordedBecome = recordedBecome + (pid -> updated)
  }

  var recordedCreate: Map[PID, Set[(String, List[Abs])]] = Map.empty.withDefaultValue(Set.empty)
  def recordCreate(pid: PID, name: String, args: List[Abs]): Unit = if (recordValues) {
    val existing = recordedCreate(pid)
    val subsumed = existing.exists({ case (name2, args2) =>
      name == name2 && args.zip(args2).forall({ case (v, v2) => JoinLattice[Abs].subsumes(v2, v) })
    })
    val updated = if (subsumed) { existing } else {
      existing.filter({ case (name2, args2) =>
        /* remove all subsumed by new element */
        !(name == name2 && args.zip(args2).forall({ case (v, v2) => JoinLattice[Abs].subsumes(v, v2) }))
      }) + ((name, args))
    }
    recordedCreate = recordedCreate + (pid -> updated)
  }

  var recordedReceive: Map[PID, Set[(String, List[Abs])]] = Map.empty.withDefaultValue(Set.empty)
  def recordReceive(pid: PID, name: String, args: List[Abs]): Unit = if (recordValues) {
    val existing = recordedReceive(pid)
    val subsumed = existing.exists({ case (name2, args2) =>
      name == name2 && args.zip(args2).forall({ case (v, v2) => JoinLattice[Abs].subsumes(v2, v) })
    })
    val updated = if (subsumed) { existing } else {
      existing.filter({ case (name2, args2) =>
        /* remove all subsumed by new element */
        !(name == name2 && args.zip(args2).forall({ case (v, v2) => JoinLattice[Abs].subsumes(v, v2) }))
      }) + ((name, args))
    }
    recordedReceive = recordedReceive + (pid -> updated)
  }

  type G = Graph[ActorState, Unit, Unit]
  implicit val graphNode = new GraphNode[ActorState, Unit] {
    override def labelXml(n: ActorState) = n.toXml
    override def color(n: ActorState) = if (n.hasError) {
      Colors.Red
    } else if (n.halted) {
      Colors.Yellow
    } else {
      Colors.White
    }
  }

  object G {
    def apply(): G = Graph.empty[ActorState, Unit, Unit]
    def apply(s: ActorState): G = Graph.node[ActorState, Unit, Unit](s)
  }

  trait KontAddr
  case class NormalKontAddress(pid: PID, exp: Exp, time: Time) extends KontAddr
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

  /* TODO */
  type Message = (PID, String, List[Abs])
  case class Mailbox(messages: Set[Message]) {
    override def toString = messages.mkString(" + ")
    def push(m: Message): Mailbox = this.copy(messages = messages + m)
    def pop: Set[Message] = messages
  }
  object Mailbox {
    def empty: Mailbox = Mailbox(Set.empty)
  }
  implicit val stateWithKey = new WithKey[ActorState] {
    type K = KontAddr
    def key(st: ActorState) = st.kont
  }
  case class ActorState(pid: PID, control: Control, kont: KontAddr, inst: ActorInstance, t: Time) {
    def toXml: List[scala.xml.Node] = (control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
      case ControlWait => List(<font color="skyblue">wait</font>)
    }) ++ List(<br/>, scala.xml.Text(inst match {
      case ActorInstanceMain => "main"
      case ActorInstanceActor(e, _) => e.toString.take(10)
    }))
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => inst == ActorInstanceMain && kont == HaltKontAddress
      case ControlError(_) => true
      case ControlWait => true
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }
    def extractArgs(actd: Exp, env: Env, sto: Sto): List[Abs] = actd match {
      case SchemeActor(name, xs, _, _) =>
        xs.map(x => env.lookup(x.name) match {
          case Some(a) => sto.lookupBot(a)
          case None => throw new Error("unbound variable: $x")
        })
    }
    def integrate(act: Act, store: GlobalStore):
        (
          /* Successor state (None if actor terminated) */
          Option[ActorState],
          /* Resulting store */
          GlobalStore,
          /* Potentially created actor */
          Option[ActorState],
          /* Potentially sent message */
          Option[Message]
        ) = act match {
      case ActionReachedValue(v, store2, effs) =>
        (Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta), Option.empty, Option.empty)
      case ActionPush(frame, e, env, store2, effs) =>
        val next = NormalKontAddress(pid, e, t)
        (Some(this.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta).push(next, Kont(frame, kont)),
          Option.empty, Option.empty)
      case ActionEval(e, env, store2, effs) =>
        (Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t))),
          store.includeDelta(store2.delta), Option.empty, Option.empty)
      case ActionStepIn(fexp, clo, e, env, store2, argsv, effs) =>
        (Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t, fexp))),
          store.includeDelta(store2.delta), Option.empty, Option.empty)
      case ActionError(err) =>
        (Some(this.copy(control = ControlError(err))),
          store, None, None)
      case ActorActionBecome(name, actd, args, env2, store2, vres, effs) =>
        recordBecome(pid, name, args)
        (Some(this.copy(control = ControlWait, inst = ActorInstanceActor(actd, env2), kont = HaltKontAddress, t =  ActorTimestamp[Time].actorBecome(t, actd))),
          store.includeDelta(store2.delta), None, None)
      case ActorActionTerminate(_) =>
        (None, store, None, None)
      case ActorActionCreate(name, actd, exp, args, env2, store2, fres : (PID => Abs), effs) =>
        recordCreate(pid, name, args)
        val p2 = ThreadIdentifier[PID].thread(t, name)
        (Some(this.copy(control = ControlKont(fres(p2)), t = ActorTimestamp[Time].actorCreated(t, p2))),
          store.includeDelta(store2.delta),
          Some(ActorState(p2, ControlWait, HaltKontAddress, ActorInstanceActor(actd, env2), Timestamp[Time].initial(""))),
          None)
      case ActorActionSend(ptarget : PID @unchecked, name, msg, vres, effs) =>
        (Some(this.copy(control = ControlKont(vres), t = ActorTimestamp[Time].messageSent(t, ptarget, name, msg))),
          store, None, Some((ptarget, name, msg)))
    }

    def step(sem: Semantics[Exp, Abs, Addr, Time], store: GlobalStore, mailbox: Mailbox):
        (
          /* Successor states */
          Set[ActorState],
          /* Created actors */
          Set[ActorState],
          /* Messages sent */
          Set[Message],
          /* Resulting store */
          GlobalStore
        ) = {
      val init: (Set[ActorState], Set[ActorState], Set[Message], GlobalStore) = (Set.empty, Set.empty, Set.empty, store)
      control match {
        case ControlEval(e, env) => sem.stepEval(e, env, store.store, t).foldLeft(init)((acc, action) =>
          integrate(action, acc._4) match { case (s, store2, created, sent) =>
            (acc._1 ++ s.toSet, acc._2 ++ created.toSet, acc._3 ++ sent.toSet, store2)
          })
        case ControlKont(v) if kont != HaltKontAddress =>
          store.kstore.lookup(kont).foldLeft(init)((acc, kont) => kont match {
            case Kont(frame, next) => sem.stepKont(v, frame, acc._4.store, t).foldLeft(acc)((acc, action) =>
              this.copy(kont = next).integrate(action, acc._4) match {
                case (s, store2, created, sent) => (acc._1 ++ s.toSet, acc._2 ++ created.toSet, acc._3 ++ sent.toSet, store2)
              })
          })
        case ControlKont(v) if kont == HaltKontAddress && inst != ActorInstanceMain => /* TODO */
          (Set(this.copy(control = ControlWait, t = Timestamp[Time].tick(t))), Set.empty, Set.empty, store)
        case ControlKont(v) if kont == HaltKontAddress && inst == ActorInstanceMain =>
          (Set.empty, Set.empty, Set.empty, store)
        case ControlError(_) => (Set.empty, Set.empty, Set.empty, store)
        case ControlWait => inst match {
          case ActorInstanceActor(actd, env) =>
            /* TODO: pop from mailbox */
            mailbox.pop.foldLeft(init)((acc, m) => m match {
              case message @ (sender, name, values) =>
                recordReceive(pid, name, values)
                sem.stepReceive(pid, name, values, actd, env, acc._4.store, ActorTimestamp[Time].messageReception(t, sender, name, values)).foldLeft(acc)((acc, action) =>
                  integrate(action, acc._4) match {
                    case (s, store2, created, sent) =>
                      (acc._1 ++ s.toSet, acc._2 ++ created.toSet, acc._3 ++ sent.toSet, store2)
                  })
            })
          case ActorInstanceMain =>
            (Set.empty, Set.empty, Set.empty, store) /* main cannot receive messages */
        }
      }
    }
  }
  case class ActorModularOutput(time: Double, graphs: Map[PID, Option[G]], timedOut: Boolean) extends Output {
    def numberOfStates = 0
    def finalValues: Set[Abs] = Set()
    def toFile(path: String)(output: GraphOutput) =
      graphs.foreach({
        case (k, Some(g)) => output.toFile(g, ())(path + "-" + k.toString + ".dot")
        case (_, None) => ()
      })
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    type WL[A] = Set[A]
    type VS[A] = KeyMap[A]
    val startingTime = System.nanoTime
    case class InnerLoopState(
      /* Which PID this corresponds to */
      pid: PID,
      /* What's still to visit */
      todo: WL[ActorState],
      /* Set of seen states (can be cleared due to global store) */
      visited: VS[ActorState],
      /* Set of seen states that cannot be cleared (only used to count number of actual states visited) */
      reallyVisited: VS[ActorState],
      /* The graph computed */
      graph: Option[G],
      /* Actors created */
      created: Set[ActorState],
      /* Messages sent */
      sent: Set[Message]
    )
    implicit val innerLoopStateWithKey = new WithKey[InnerLoopState] {
      type K = PID
      def key(st: InnerLoopState) = st.pid
    }
    @scala.annotation.tailrec
    def innerLoop(st: InnerLoopState, mailbox: Mailbox, store: GlobalStore):
        (/* "Final" state */ InnerLoopState, /* Resulting store */ GlobalStore) =
      if (st.todo.isEmpty || timeout.reached) {
        (st, store)
      } else {
        val (edges, store2, created, sent) = st.todo.foldLeft((Set[(ActorState, Unit, ActorState)](), store, Set[ActorState](), Set[Message]()))((acc, state) =>
          state.step(sem, acc._2, mailbox) match {
            case (succs, created, sent, store2) =>
              (acc._1 ++ succs.map(state2 => (state, (), state2)), store2, acc._3 ++ created, acc._4 ++ sent)
          })
        val newTodo = edges.map(_._3)
        val newGraph = st.graph.map(_.addEdges(edges))
        if (store2.mainIsUnchanged) {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            visited = VisitedSet[VS].append(st.visited, st.todo),
            reallyVisited = VisitedSet[VS].append(st.reallyVisited, st.todo),
            graph = newGraph,
            sent = st.sent ++ sent,
            created = st.created ++ created
          ), mailbox, store2)
        } else {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            visited = VisitedSet[VS].empty,
            reallyVisited = VisitedSet[VS].append(st.reallyVisited, st.todo),
            graph = newGraph,
            sent = st.sent ++ sent,
            created = st.created ++ created), mailbox, store2.commitMain)
        }
      }
    case class OuterLoopState(
      /* What's still to visit */
      todo: WL[InnerLoopState],
      /* Which inner loop states corresponds to which pid */
      pids: Map[PID, Set[InnerLoopState]],
      /* Which mailbox corresponds to which pid */
      mailboxes: Map[PID, Mailbox],
      /* Global store */
      store: GlobalStore,
      /* Graphs for each actor */
      graphs: Map[PID, Option[G]]
    )
    def fromCreated(created: Set[ActorState], pids: Map[PID, Set[InnerLoopState]]):
        (Set[InnerLoopState], Map[PID, Set[InnerLoopState]]) =
      created.foldLeft((Set[InnerLoopState](), pids))((acc, st) => {
        if (acc._2(st.pid).exists((inner: InnerLoopState) => inner.todo.contains(st))) {
          /* already spawned this actor, nothing changed */
          acc
        } else {
          val inner = InnerLoopState(st.pid, Set(st), VisitedSet[VS].empty, VisitedSet[VS].empty, Option(G()), Set.empty, Set.empty)
          (acc._1 + inner, acc._2 + (st.pid -> (acc._2(st.pid) + inner)))
        }
      })
    def fromSent(sent: Set[Message], pids: Map[PID, Set[InnerLoopState]], mailboxes: Map[PID, Mailbox]): (Set[InnerLoopState], Map[PID, Mailbox]) =
      sent.foldLeft((Set[InnerLoopState](), mailboxes))((acc, m) => {
        // was: (acc._1 ++ pids(m._1), acc._2 + (m._1 -> (acc._2(m._1).push(m))))
        val oldmbox = acc._2(m._1)
        val newmbox = oldmbox.push(m)
        if (newmbox == oldmbox) {
          /* nothing changed */
          acc
        } else {
          (acc._1 ++ pids(m._1), acc._2 + (m._1 -> newmbox))
        }
      })

    @scala.annotation.tailrec
    def outerLoop(st: OuterLoopState, iteration: Int): Output = {
      // println("---------------")
      // println(s"Iteration: $iteration")
      if (st.todo.isEmpty || timeout.reached) {
        /* println("Final mailboxes:")
        st.mailboxes.foreach({ case (k, v) =>
          println(s"$k -> $v")
        })
        println(s"Number of iterations: $iteration") */
        new ActorModularOutput(timeout.time, st.graphs, timeout.reached)
      } else {
        val succ = st.todo.foldLeft(Set[InnerLoopState](), st.pids, st.mailboxes, st.store, st.graphs)((acc, actorState) => {
          // println(s"Exploring actor ${actorState.pid} with mailbox ${st.mailboxes(actorState.pid)}")
          val (ist, store2) = innerLoop(actorState, st.mailboxes(actorState.pid), acc._4)
          val (todoCreated, pidsCreated) = fromCreated(ist.created, acc._2)
          // println(s"Created pids: ${ist.created.map(_.pid)}")
          val (todoSent, mailboxesSent) = fromSent(ist.sent, acc._2, acc._3)
          // println(s"Messages sent: ${ist.sent.map(x => (x._1, x._2))}")
          (acc._1 ++ todoCreated ++ todoSent, pidsCreated, mailboxesSent, store2, acc._5 + (ist.pid -> ist.graph))
        })
        val newOuter = OuterLoopState(succ._1, succ._2, succ._3, succ._4, succ._5)
        newOuter.graphs.foreach({
          case (k, Some(g)) => () // GraphDOTOutput.toFile(g, ())(s"iteration-$iteration-$k.dot")
          case (_, None) => ()
        })
        if (newOuter.mailboxes == st.mailboxes && newOuter.pids == st.pids) {
          /* if it didn't change, we skip the todos */
          outerLoop(newOuter.copy(todo = Set.empty), iteration+1)
        } else {
          outerLoop(newOuter, iteration+1)
        }
      }
    }
    val mainPid = ThreadIdentifier[PID].initial
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): (ActorState, GlobalStore) = {
      val initEnv = Environment.initial[Addr](env)
      (ActorState(mainPid, ControlEval(exp, initEnv), HaltKontAddress, ActorInstanceMain, Timestamp[Time].initial("")),
        GlobalStore.initial(store))
    }
    val (initialState, store) = inject(exp, sem.initialEnv, sem.initialStore)
    val initialInner = InnerLoopState(initialState.pid, Set(initialState), VisitedSet[VS].empty, VisitedSet[VS].empty, Option(G()), Set.empty, Set.empty)
    val res = outerLoop(OuterLoopState(Set(initialInner),
      Map[PID, Set[InnerLoopState]]().withDefaultValue(Set.empty) + (mainPid -> Set(initialInner)),
      Map[PID, Mailbox]().withDefaultValue(Mailbox.empty),
      store,
      Map[PID, Option[G]]().withDefaultValue(Option(G()))), 0)

    reportRecorded()
    res
  }
  def reportRecorded(): Unit = {
    def surround(x: String) = "\"" + x + "\""
    val keys = recordedCreate.keySet ++ recordedBecome.keySet ++ recordedReceive.keySet
    val keysstr = keys.mkString(", ")
    println(s"${keys.size} actor types: $keysstr")
    println("=========")
    keys.foreach(k => {
      val created = recordedCreate(k).map({
        case (name, args) =>
          val argsstr = args.mkString(" ")
          s"(create $name ($argsstr))"
      }).mkString(" ")
      val become = recordedBecome(k).map({
        case (name, args) =>
          val argsstr = args.mkString(" ")
          s"(become $name ($argsstr))"
      }).mkString(" ")
      val received = recordedReceive(k).map({
        case (name, args) =>
          val argsstr = args.mkString(" ")
          s"(received $name ($argsstr))"
      }).mkString(" ")
      println(s"(${k.toString} ($created $become $received))")
    })
  }

}