import scalaz.Scalaz._
import scalaz._

class ActorsAAM[Exp : Expression, Abs : IsASchemeLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
  def pid = implicitly[ThreadIdentifier[PID]]

  def name = "ActorsAAM"

  trait KontAddr
  case class NormalKontAddress(pid: PID, exp: Exp, time: Time) extends KontAddr
  case object HaltKontAddress extends KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  type Act = Action[Exp, Abs, Addr]
  type Beh = (List[Abs], PID, PID, Store[Addr, Abs], Time) => Act
  trait Behavior
  case class ActorBehavior(beh: Beh) extends Behavior
  case object MainBehavior extends Behavior

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

  type Message = (PID, List[Abs])
  case class Mbox(messages: Set[Message]) {
    def pop: Set[(Message, Mbox)] = messages.map(m => (m, this))
    def push(m: Message): Mbox = this.copy(messages = messages + m)
    def isEmpty: Boolean = messages.isEmpty
  }
  object Mbox {
    def empty: Mbox = Mbox(Set[Message]())
  }

  case class Context(control: Control, kont: KontAddr, beh: Behavior, mbox: Mbox) {
    def toXml: List[scala.xml.Node] = control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
      case ControlWait => List(<font color="skyblue">wait</font>)
    }
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => beh == MainBehavior && kont == HaltKontAddress
      case ControlError(_) => true
      case ControlWait => mbox.isEmpty
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }
  }
  object Context {
    def create(p: PID, beh: Beh): Context =
      Context(ControlWait, HaltKontAddress, ActorBehavior(beh), Mbox.empty)
  }

  /* TODO: add counting */
  case class Procs(content: Map[PID, Set[Context]]) {
    /* TODO: adjust to multiple contexts */
    def toXml: List[scala.xml.Node] = content.keySet.toList.map(p =>
      scala.xml.Text(s"$p: ") :: content(p).head.toXml).reduceLeft({ (acc, l) => acc ++ (<br/> :: l) })
    def get(p: PID): Set[Context] = content(p)
    def update(v: (PID, Context)): Procs = {
      assert(content(v._1).size <= 1) /* TODO: can remove this assertion when we have counting, and add strong updates */
      Procs(content = content + (v._1 -> Set(v._2)))
    }
    def extend(v: (PID, Context)): Procs = {
      assert(get(v._1).size == 0) /* TODO */
      Procs(content = content + (v._1 -> (get(v._1) + v._2)))
    }
    def pids: Set[PID] = content.keySet
    def exists(p: (PID, Context) => Boolean): Boolean = content.exists({ case (pid, ctxs) => ctxs.exists(ctx => p(pid, ctx)) })
    def forall(p: (PID, Context) => Boolean): Boolean = content.forall({ case (pid, ctxs) => ctxs.forall(ctx => p(pid, ctx)) })
  }
  object Procs {
    def empty: Procs = Procs(Map[PID, Set[Context]]().withDefaultValue(Set[Context]()))
  }

  val t0 = time.initial("dummy") /* TODO: add timestamping */

  trait ActorEffect
  case class ActorEffectSend(target: PID) extends ActorEffect
  case class ActorEffectSendSelf(target: PID) extends ActorEffect

  case class State(procs: Procs, store: Store[Addr, Abs], kstore: KontStore[KontAddr]) {
    def toXml = procs.toXml
    def halted: Boolean = procs.forall((p, ctx) => ctx.halted)
    def hasError: Boolean = procs.exists((pid, ctx) => ctx.hasError)
    def stepAll(sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = stepPids(procs.pids, sem)
    def stepPids(pids: Set[PID], sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = pids.flatMap(p => stepPid(p, sem))
    def stepAllExceptPid(p: PID, sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = stepPids(procs.pids - p, sem)

    def integrate(p: PID, ctx: Context, act: Act): (State, PID, Option[ActorEffect]) = act match {
      case ActionReachedValue(v, store2, effs) =>
        (this.copy(procs = procs.update(p, ctx.copy(control = ControlKont(v))),
          store = store2), p, None)
      case ActionPush(frame, e, env, store2, effs) =>
        val next = NormalKontAddress(p, e, t0)
        (this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), kont = next)),
          kstore = kstore.extend(next, Kont(frame, ctx.kont)), store = store2), p, None)
      case ActionEval(e, env, store2, effs) =>
        (this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env))),
          store = store2), p, None)
      case ActionStepIn(fexp, _, e, env, store2, _, effs) =>
        (this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env))),
          store = store2), p, None)
      case ActionError(err) =>
        (this.copy(procs = procs.update(p, ctx.copy(control = ControlError(err)))), p, None)
      case ActorActionSend(ptarget : PID @unchecked, msg, vres, effs) if ptarget != p =>
        val ctxtarget = procs.get(ptarget).head /* TODO: map */
        (this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(vres)))
          .update(ptarget -> ctxtarget.copy(mbox = ctxtarget.mbox.push(p -> msg)))),
          p, Some(ActorEffectSend(ptarget)))
      case ActorActionSend(ptarget, msg, vres, effs) if ptarget == p => /* TODO: special care need to be taken if p maps to more than a single actor */
        (this.copy(procs = procs.update(p -> ctx.copy(control = ControlKont(vres), mbox = ctx.mbox.push(p -> msg)))),
          p, Some(ActorEffectSendSelf(p)))
      case ActorActionCreate(beh : Beh @unchecked, exp, fres : (PID => Abs), effs) =>
        val p2 = pid.thread(exp, t0)
        (this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(fres(p2))))
          .extend(p2 -> Context.create(p2, beh))), p, None)
      case ActorActionBecome(beh2 : Beh @unchecked, vres, effs) =>
        (this.copy(procs = procs.update(p -> ctx.copy(control = ControlKont(vres), beh = ActorBehavior(beh2)))), p, None)
    }

    def stepPid(p: PID, sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID, Option[ActorEffect])] = procs.get(p).flatMap(ctx => ctx.control match {
      case ControlEval(e, env) => /* call semantics */
        sem.stepEval(e, env, store, t0).map(action => integrate(p, ctx, action))
      case ControlKont(v) if ctx.kont != HaltKontAddress => /* apply continuation */
        kstore.lookup(ctx.kont).flatMap({
          case Kont(frame, next) => sem.stepKont(v, frame, store, t0).map(action => integrate(p, ctx.copy(kont = next), action))
        })
      case ControlKont(v) if ctx.kont == HaltKontAddress && ctx.beh != MainBehavior => /* go to wait */
        Set[(State, PID, Option[ActorEffect])]((this.copy(procs = procs.update(p -> ctx.copy(control = ControlWait))), p, None))
      case ControlKont(v) if ctx.kont == HaltKontAddress && ctx.beh == MainBehavior =>
        Set[(State, PID, Option[ActorEffect])]() /* main is stuck at this point */
      case ControlError(_) => Set[(State, PID, Option[ActorEffect])]() /* no successor */
      case ControlWait => /* receive a message */
        ctx.beh match {
          case ActorBehavior(beh) =>
            ctx.mbox.pop.map({ case ((sender, values), mbox2) =>
              integrate(p, ctx.copy(mbox = mbox2), beh(values, p, sender, store, t0))
            })
          case MainBehavior => Set[(State, PID, Option[ActorEffect])]() /* main cannot receive messages */
        }
    })
  }

  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) =
      State(
        Procs.empty.extend(pid.initial -> Context(ControlEval(exp, Environment.initial[Addr](env)), HaltKontAddress, MainBehavior, Mbox.empty)),
        Store.initial[Addr, Abs](store),
        KontStore.empty[KontAddr])
  }

  case class ActorsAAMOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[Graph[State, PID]], timedOut: Boolean)
      extends Output[Abs] {
    def finalValues: Set[Abs] = halted.flatMap(st => st.procs.get(pid.initial).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def containsFinalValue(v: Abs): Boolean = finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toXml,
        (s) => if (s.hasError) {
          Colors.Red
        } else if (halted.contains(s)) {
          Colors.Yellow
        } else {
          Colors.White
        }, p => List(scala.xml.Text(p.toString)))
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
            case Some(state2) =>
              println(state == state2)
              println(state)
              println("===")
              println(state2)
            case None => println(s"Graph doesn't contain state ${state2Number}")
          }
          case Failure(e) => println(s"Cannot parse state number ($s): $e")
        }
        case v => println(s"Unknown inspection query on $stateNumber: $query")
      }
      case None => println(s"Graph was either not generated, or doesn't contain state $stateNumber. I cannot query it")
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] = {
    val startingTime = System.nanoTime
    @scala.annotation.tailrec
    def loopAllInterleavings(todo: Set[State], visited: Set[State], halted: Set[State], graph: Option[Graph[State, PID]]): ActorsAAMOutput = {
      if (Util.timeoutReached(timeout, startingTime)) {
        ActorsAAMOutput(halted, visited.size, Util.timeElapsed(startingTime), graph, true)
      } else {
        todo.headOption match {
          case Some(s) =>
            // graph.foreach(g => println(s"State ${g.nodeId(s)}"))
            if (visited.contains(s)) {
              loopAllInterleavings(todo.tail, visited, halted, graph)
            } else if (s.halted) {
              loopAllInterleavings(todo.tail, visited + s, halted + s, graph)
            } else {
              val succs: Set[(State, PID, Option[ActorEffect])] = s.stepAll(sem)
              val newGraph = graph.map(_.addEdges(succs.map({ case (s2, pid, _) => (s, pid, s2) })))
              loopAllInterleavings(todo.tail ++ succs.map(_._1), visited + s, halted, newGraph)
            }
          case None =>
            ActorsAAMOutput(halted, visited.size, Util.timeElapsed(startingTime), graph, false)
        }
      }
    }
    @scala.annotation.tailrec
    def loopSendInterleavings(todo: Set[(State, PID)], visited: Set[(State, PID)], halted: Set[State], graph: Option[Graph[State, PID]]): ActorsAAMOutput = {
      if (Util.timeoutReached(timeout, startingTime)) {
        ActorsAAMOutput(halted, visited.size, Util.timeElapsed(startingTime), graph, true)
      } else {
        todo.headOption match {
          case Some((s, p)) =>
            if (visited.contains((s, p))) {
              loopSendInterleavings(todo.tail, visited, halted, graph)
            } else if (s.halted) {
              loopSendInterleavings(todo.tail, visited + ((s, p)), halted + s, graph)
            } else {

              // TODO: problem with loops, we might keep running the same process forever, since messages might not be removed from the mailbox in the abstract
              // 1: Step only the current pid
              val succs: Set[(State, PID, Option[ActorEffect])] = s.stepPid(p, sem)
              // 2: If we sent a message, we need to explore other pids as well
              val send: Boolean = succs.exists({ case (_, _, eff) => eff match {
                case Some(_: ActorEffectSend) => true
                case Some(_: ActorEffectSendSelf) => true
                case _ => false
              }})
              val succs2: Set[(State, PID, Option[ActorEffect])] = if (!send) { succs } else {
                succs ++ s.stepAllExceptPid(p, sem)
              }
              val succs3: Set[(State, PID, Option[ActorEffect])] = if (!succs2.isEmpty) { succs2 } else {
                // 3: if there is no successor at all, we may need to explore more pids. This basically applies when no message was sent (and succs == succs2 == empty set)
                s.stepAllExceptPid(p, sem)
              }
              // 4: Compute the pids we explored
              val pids: Set[PID] = succs3.map({ case (_, p, _) => p })
              // 5: Add new stuff to the graph
              val newGraph = graph.map(_.addEdges(succs3.map({ case (s2, p, _) => (s, p, s2) })))
              // 6: Explore the remainder + new states
              loopSendInterleavings(todo.tail ++ succs3.map({ case (s2, p, _) => (s2, p) }), visited ++ pids.map(p => (s, p)), halted, newGraph)
            }
          case None =>
            ActorsAAMOutput(halted, visited.size, Util.timeElapsed(startingTime), graph, false)
        }
      }
    }
    val initialState = State.inject(exp, sem.initialEnv, sem.initialStore)
    // loopAllInterleavings(Set(initialState), Set(), Set(), if (graph) { Some(new Graph[State, PID]()) } else { None })
    loopSendInterleavings(Set((initialState, pid.initial)), Set(), Set(), if (graph) { Some(new Graph[State, PID]()) } else { None })
  }
}
