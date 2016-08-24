import scalaz.Scalaz._
import scalaz._

class ActorsAAM[Exp : Expression, Abs : IsASchemeLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def abs = implicitly[JoinLattice[Abs]]
  def sabs = implicitly[IsSchemeLattice[Abs]] /* TODO: machine should be independent of Scheme and AScheme lattices */
  def aabs = implicitly[IsASchemeLattice[Abs]]
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
  type Beh = (List[Abs], Store[Addr, Abs], Time) => Act
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
  }
  object Mbox {
    def empty: Mbox = Mbox(Set[Message]())
  }

  case class Context(control: Control, kont: KontAddr, beh: Behavior, mbox: Mbox) {
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => beh == MainBehavior && kont == HaltKontAddress
      case ControlError(_) => true
      case ControlWait => beh == MainBehavior
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
    def exists(p: (PID, Set[Context]) => Boolean): Boolean = content.exists({ case (pid, ctxs) => p(pid, ctxs) })
  }
  object Procs {
    def empty: Procs = Procs(Map[PID, Set[Context]]().withDefaultValue(Set[Context]()))
  }

  val t0 = time.initial("dummy") /* TODO: add timestamping */


  case class State(procs: Procs, store: Store[Addr, Abs], kstore: KontStore[KontAddr]) {
    def halted: Boolean = procs.get(pid.initial).forall(_.halted)
    def hasError: Boolean = procs.exists((pid, ctxs) => ctxs.exists(_.hasError))
    def stepAll(sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID)] = stepPids(procs.pids, sem)
    def stepPids(pids: Set[PID], sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID)] = pids.flatMap(p => stepPid(p, sem))

    def integrate(p: PID, ctx: Context, act: Act): State = act match {
      case ActionReachedValue(v, store2, effs) =>
        this.copy(procs = procs.update(p, ctx.copy(control = ControlKont(v))),
          store = store2)
      case ActionPush(frame, e, env, store2, effs) => {
        val next = NormalKontAddress(p, e, t0)
        this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env), kont = next)),
          kstore = kstore.extend(next, Kont(frame, ctx.kont)), store = store2)
      }
      case ActionEval(e, env, store2, effs) =>
        this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env))),
          store = store2)
      case ActionStepIn(fexp, _, e, env, store2, _, effs) =>
        this.copy(procs = procs.update(p, ctx.copy(control = ControlEval(e, env))),
          store = store2)
      case ActionError(err) =>
        this.copy(procs = procs.update(p, ctx.copy(control = ControlError(err))))
      case ActorActionSend(ptarget : PID @unchecked, msg, act2, effs) if ptarget != p => {
        val ctxtarget = procs.get(ptarget).head /* TODO: map */
        this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(sabs.inject(false))))
          .extend(ptarget -> ctxtarget.copy(mbox = ctxtarget.mbox.push(p -> msg))))
      }
      case ActorActionSend(ptarget, msg, act2, effs) if ptarget == p => /* TODO: special care need to be taken if p maps to more than a single actor */
        this.copy(procs = procs.update(p -> ctx.copy(control = ControlKont(sabs.inject(false)), mbox = ctx.mbox.push(p -> msg))))
      case ActorActionCreate(beh : Beh @unchecked, exp, effs) => {
        val p2 = pid.thread(exp, t0)
        this.copy(procs = procs
          .update(p -> ctx.copy(control = ControlKont(aabs.injectPid(p2))))
          .extend(p -> Context.create(p2, beh)))
      }
      case ActorActionBecome(beh2 : Beh @unchecked, effs) =>
        this.copy(procs = procs.update(p -> ctx.copy(control = ControlKont(sabs.inject(false)), beh = ActorBehavior(beh2))))
    }
    def stepPid(p: PID, sem: Semantics[Exp, Abs, Addr, Time]): Set[(State, PID)] = procs.get(p).flatMap(ctx => ctx.control match {
      case ControlEval(e, env) => /* call semantics */
        sem.stepEval(e, env, store, t0).map(action => (integrate(p, ctx, action), p))
      case ControlKont(v) if ctx.halted && ctx.beh != MainBehavior => /* apply continuation if exists, otherwise go to wait */
        kstore.lookup(ctx.kont).flatMap({
          case Kont(frame, next) => sem.stepKont(v, frame, store, t0).map(action => (integrate(p, ctx.copy(kont = next), action), p))
        })
      case ControlError(_) => Set[(State, PID)]() /* no successor */
      case ControlWait => /* receive a message */
        ctx.beh match {
          case ActorBehavior(beh) =>
            ctx.mbox.pop.map({ case ((sender, values), mbox2) => /* TODO: bind sender and self */
              (integrate(p, ctx.copy(mbox = mbox2), beh(values, store, t0)), p)
            })
          case MainBehavior => Set[(State, PID)]() /* main cannot receive messages */
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
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString.take(40))),
        (s) => if (halted.contains(s)) {
          Colors.Yellow
        } else if (s.hasError) {
          Colors.Red
        } else {
          Colors.White
        }, p => List(scala.xml.Text(p.toString)))
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] = {
    val startingTime = System.nanoTime
    def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Option[Graph[State, PID]]): ActorsAAMOutput = {
      if (timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
        ActorsAAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, true)
      } else {
        todo.headOption match {
          case Some(s) =>
            if (visited.contains(s)) {
              loop(todo.tail, visited, halted, graph)
            } else if (s.halted) {
              loop(todo.tail, visited + s, halted + s, graph)
            } else {
              val succs: Set[(State, PID)] = s.stepAll(sem)
              val newGraph = graph.map(_.addEdges(succs.map({ case (s2, pid) => (s, pid, s2) })))
              loop(todo.tail ++ succs.map(_._1), visited + s, halted, newGraph)
            }
          case None =>
            ActorsAAMOutput(halted, visited.size, (System.nanoTime - startingTime) / Math.pow(10, 9), graph, false)
        }
      }
    }
    loop(Set(State.inject(exp, sem.initialEnv, sem.initialStore)), Set(), Set(), if (graph) { Some(new Graph[State, PID]()) } else { None })
  }
}
