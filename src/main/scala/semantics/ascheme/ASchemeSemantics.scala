import scalaz.Scalaz._
import scalaz._

class ASchemeSemantics[Abs : IsASchemeLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier](primitives: Primitives[Addr, Abs])
    extends SchemeSemantics[Abs, Addr, Time](primitives) {
  def aabs = implicitly[IsASchemeLattice[Abs]]
  def pid = implicitly[ThreadIdentifier[PID]]

  object ActorAction extends ActorActionHelpers[SchemeExp, Abs, Addr, Time, PID]
  type ActorDefinition = ActorAction.ActorDefinition

  case class FrameSendTarget(message: String, args: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameSend(message: String, target: Abs, argsv: List[Abs], args: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameCreate(argsv: List[Abs], args: List[SchemeExp], exp: SchemeExp, env: Env) extends SchemeFrame
  case class FrameBecome(argsv: List[Abs], args: List[SchemeExp], env: Env) extends SchemeFrame

  override def atomicEval(e: SchemeExp, env: Env, store: Sto): Option[(Abs, Set[Effect[Addr]])] = e match {
    case a: SchemeActor => Some((aabs.injectActor[SchemeExp, Addr](a, env), Set()))
    case _ => super.atomicEval(e, env, store)
  }

  override def stepEval(e: SchemeExp, env: Env, store: Sto, t: Time) = optimizeAtomic(e match {
    case a: SchemeActor => Action.value(aabs.injectActor[SchemeExp, Addr](a, env), store)
    case SchemeSend(target, message, args, _) => Action.push(FrameSendTarget(message, args, env), target, env, store)
    case SchemeCreate(beh, args, _) => Action.push(FrameCreate(List(), args, beh, env), beh, env, store)
    case SchemeBecome(beh, args, _) => Action.push(FrameBecome(List(), args, env), beh, env, store)
    case SchemeTerminate(_) => Action.none /* TODO: specific action to kill the actor? */
    case _ => super.stepEval(e, env, store, t)
  }, t)


  private def createActor(actorName: String, xs: List[String], defs: Map[String, (List[String], List[SchemeExp])],
    argsv: List[Abs], env: Environment[Addr], store: Store[Addr, Abs], t: Time): (Store[Addr, Abs], ActorDefinition) = {
    val (env2, store2) = bindArgs(xs.zip(argsv), env, store, t)
    def act(message: String, margsv: List[Abs], self: PID, sender: PID, store: Store[Addr, Abs], t2: Time): Action[SchemeExp, Abs, Addr] =
      defs.get(message) match {
        case Some((margs, body)) =>
          val (env3, store3) = bindArgs(margs.zip(margsv), env2, store, t2)
          val vself = aabs.injectPid(self)
          val aself = addr.variable("self", vself, t2)
          Action.eval(if (body.size == 1) { body.head } else { SchemeBegin(body, body.head.pos) },
            env3.extend("self", aself), store3.extend(aself, vself))
        case None =>
          Action.error(MessageNotSupported(actorName, message, defs.keys.toList))
      }
    (store2, act)
  }

  private def send(target: Abs, message: String, args: List[Abs]): Set[Action[SchemeExp, Abs, Addr]] = {
    val pids = aabs.getPids(target)
    if (pids.isEmpty) {
      Action.error(TypeError("send", "first operand", "pid value", s"non-pid value ($target)"))
    } else {
      pids.map(p => ActorAction.send(p, message, args, aabs.injectPid(p)))
    }
  }

  override def stepKont(v: Abs, frame: Frame, store: Sto, t: Time) = optimizeAtomic(frame match {
    case FrameSendTarget(message, List(), env) =>
      send(v, message, List())
    case FrameSendTarget(message, first :: rest, env) =>
      Action.push(FrameSend(message, v, List(), rest, env), first, env, store)
    case FrameSend(message, target, revargsv, List(), env) =>
      val argsv = (v :: revargsv).reverse
      send(target, message, (v :: revargsv).reverse)
    case FrameSend(message, target, argsv, first :: rest, env) =>
      Action.push(FrameSend(message, target, v :: argsv, rest, env), first, env, store)
    case FrameCreate(revargsv, List(), exp, env) =>
      val act :: argsv = (v :: revargsv).reverse
      val actors = aabs.getActors[SchemeExp, Addr](act)
      if (actors.isEmpty) {
        Action.error(TypeError("create", "first operand", "actor", s"non-actor value ($act)"))
      } else {
        actors.map({ case (SchemeActor(name, xs, defs, _), env) =>
          if (xs.size != argsv.size) {
            Action.error(ArityError("create actor", xs.size, argsv.size))
          } else {
            val (store2, actor) = createActor(name, xs, defs, argsv, env, store, t)
            ActorAction.create(actor, exp, aabs.injectPid _, store2)
          }
        })
      }
    case FrameCreate(argsv, first :: rest, exp, env) =>
      Action.push(FrameCreate(v :: argsv, rest, exp, env), first, env, store)
    case FrameBecome(revargsv, List(), env) =>
      val act :: argsv = (v :: revargsv).reverse
      val actors = aabs.getActors[SchemeExp, Addr](act)
      if (actors.isEmpty) {
        Action.error(TypeError("become", "first operand", "actor", s"non-actor value ($act)"))
      } else {
        actors.map({ case (SchemeActor(name, xs, defs, _), env) =>
          if (xs.size != argsv.size) {
            Action.error(ArityError("become behavior", xs.size, argsv.size))
          } else {
            val (store2, actor) = createActor(name, xs, defs, argsv, env, store, t)
            ActorAction.become(actor, aabs.inject(false), store2)
          }
        })
      }
    case FrameBecome(argsv, first :: rest, env) =>
      Action.push(FrameBecome(v :: argsv, rest, env), first, env, store)
    case _ => super.stepKont(v, frame, store, t)
  }, t)
}
