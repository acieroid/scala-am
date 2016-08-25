import scalaz.Scalaz._
import scalaz._

class ASchemeSemantics[Abs : IsASchemeLattice, Addr : Address, Time : Timestamp, PID : ThreadIdentifier](primitives: Primitives[Addr, Abs])
    extends SchemeSemantics[Abs, Addr, Time](primitives) {
  def aabs = implicitly[IsASchemeLattice[Abs]]
  def pid = implicitly[ThreadIdentifier[PID]]

  object ActorAction extends ActorActionHelpers[SchemeExp, Abs, Addr, Time, PID]

  case class FrameSend(argsv: List[Abs], args: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameCreate(argsv: List[Abs], args: List[SchemeExp], exp: SchemeExp, env: Env) extends SchemeFrame
  case class FrameBecome(argsv: List[Abs], args: List[SchemeExp], env: Env) extends SchemeFrame

  override def atomicEval(e: SchemeExp, env: Env, store: Sto): Option[(Abs, Set[Effect[Addr]])] = e match {
    case b: SchemeBehavior => Some((aabs.injectBehavior[SchemeExp, Addr](b, env), Set()))
    case _ => super.atomicEval(e, env, store)
  }

  /* TODO: add calls to optimizeAtomic? */
  override def stepEval(e: SchemeExp, env: Env, store: Sto, t: Time) = optimizeAtomic(e match {
    case b: SchemeBehavior => Action.value(aabs.injectBehavior[SchemeExp, Addr](b, env), store)
    case SchemeSend(target, args, _) => Action.push(FrameSend(List(), args, env), target, env, store)
    case SchemeCreate(beh, args, _) => Action.push(FrameCreate(List(), args, beh, env), beh, env, store)
    case SchemeBecome(beh, args, _) => Action.push(FrameBecome(List(), args, env), beh, env, store)
    case _ => super.stepEval(e, env, store, t)
  }, t)

  private def createBeh(xs: List[String], ys: List[String], env: Environment[Addr], body: List[SchemeExp], argsv: List[Abs], t: Time):
      ((List[Abs], PID, PID, Store[Addr, Abs], Time) => Action[SchemeExp, Abs, Addr]) =
    (yvals: List[Abs], self: PID, sender: PID, store: Store[Addr, Abs], t2: Time) => {
      if (ys.size != yvals.size) {
        Action.error(ArityError("instanciate behavior", ys.size, yvals.size))
      } else {
        val (env2, store2) = bindArgs(xs.zip(argsv), env, store, t)
        val (env3, store3) = bindArgs(ys.zip(yvals), env2, store2, t2)
        val (vself, vsender) = (aabs.injectPid(self), aabs.injectPid(sender))
        val (aself, asender) = (addr.variable("self", vself, t2), addr.variable("sender", vsender, t2))
        Action.eval(if (body.size == 1) { body.head } else { SchemeBegin(body, body.head.pos) },
          env3.extend("self", aself).extend("sender", asender),
          store3.extend(aself, vself).extend(asender, vsender))
      }
    }
  override def stepKont(v: Abs, frame: Frame, store: Sto, t: Time) = optimizeAtomic(frame match {
    case FrameSend(revargsv, List(), env) =>
      val target :: argsv = (v :: revargsv).reverse
      val pids = aabs.getPids(target)
      if (pids.isEmpty) {
        Action.error(TypeError("send", "first operand", "pid value", s"non-pid value ($target)"))
      } else {
        pids.map(p => ActorAction.send(p, argsv, Action.value(aabs.injectPid(p), store)))
      }
    case FrameSend(argsv, first :: rest, env) =>
      Action.push(FrameSend(v :: argsv, rest, env), first, env, store)
    case FrameCreate(revargsv, List(), exp, env) =>
      val beh :: argsv = (v :: revargsv).reverse
      val behs = aabs.getBehaviors[SchemeExp, Addr](beh)
      if (behs.isEmpty) {
        Action.error(TypeError("create", "first operand", "behavior", s"non-behavior value ($beh)"))
      } else {
        behs.map({ case (SchemeBehavior(xs, ys, body, _), env) =>
          if (xs.size != argsv.size) {
            Action.error(ArityError("create behavior", xs.size, argsv.size))
          } else {
            ActorAction.create(createBeh(xs, ys, env, body, argsv, t), exp)
          }
        })
      }
    case FrameCreate(argsv, first :: rest, exp, env) =>
      Action.push(FrameCreate(v :: argsv, rest, exp, env), first, env, store)
    case FrameBecome(revargsv, List(), env) =>
      val beh :: argsv = (v :: revargsv).reverse
      val behs = aabs.getBehaviors[SchemeExp, Addr](beh)
      if (behs.isEmpty) {
        Action.error(TypeError("become", "first operand", "behavior", s"non-behavior value ($beh)"))
      } else {
        behs.map({ case (SchemeBehavior(xs, ys, body, _), env) =>
          if (xs.size != argsv.size) {
            Action.error(ArityError("become behavior", xs.size, argsv.size))
          } else {
            ActorAction.become(createBeh(xs, ys, env, body, argsv, t))
          }
        })
      }
    case FrameBecome(argsv, first :: rest, env) =>
      Action.push(FrameBecome(v :: argsv, rest, env), first, env, store)
    case _ => super.stepKont(v, frame, store, t)
  }, t)
}
