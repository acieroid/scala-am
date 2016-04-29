trait LamExp
case class Lam(x: String, e: LamExp) extends LamExp
case class App(e1: LamExp, e2: LamExp) extends LamExp
case class Var(x: String) extends LamExp

class LamSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends BaseSemantics[LamExp, Abs, Addr, Time] {
  type Env = Environment[Addr]
  type Sto = Store[Addr, Abs]
  trait ANFFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
  }
  case class FrameArg(e: LamExp, env: Env) extends ANFFrame
  case class FrameFun(v: Abs) extends ANFFrame

  def stepEval(e: LamExp, env: Env, store: Sto, t: Time) = e match {
    case Lam(_, _) => Set(ActionReachedValue(abs.inject((e, env)), store))
    case App(e1, e2) => Set(ActionPush(e1, FrameArg(e2, env), env, store))
    case Var(x) => env.lookup(x) match {
      case Some(a) => Set(ActionReachedValue(store.lookup(a), store))
      case None => Set(ActionError(s"unbound variable $x"))
    }
  }

  def stepKont(v: Abs, frame: Frame, store: Sto, t: Time) = frame match {
    case FrameArg(e, env) => Set(ActionPush(e, FrameFun(v), env, store))
    case FrameFun(fun) => abs.getClosures[LamExp, Addr](fun).map({
      case (Lam(x, e), env) => {
        val a = addr.variable(x, v, t)
        ActionEval(e, env.extend(x, a), store.extend(a, v))
      }
    })
  }

  def parse(program: String): LamExp = Lam("x", Var("x"))
}
