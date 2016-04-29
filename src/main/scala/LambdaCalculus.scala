/* We show here how to add support for a new language. We do it for lambda calculus */

/** A lambda calculus expression is represented by a LamExp */
trait LamExp
/** An abstraction: lambda x. e */
case class Lam(x: String, e: LamExp) extends LamExp
/** An application: (e1 e2) */
case class App(e1: LamExp, e2: LamExp) extends LamExp
/** A variable reference: x */
case class Var(x: String) extends LamExp

/** Our value domain should form a lattice, but we need support for a bit more than just join operations */
trait LamLattice[L] extends JoinLattice[L] {
  /** We can inject a closure inside the value domain */
  def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L
  /** We can get the closures out of an element of the value domain */
  def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])]
}

/** This defines the semantics of call-by-value lambda expressions */
class LamSemantics[Abs : LamLattice, Addr : Address, Time : Timestamp]
    extends BaseSemantics[LamExp, Abs, Addr, Time] {
  /** We inherit the value abs that is bound to a JoinLattice[Abs], but we need
    * access to our inject and getClosures, so we need a LamLattice[Abs] as
    * well. */
  def labs = implicitly[LamLattice[Abs]]
  /** Shortcut for the environment */
  type Env = Environment[Addr]
  /** Shorcuct for the store */
  type Sto = Store[Addr, Abs]
  /** We need some frames */
  trait ANFFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
  }
  /** One frame to remember the operand when we evaluate the operator */
  case class FrameArg(e: LamExp, env: Env) extends ANFFrame
  /** And one frame to remember the operator value when we evaluate the operand */
  case class FrameFun(v: Abs) extends ANFFrame

  /** The stepEval function defines how to perform an evaluation step on an
    * expression */
  def stepEval(e: LamExp, env: Env, store: Sto, t: Time) = e match {
    /* A lambda evaluate to a closure by pairing it with the current environment,
     * and injecting this in the abstract domain */
    case Lam(_, _) => Set(ActionReachedValue(labs.inject((e, env)), store))
    /* To evaluate an application, we first have to evaluate e1, and we push a
     * continuation to remember to evaluate e2 in the environment env */
    case App(e1, e2) => Set(ActionPush(e1, FrameArg(e2, env), env, store))
    /* To evaluate a variable, just look it up in the store */
    case Var(x) => env.lookup(x) match {
      case Some(a) => Set(ActionReachedValue(store.lookup(a), store))
      case None => Set(ActionError(s"unbound variable $x"))
    }
  }

  /** The stepKont function defines how to behave when we reached a value v and we
    * have frame as the top continuation on the stack */
  def stepKont(v: Abs, frame: Frame, store: Sto, t: Time) = frame match {
    /* We have evaluated the operator v but still need to evaluate the operator e */
    case FrameArg(e, env) => Set(ActionPush(e, FrameFun(v), env, store))
    /* We have evaluated both the operator (fun) and the operand (v). We go through
     * the possible closures bound to the operator and for each of them, we
     * have to evaluate their body by extending their environment with their
     * argument */
    case FrameFun(fun) => labs.getClosures[LamExp, Addr](fun).map({
      case (Lam(x, e), env) => {
        val a = addr.variable(x, v, t)
        ActionEval(e, env.extend(x, a), store.extend(a, v))
      }
    })
  }

  /** The parse function just parses an expression from a string */
  def parse(program: String): LamExp = Lam("x", Var("x"))
}
