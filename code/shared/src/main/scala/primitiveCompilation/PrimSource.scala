package primitiveCompilation

object PrimSource {

  type Args = Array[AExp]

  sealed trait Exp

  case class AE(ae: AExp)                        extends Exp
  case class If(cond: AExp, cons: Exp, alt: Exp) extends Exp
  case class Let(v: Var, exp: Exp, body: Exp)    extends Exp
  //case class RecCall()                           extends Exp
  case class PrimCall(prim: Exp, args: Args)     extends Exp
  case class OpCall(op: PrimOp, args: Args)      extends Exp

  sealed trait AExp

  case class Var(v: Id) extends AExp
  case class Arg(a: Id) extends AExp
  case class Num(n: Int) extends AExp
  case class Boo(b: Boolean) extends AExp

}
