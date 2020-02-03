package primitiveCompilation

object PrimSource {

  type Args = Array[AExp]

  trait Exp

  case class AE(ae: AExp)                        extends Exp
  case class If(cond: AExp, cons: Exp, alt: Exp) extends Exp
  case class Let(init: Exp, body: Exp)           extends Exp
  //case class RecCall()                           extends PrimSourceExp
  case class PrimCall(prim: Exp, args: Args)     extends Exp
  case class OpCall(op: PrimOp, args: Args)      extends Exp


  trait AExp

  case class Var(v: Int) extends AExp
  case class Arg(a: Int) extends AExp
  case class Num(n: Int) extends AExp
  case class Boo(b: Boolean) extends AExp

}
