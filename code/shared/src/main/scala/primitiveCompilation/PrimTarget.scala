package primitiveCompilation

object PrimTarget {

  type Args = Array[AExp]

  trait Exp

  case class Bind(e1: Exp, e2: Exp) extends Exp
  case class BindC(e1: Exp, e2: Exp) extends Exp
  case class Fail() extends Exp
  case class PrimCall(prim: Exp, args: Args) extends Exp
  case class OpCall(op: OpCall, args: Args) extends Exp
  case class Lat(l: LExp) extends Exp
  case class IfTrue(cond: LExp, cons: Exp) extends Exp
  case class IfFalse(cond: LExp, cons: Exp) extends Exp

  trait AExp

  case class Var(v: Int) extends AExp
  case class CVa(v: Int) extends AExp
  case class Arg(a: Int) extends AExp
  case class Num(n: Int) extends AExp
  case class Boo(b: Boolean) extends AExp

  trait LExp

  case object Bot extends LExp
  case object Top extends LExp
  case class Inj(e: Exp) extends LExp
  case class Join(l1: LExp, l2: LExp) extends LExp

}