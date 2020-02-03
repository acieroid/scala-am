package primitiveCompilation

import primitiveCompilation.PrimTarget.AExp

object PrimTarget {

  type Args = Array[AExp]

  sealed trait Exp

  case class Bind(v: Var, e1: Exp, e2: Exp) extends Exp
  case class BindC(v: CVa, e1: Exp, e2: Exp) extends Exp
  case class Fail() extends Exp
  case class PrimCall(prim: Exp, args: Args) extends Exp
  case class OpCall(op: OpCall, args: Args) extends Exp
  case class Lat(l: LExp) extends Exp
  case class IfTrue(cond: LExp, cons: Exp) extends Exp
  case class IfFalse(cond: LExp, cons: Exp) extends Exp

  sealed trait AExp

  case class Var(v: Id) extends AExp { override def toString: String = v.toString }
  case class CVa(v: Id = Id.genId()) extends AExp { override def toString: String = v.toString }
  case class Arg(a: Id) extends AExp { override def toString: String = a.toString }
  case class Num(n: Int) extends AExp { override def toString: String = n.toString }
  case class Boo(b: Boolean) extends AExp { override def toString: String = b.toString }

  sealed trait LExp

  case object Bot extends LExp
  case object Top extends LExp
  case class Inj(e: AExp) extends LExp
  case class Join(l1: LExp, l2: LExp) extends LExp

}