package scalaam.primitiveCompilation

object PrimTarget {

  type Args = Array[AExp]

  sealed trait Exp

  case class Bind(v: Var, e1: Exp, e2: Exp) extends Exp
  case class Fail() extends Exp
  case class PrimCall(prim: Exp, args: Args) extends Exp
  case class OpCall(op: OpCall, args: Args) extends Exp
  case class Lat(l: LExp) extends Exp
  case class IfTrue(cond: LExp, cons: Exp) extends Exp
  case class IfFalse(cond: LExp, cons: Exp) extends Exp

  sealed trait AExp

  case class Var(v: Id = Id.genId()) extends AExp { override def toString: String = v.toString }
  case class Num(n: Int) extends AExp { override def toString: String = s"SchemeLattice.number($n)" }
  case class Boo(b: Boolean) extends AExp { override def toString: String = s"SchemeLattice.bool($b)" }

  sealed trait LExp

  case object Bot extends LExp { override def toString: String = "bottom" }
  case object Top extends LExp { override def toString: String = "top" }
  case class Inj(e: AExp) extends LExp { override def toString: String = e.toString }
  case class Join(l1: LExp, l2: LExp) extends LExp

}