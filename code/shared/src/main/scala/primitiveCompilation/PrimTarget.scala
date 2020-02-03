package scalaam.primitiveCompilation

object PrimTarget {

  case class Args(args: Array[AExp]) {
    override def toString: String = "(" ++ args.map(_.toString).mkString(", ") ++ ")"
  }

  sealed trait Exp

  case class Bind(v: Var, e1: Exp, e2: Exp) extends Exp { override def toString: String = s"$e1 >>= { $v => $e2 }" }
  case class Fail() extends Exp { override def toString: String = "MayFail.Failure"}
  case class PrimCall(prim: Exp, args: Args) extends Exp {
    override def toString: String = prim.toString ++ args.toString
  }
  case class OpCall(op: PrimOp, args: Args) extends Exp {
    override def toString: String = op.name.toString ++ args.toString
  }
  case class Lat(l: LExp) extends Exp { override def toString: String = l.toString }
  case class IfTrue(cond: LExp, cons: Exp) extends Exp {
    override def toString: String = s"{ if (isTrue($cond)) { $cons } else { MayFail.Success($Bot) } }"
  }
  case class IfFalse(cond: LExp, cons: Exp) extends Exp {
    override def toString: String = s"{ if (isFalse($cond)) { $cons } else { MayFail.Success($Bot) } }"
  }

  sealed trait AExp

  case class Var(v: Id = Id.genId()) extends AExp { override def toString: String = v.toString }
  case class Num(n: Int) extends AExp { override def toString: String = s"SchemeLattice.number($n)" }
  case class Boo(b: Boolean) extends AExp { override def toString: String = s"SchemeLattice.bool($b)" }

  sealed trait LExp

  case object Bot extends LExp { override def toString: String = "bottom" }
  case object Top extends LExp { override def toString: String = "top" }
  case class Inj(e: AExp) extends LExp { override def toString: String = e.toString }
  case class Join(l1: LExp, l2: LExp) extends LExp { override def toString: String = s"SchemeLattice.join($l1, $l2)"}

}