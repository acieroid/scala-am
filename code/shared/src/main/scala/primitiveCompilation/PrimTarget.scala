package scalaam.primitiveCompilation

object PrimTarget {

  case class Args(args: Array[AExp]) {
    override def toString: String = "(" ++ args.map(_.toString).mkString(", ") ++ ")"
  }

  sealed trait Exp {
    def indent(i: Int): String = " " * i
    def jump(i: Int): Int = i + 2
    def print(i: Int): String
    override def toString: String = print(0)
  }

  case class Bind(v: Var, e1: Exp, e2: Exp) extends Exp {
    def print(i: Int): String = s"${e1.print(i)} >>= { $v =>\n${e2.print(jump(i))}\n${indent(i)}}"
  }
  case class Fail() extends Exp {
    def print(i: Int): String = indent(i) ++ "MayFail.Failure"
  }
  case class PrimCall(prim: Exp, args: Args) extends Exp {
    def print(i: Int): String = indent(i) ++ prim.toString ++ args.toString
  }
  case class OpCall(op: PrimOp, args: Args) extends Exp {
    def print(i: Int): String = indent(i) ++ op.name.toString ++ args.toString
  }
  case class Lat(l: LExp) extends Exp {
    def print(i: Int): String = indent(i) ++ l.toString }
  case class IfTrue(cond: LExp, cons: Exp) extends Exp {
    def print(i: Int): String = s"${indent(i)}{ if (isTrue($cond)) {\n${cons.print(jump(jump(i)))}\n${indent(jump(i))}} else {\n${indent(jump(jump(i)))}MayFail.Success($Bot)\n${indent(jump(i))}}\n${indent(i)}}"
  }
  case class IfFalse(cond: LExp, cons: Exp) extends Exp {
    def print(i: Int): String = s"${indent(i)}{ if (isFalse($cond)) {\n${cons.print(jump(jump(i)))}\n${indent(jump(i))}} else {\n${indent(jump(jump(i)))}MayFail.Success($Bot)\n${indent(jump(i))}}\n${indent(i)}}"
  }

  sealed trait AExp

  case class Var(v: Id = Id.genId()) extends AExp { override def toString: String = v.toString }
  case class Num(n: Int) extends AExp { override def toString: String = s"SchemeLattice.number($n)" }
  case class Boo(b: Boolean) extends AExp { override def toString: String = s"SchemeLattice.bool($b)" }

  sealed trait LExp

  case object Bot extends LExp { override def toString: String = "bottom" }
  case object Top extends LExp { override def toString: String = "top" } // TODO: This is not needed.
  case class Inj(e: AExp) extends LExp { override def toString: String = e.toString }
  case class Join(l1: LExp, l2: LExp) extends LExp { override def toString: String = s"SchemeLattice.join($l1, $l2)"}

}