package scalaam.primitiveCompilation

import scalaam.core._

object PrimTarget {

  case class Args(args: Array[(AExp, Identity.Position)]) {
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
  case class PrimCall(prim: Exp, args: Args, rec: Boolean, sto: Boolean, pos: Identity.Position) extends Exp {
    def print(i: Int): String = (rec, sto) match {
      case (true, false)  => indent(i) ++ prim.toString ++ "(List" ++ args.toString ++")"
      case (false, false) => indent(i) ++ prim.toString ++ args.toString
      case (_, true)   => s"${indent(i)}$prim.call($pos, originPos, (List $args), alloc)"
      //case (false, true)  => s"${indent(i)}$prim.call($pos, originPos, $args, alloc)" // TODO: are there primitives of this kind? (Note: If enabled, also enable this in PrimCompiler.scala).
    }
  }
  case class OpCall(op: PrimOp, args: Args, pos: Identity.Position) extends Exp {
    def print(i: Int): String =
      s"${indent(i)}$op.call($pos, originPos, $args, alloc)"
  }
  case class Lat(l: LExp) extends Exp {
    def print(i: Int): String = indent(i) ++ l.toString }
  /*
  case class IfTrue(cond: LExp, cons: Exp) extends Exp {
    def print(i: Int): String = s"${indent(i)}{ if (isTrue($cond)) {\n${cons.print(jump(jump(i)))}\n${indent(jump(i))}} else {\n${indent(jump(jump(i)))}MayFail.success($Bot)\n${indent(jump(i))}}\n${indent(i)}}"
  }
  case class IfFalse(cond: LExp, cons: Exp) extends Exp {
    def print(i: Int): String = s"${indent(i)}{ if (isFalse($cond)) {\n${cons.print(jump(jump(i)))}\n${indent(jump(i))}} else {\n${indent(jump(jump(i)))}MayFail.success($Bot)\n${indent(jump(i))}}\n${indent(i)}}"
  }
  */
  case class Cond(cond: LExp, cons: Exp, alt: Exp) extends Exp {
    def print(i: Int): String = s"${indent(i)}ifThenElse($cond)\n${indent(i)}{\n${cons.print(jump(i))}\n${indent(i)}}\n${indent(i)}{\n${alt.print(jump(i))}\n${indent(i)}}"
  }

  sealed trait AExp

  case class Var(v: Id = Id.genId()) extends AExp { override def toString: String = v.toString }
  case class Num(n: Int) extends AExp { override def toString: String = s"number($n)" }
  case class Boo(b: Boolean) extends AExp { override def toString: String = s"bool($b)" }

  sealed trait LExp

  case object Bot extends LExp { override def toString: String = "bottom" }
  case object Top extends LExp { override def toString: String = "top" } // TODO: This is not needed.
  case class Inj(e: AExp) extends LExp { override def toString: String = e.toString }
  case class Join(l1: LExp, l2: LExp) extends LExp { override def toString: String = s"join($l1, $l2)"}
}
