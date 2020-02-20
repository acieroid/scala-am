package scalaam.primitiveCompilation

import scalaam.core._

object PrimTarget {

  case class Args(args: Array[(AExp, Identity.Position)]) {
    override def toString: String =
      // TODO: this is a precision-improving optimization, it should be fine only using a._2.toString, but could be less precise
      "(" ++ args.map(a => s"(${if (a._1.toString.charAt(0) == '_') { a._2.toString } else { s"${a._1}_pos" }}, ${a._1})").mkString(", ") ++ ")"
    def splicedString: String = args.map(_._1.toString).mkString(", ")
  }

  sealed trait Exp {
    def indent(i: Int): String = " " * i
    def jump(i: Int): Int = i + 2
    def print(i: Int): String
    override def toString: String = print(0)
  }

  def scalaNameOf(prim: String): String =
    prim.replace('?', 'p')
      .replace("<","Lt")
      .replace(">","Gt")
      .replace("=", "Eq")
      .replaceAll("[^a-zA-Z0-9]+", "_")

  case class Bind(v: Var, e1: Exp, e2: Exp) extends Exp {
    def print(i: Int): String = s"${e1.print(i)} >>= { $v  =>\n${e2.print(jump(i))}\n${indent(i)}}"
  }
  case class Fail() extends Exp {
    def print(i: Int): String = indent(i) ++ "MayFail.Failure"
  }
  case class PrimCall(prim: Exp, args: Args, rec: Boolean, sto: Boolean, pos: Identity.Position) extends Exp {
    def print(i: Int): String =
      if (rec) {
        indent(i) ++ "recursiveCall(List" ++ args.toString ++ ")"
      } else {
        s"${indent(i)}${prim.toString.capitalize}.call(fpos, $pos, List$args, store, alloc).map(_._1)"
      }
  }

/*case class LatticeOp(op: LatOp, args: Args, pos: Identity.Position) extends Exp {
    def print(i: Int): String = (LatticeOperations.alcNams.contains(op.name), LatticeOperations.stoNams.contains(op.name)) match {
      case (true, true) => s"${indent(i)}$op.call(fpos, $pos, List$args, store, alloc)"
      case (true, false) => throw new Exception("A primitive operation without access to the store cannot perform allocations. Illegal state.")
      case (false, true) => s"${indent(i)}$op.call(${args.splicedString}, store).map(_._1)" // TODO: can we assume only one argument here? // Map needed because store must not be propagated further.
      case (false, false) if op.arity == -1 => s"${indent(i)}$op.call(List$args)"
      case (false, false) => s"${indent(i)}$op.call$args"
    }
  }*/
  case class Lat(l: LExp) extends Exp {
    def print(i: Int): String = indent(i) ++ l.toString }


  case class Cond(cond: LExp, cons: Exp, alt: Exp) extends Exp {
    def print(i: Int): String = s"${indent(i)}ifThenElse($cond)\n${indent(i)}{\n${cons.print(jump(i))}\n${indent(i)}}\n${indent(i)}{\n${alt.print(jump(i))}\n${indent(i)}}"
  }

  sealed trait AExp

  case class Var(v: Id = Id.genId()) extends AExp { override def toString: String = scalaNameOf(v.toString) }
  case class Num(n: Int) extends AExp { override def toString: String = s"number($n)" }
  case class Boo(b: Boolean) extends AExp { override def toString: String = s"bool($b)" }

  sealed trait LExp

  case object Bot extends LExp { override def toString: String = "bottom" }
  case object Top extends LExp { override def toString: String = "top" } // TODO: This is not needed.
  case class Inj(e: AExp) extends LExp { override def toString: String = e.toString }
  case class Join(l1: LExp, l2: LExp) extends LExp { override def toString: String = s"join($l1, $l2)"}
}
