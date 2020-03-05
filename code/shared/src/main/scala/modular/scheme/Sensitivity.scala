package scalaam.modular.scheme

import scalaam.core.Identity.Position

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemantics {
  case class ComponentContext() {
    override def toString = ""
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString: String = args.mkString(",")
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext(args)
}

trait CallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position) {
    override def toString: String = s"$call->$fn"
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext(clo._1.idn.pos, call)
}

trait FullArgumentCallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position, args: List[Value]) {
    override def toString: String = {
      val argsstr = args.mkString(",")
      s"$call->$fn $argsstr"
    }
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext =
    ComponentContext(clo._1.idn.pos, call, args)
}

trait PrimitiveSensitivity extends SchemeModFSemantics {
  trait ComponentContext
  case class PrimitiveContext(fn: Position, call: Position, args: List[Value]) extends ComponentContext {
    override def toString: String = {
      val argsstr = args.mkString(",")
      s"$call->$fn $argsstr"
    }
  }
  case class NoContext() extends ComponentContext {
    override def toString = ""
  }
  def prims = Set("<=", ">=", ">",
      "zero?", "positive?", "negative?", "odd?", "even?",
      "max", "min", "abs", "gcd", "lcm",
      "not",
      "newline", "display",
      "caar", "cadr", "cddr", "cdar", "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr", "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr", "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
    "equal?", "list?", "list-ref", "member", "memq", "assoc", "assq", "list-tail", "length", "append", "reverse",
    // "map", "for-each", "foldr", "foldr-aux", "foldl", "foldl-aux"
  )
  def isPrimitive(nam: Option[String]): Boolean = nam match {
    case Some(n) if prims.contains(n) => true
    case _ => false
  }

  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = {
    if (isPrimitive(nam)) {
//      println(s"Apply ${nam.get}")
      PrimitiveContext(clo._1.idn.pos, call, args)
    } else {
      NoContext()
    }
  }
}
