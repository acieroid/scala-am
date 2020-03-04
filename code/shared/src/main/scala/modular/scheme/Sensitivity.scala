package scalaam.modular.scheme

import scalaam.core.Identity.Position

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemantics {
  case class ComponentContext() {
    override def toString = ""
  }
  def allocCtx(clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString: String = args.mkString(",")
  }
  def allocCtx(clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext(args)
}

trait CallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position) {
    override def toString: String = s"$call->$fn"
  }
  def allocCtx(clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext(clo._1.idn.pos, call)
}

trait FullArgumentCallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position, args: List[Value]) {
    override def toString: String = {
      val argsstr = args.mkString(",")
      s"$call->$fn $argsstr"
    }
  }
  def allocCtx(clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext(clo._1.idn.pos, call, args)
}
