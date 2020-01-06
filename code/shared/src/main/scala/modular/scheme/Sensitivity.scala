package scalaam.modular.scheme

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemantics {
  case class Context() {
    override def toString = ""
  }
  def allocCtx(clo: lattice.Closure, args: List[Value]): Context = Context()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemantics {
  case class Context(args: List[Value]) {
    override def toString: String = args.mkString(",")
  }
  def allocCtx(clo: lattice.Closure, args: List[Value]): Context = Context(args)
}
