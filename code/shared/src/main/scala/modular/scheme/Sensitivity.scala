package scalaam.modular.scheme

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemanticBase {
  case class Context() {
    override def toString = ""
  }
  def allocCtx(clo: lattice.Closure, args: List[Value]) = Context()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemanticBase {
  case class Context(args: List[Value]) {
    override def toString = args.mkString(",")
  }
  def allocCtx(clo: lattice.Closure, args: List[Value]): Context = Context(args)
}
