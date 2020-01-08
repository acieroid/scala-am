package scalaam.modular.scheme

import scalaam.core._

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

/* Adaptive argument sensitivity */
trait AdaptiveArgumentSensitivity extends AdaptiveSchemeModFSemantics {
  // A context is a partial mapping from the closure's formal parameters to argument values
  case class Context(args: Map[Identifier, Value]) {
    override def toString = args.toList
                                .map({ case (i,v) => s"$i -> $v" })
                                .mkString(" ; ")
  }
  // It's a partial mapping, because some parameters are not included for a given closure
  // (this way, we can fine-tune argument-sensitivity to avoid scalability issues with certain parameters)
  private var excludedArgs = Map[lattice.Closure, Set[Identifier]]().withDefaultValue(Set.empty)
  private def excludeArg(clo: lattice.Closure, par: Identifier) =
    excludedArgs += (clo -> (excludedArgs(clo) + par))
  private def excludeArgs(clo: lattice.Closure, prs: Set[Identifier]) =
    prs.foreach(par => excludeArg(clo,par))
  // The context for a given closure only consists of argument values for non-excluded parameters for that closure
  def allocCtx(clo: lattice.Closure, args: List[Value]): Context =
    Context(clo._1.args.zip(args).toMap -- excludedArgs(clo))
  // To adapt an existing component, we drop the argument values for parameters that have to be excluded
  def alpha(cmp: Component): Component = cmp match {
    case Main => Main
    case Call(clo,nam,ctx) =>
      val updatedCtx = Context(ctx.args -- excludedArgs(clo))
      Call(clo,nam,updatedCtx)
  }
  // The main logic for adaptive argument sensitivity
  // parameterized by a method `adaptArguments` to configure the concrete policy
  // This method takes a new component that is added to the analysis ...
  // ... and can decide which parameters of the corresponding closure need to be "excluded"
  protected def adaptArguments(call: Call): Set[Identifier]
  // this gets called whenever new components are added to the analysis
  // it calls `adaptArguments` for every new component, and "adapts" the analysis if necessary
  override protected def registerNewComponents(cmps: Set[Component]) = {
    super.registerNewComponents(cmps)
    val updates = cmps.foldLeft(List.empty[(lattice.Closure,Set[Identifier])]) {
      (acc, cmp) => cmp match {
        case Main => throw new Exception("This should not happen!")
        case call: Call =>
          val excludedPars = adaptArguments(call)
          if (excludedPars.nonEmpty) { (call.clo, excludedPars) :: acc } else { acc }
      }
    }
    // update the argument-sensitivity policy (of course, only if there are updates) and signal that the definition of alpha has changed
    if (updates.nonEmpty) {
      updates.foreach { case (clo,prs) => excludeArgs(clo,prs) }
      onAlphaChange()
    }
  }
}
