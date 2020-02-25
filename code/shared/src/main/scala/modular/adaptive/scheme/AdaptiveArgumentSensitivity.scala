package scalaam.modular.adaptive.scheme

import scalaam.core._
import scalaam.util.MonoidImplicits._

trait AdaptiveArgumentSensitivity extends AdaptiveSchemeModFSemantics {
  // A context is a partial mapping from the closure's formal parameters to argument values
  case class ComponentContext(args: Map[Identifier, Value]) {
   override def toString = args.toList
                               .map({ case (i,v) => s"$i -> $v" })
                               .mkString(" ; ")
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext) =
   ComponentContext(updateMap(updateValue(update))(ctx.args))
  // It's a partial mapping, because some parameters are not included for a given closure
  // (this way, we can fine-tune argument-sensitivity to avoid scalability issues with certain parameters)
  private var excludedArgs = Map[lattice.Closure, Set[Identifier]]().withDefaultValue(Set.empty)
  private def excludeArg(clo: lattice.Closure, par: Identifier) =
   excludedArgs += (clo -> (excludedArgs(clo) + par))
  private def excludeArgs(clo: lattice.Closure, prs: Set[Identifier]) =
   prs.foreach(par => excludeArg(clo,par))
  // The context for a given closure only consists of argument values for non-excluded parameters for that closure
  def allocCtx(clo: lattice.Closure, args: List[Value]): ComponentContext =
   ComponentContext(clo._1.args.zip(args).toMap -- excludedArgs(clo))
  // To adapt an existing component, we drop the argument values for parameters that have to be excluded
  def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
    case Main => Main
    case Call(clo,nam,ctx) =>
      val adaptedCtx = ComponentContext(ctx.args -- excludedArgs(clo))
      Call(clo,nam,adaptedCtx)
  }
  // The main logic for adaptive argument sensitivity
  // parameterized by a method `adaptArguments` to configure the concrete policy
  // This method takes a new component that is added to the analysis ...
  // ... and can decide which parameters of the corresponding closure need to be "excluded"
  def adaptArguments(cmp: Component, call: Call): Set[Identifier]
  // the actual method that gets called for every new component
  override protected def adaptOnNewComponent(cmp: Component, call: Call): Boolean = {
    val excludedPars = adaptArguments(cmp,call)
    if(excludedArgs.nonEmpty) {
      excludeArgs(call.clo, excludedPars)
      return true
    } else {
      return false
    }
  }
  // we need to update the excludedArgs data structure when the analysis adapts
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    excludedArgs = updateMap(updateClosure(update), (s: Set[Identifier]) => s)(excludedArgs)
  }
}

trait SimpleAdaptiveArgumentSensitivity extends AdaptiveArgumentSensitivity {

  // parameterized by a simple limit
  // every closure can only have at most "limit" components
  val limit: Int
  // track the number of components per closure
  private var closureCmps = Map[lattice.Closure, Set[Component]]()
  def adaptArguments(cmp: Component, call: Call): Set[Identifier] = {
    val Call(clo, _, _) = call
    // update the set of components per closure
    val prvCmps = closureCmps.get(clo).getOrElse(Set())
    val newCmps = prvCmps + cmp
    closureCmps += (clo -> newCmps)
    // if there are too many components => do something about it!
    if (limit < newCmps.size) {
      clo._1.args.toSet // naive policy: drop all of the argument-sensitivity
    } else { // otherwise, not arguments need to be excluded
      Set.empty
    }
  }
  // we need to update the closureCmps data structure when the analysis adapts
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    closureCmps = updateMap(updateClosure(update),updateSet(update))(closureCmps)
  }
}
