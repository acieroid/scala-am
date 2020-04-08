package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.modular.adaptive.scheme._

trait AdaptiveArgumentSensitivity extends AdaptiveSchemeModFSemantics {
  // A context is a partial mapping from the closure's formal parameters to argument values
  type ArgumentMapping = Map[Identifier, Value]
  case class ComponentContext(args: ArgumentMapping) {
   override def toString = args.toList
                               .map({ case (i,v) => s"$i -> $v" })
                               .mkString(" ; ")
  }
  def updateArgumentMapping(update: Component => Component)(args: ArgumentMapping) = 
    updateMap(updateValue(update))(args)
  def updateCtx(update: Component => Component)(ctx: ComponentContext) =
   ComponentContext(updateArgumentMapping(update)(ctx.args))
  // It's a partial mapping, because some parameters are not included for a given closure
  // (this way, we can fine-tune argument-sensitivity to avoid scalability issues with certain parameters or even specific argument values)
  def filterArgs(clo: lattice.Closure, args: ArgumentMapping): ArgumentMapping
  // The context for a given closure only consists of argument values for non-excluded parameters for that closure
  def allocCtx(clo: lattice.Closure, args: List[Value]): ComponentContext =
   ComponentContext(filterArgs(clo, clo._1.args.zip(args).toMap))
  // To adapt an existing component, we drop the argument values for parameters that have to be excluded
  def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
    case Main => Main
    case Call(clo,nam,ctx) =>
      val adaptedCtx = ComponentContext(filterArgs(clo,ctx.args))
      Call(clo,nam,adaptedCtx)
  }
}