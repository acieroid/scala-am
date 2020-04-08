package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.modular.adaptive.scheme._
import scalaam.util.MonoidImplicits._

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

trait AdaptiveArgumentSensitivityAlt extends AdaptiveSchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString = s"[${args.mkString(",")}]"
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext) =
    ComponentContext(ctx.args.map(updateValue(update)))
  // to determine how arguments need to be adapted
  private var adaptedArgs = Map[(lattice.Closure,Identifier),Set[Value]]()
  protected def widenArg(clo: lattice.Closure, par: Identifier, arg: Value) = {
    val currentAbs = adaptedArgs.getOrElse((clo,par),Set.empty)
    val updatedAbs = currentAbs.filterNot(lattice.subsumes(arg,_)) + arg
    this.adaptedArgs += ((clo,par) -> updatedAbs)
  }
  private def adaptArg(clo: lattice.Closure, par: Identifier, arg: Value) = 
    adaptedArgs.getOrElse((clo,par),Set.empty)
               .find(lattice.subsumes(_, arg))
               .getOrElse(arg)
  private def adaptArgs(clo: lattice.Closure, args: List[Value]) =
    clo._1.args.zip(args).map { case (par,arg) => adaptArg(clo,par,arg) }
  // The context for a given closure only consists of argument values for non-excluded parameters for that closure
  def allocCtx(clo: lattice.Closure, args: List[Value]) = ComponentContext(adaptArgs(clo,args))
  // To adapt an existing component, we drop the argument values for parameters that have to be excluded
  def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
    case Main               => Main
    case Call(clo,nam,ctx)  => Call(clo, nam, ComponentContext(adaptArgs(clo,ctx.args)))
  }
  // we need to update the adaptedArgs mapping when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    def updateClosurePar(key: (lattice.Closure, Identifier)) = (updateClosure(update)(key._1), key._2)
    def updateValueSet(values: Set[Value]) = updateSet(updateValue(update))(values)
    this.adaptedArgs = updateMap(updateClosurePar,updateValueSet)(adaptedArgs)
  }
}