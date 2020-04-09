package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.util._
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
  // joining arguments
  def joinArgs(clo: lattice.Closure, args: Set[List[Value]], limit: Int) = {
    val argPositions = clo._1.args.zipWithIndex.toMap
    val valuesPerPos = args.toList.transpose.map(_.toSet)
    val valuesPerArg = clo._1.args.zip(valuesPerPos).toMap
    // sort identifiers: those with the most values come first!
    var sortedArgs = valuesPerArg.toList.sortBy(_._2.size)
    // start the iteration
    var currentArgs = args
    while(currentArgs.size > limit) {
      // look at the next arg with the most values
      val (nextArg, argVals) = sortedArgs.head
      sortedArgs = sortedArgs.tail
      // join all argument values for the parameter
      val joinedArgVal = lattice.join(argVals)
      widenArg(clo, nextArg, joinedArgVal)
      // update all argument combination by "dropping" that argument
      val argPosition = argPositions(nextArg)
      currentArgs = args.map(as => as.patch(argPosition,Nil,1))
    }
  }
  // we need to update the adaptedArgs mapping when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.adaptedArgs = updateMap(updateClosurePar(update),updateSet(updateValue(update)))(adaptedArgs)(valueSetMonoid)
  }
  private def updateClosurePar(update: Component => Component)(key: (lattice.Closure, Identifier)) = 
    (updateClosure(update)(key._1), key._2)
  private val valueSetMonoid = new Monoid[Set[Value]] {
    def zero = Set.empty
    def append(s1: Set[Value], s2: => Set[Value]) = {
      val union = s1 ++ s2
      union.filter(vlu => !union.exists(lattice.subsumes(_,vlu)))
    }
  }
}