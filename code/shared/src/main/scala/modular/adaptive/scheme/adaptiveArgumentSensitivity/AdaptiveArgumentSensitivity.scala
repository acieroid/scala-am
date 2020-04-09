package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.util._
import scalaam.modular.adaptive.scheme._

trait AdaptiveArgumentSensitivity extends AdaptiveSchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString = args.mkString(",")
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
    val valuesPerPos = args.toList.transpose.map(_.toSet)
    val valuesPerArg = clo._1.args.zip(valuesPerPos).toMap
    // sort identifiers: those with the most values come first!
    var sortedArgs = valuesPerArg.toList.sortBy(_._2.size)
    // start the iteration
    var currentPars = clo._1.args
    var currentArgs = args
    while(currentArgs.size > limit) {
      // look at the next arg with the most values
      val (nextArg, argVals) = sortedArgs.head
      sortedArgs = sortedArgs.tail
      // join all argument values for the parameter
      val joinedArgVal = lattice.join(argVals)
      widenArg(clo, nextArg, joinedArgVal)
      // update all argument combination by "dropping" that argument
      val argPosition = currentPars.indexOf(nextArg)
      currentPars = currentPars.patch(argPosition,Nil,1)
      currentArgs = currentArgs.map(as => as.patch(argPosition,Nil,1))
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
    def append(s1: Set[Value], s2: => Set[Value]) =
      if(s1.isEmpty) { 
        s2 
      } else if (s2.isEmpty) {
        s1
      } else {
        val union = s1 ++ s2
        union.filter(abs => !union.exists(other => other != abs && lattice.subsumes(other,abs)))
      }
  }
}