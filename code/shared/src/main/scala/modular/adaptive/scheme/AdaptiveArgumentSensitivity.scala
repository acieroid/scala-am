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
    if (excludedPars.nonEmpty) {
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

trait AdaptiveArgumentSensitivityPolicy1 extends AdaptiveArgumentSensitivity {
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
  // we need to update the `closureCmps` data structure when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    closureCmps = updateMap(updateClosure(update),updateSet(update))(closureCmps)
  }
}

trait AdaptiveArgumentSensitivityPolicy2 extends AdaptiveArgumentSensitivity {
  // parameterized by a simple limit
  // every trace can only have at most "limit" components of the same closure
  val limit: Int
  // for every component, keep track of all other components that can reach it ...
  var calledBy = Map[Component, Set[Component]]()
  // ... what follows is boilerplate code to correctly do the bookkeeping in the `calledBy` map
  def registerCall(source: Component, target: Component) = {
    lazy val targetCalledBy = calledBy.getOrElse(target, Set())
    lazy val sourceCalledBy = calledBy.getOrElse(source, Set())
    if (!targetCalledBy.contains(source)) {
      val newComponents = (sourceCalledBy + source) -- targetCalledBy
      calledBy += (target -> (targetCalledBy ++ newComponents))
      propagate(List((target,newComponents)))
    }
  }
  @scala.annotation.tailrec
  private def propagate(worklist: List[(Component,Set[Component])]): Unit =
    if(worklist.nonEmpty) {
      val (current, newComponents) :: rest = worklist
      val cmps = dependencies.getOrElse(current,Set())
      val updatedWorklist = cmps.foldLeft(rest) { (acc,cmp) =>
        val cmpCalledBy = calledBy(cmp) // contains at least the 'current' component
        val componentsToAdd = newComponents -- cmpCalledBy
        if(componentsToAdd.isEmpty) {
          acc
        } else {
          calledBy += (cmp -> (cmpCalledBy ++ componentsToAdd))
          (cmp, componentsToAdd) :: acc
        }
      }
      propagate(updatedWorklist)
    }
  // the policy for adapting arguments
  def adaptArguments(cmp: Component, call: Call): Set[Identifier] = Set.empty
  // we need to update the `calledBy` data structure whenever the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    calledBy = updateMap(update, updateSet(update))(calledBy)
  }
  // we instrument the intra-analysis to:
  // - perform the necessary bookkeeping for 'calledBy' whenever a function is called
  // - check if the analysis needs to be adapted on new function calls
  override def intraAnalysis(cmp: Component) = new AdaptiveSchemeModFAnalysisIntra(cmp)
  class AdaptiveSchemeModFAnalysisIntra(component: Component) extends super.IntraAnalysis(component) {
    override def call(cmp: Component) = {
      registerCall(component,cmp)
      super.call(cmp)
    }
  }


}
