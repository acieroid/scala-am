package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.util.MonoidImplicits._

trait AdaptiveArgumentSensitivityPolicy2 extends AdaptiveArgumentSensitivity
                                            with AdaptiveArgumentSelection {
  // parameterized by a simple limit
  // every trace can only have at most "limit" components of the same closure
  val limit: Int
  // for every component, keep track of all other components that can reach it ...
  var calledBy = Map[Component, Set[Component]]()
  // ... what follows is boring boilerplate code to correctly do the bookkeeping in the `calledBy` map
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
  // we keep track of which arguments need to be dropped from which components
  var excludedArgs = Map[(lattice.Closure, ArgumentMapping), Set[Identifier]]()
  private def excludeArgs(clo: lattice.Closure, args: ArgumentMapping, prs: Set[Identifier]) =
    excludedArgs += ((clo,args) -> (excludedArgs.getOrElse((clo,args),Set()) ++ prs))
  def filterArgs(clo: lattice.Closure, args: ArgumentMapping) = args -- excludedArgs.getOrElse((clo,args),Set())
  // do a simple loop check when you have too many components in a single stracktrace
  def adaptOnNewComponent(cmp: Component, call: Call): Boolean = {
    val Call(clo, _, _) = call
    val calls = calledBy(cmp).flatMap(c => view(c) match {
      case cll: Call if cll.clo == clo => Some(cll)
      case _ => None
    }) + call
    if (calls.size > limit) {
      val callArgs = calls.map(c => c.ctx.args)
      val excludedPars = dropArgs(clo, callArgs, limit)
      callArgs.foreach { args => 
        excludeArgs(clo, args, excludedPars)
      }
      return true 
    }
    return false
  }
  // we need to update the `calledBy` data structure whenever the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    excludedArgs = updateMap(updatePair(updateClosure(update), 
                                  updateArgumentMapping(update)), 
                       (s: Set[Identifier]) => s)(excludedArgs)
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
