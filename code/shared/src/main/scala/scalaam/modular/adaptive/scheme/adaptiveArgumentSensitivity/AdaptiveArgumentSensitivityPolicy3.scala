package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.modular.scheme.modf._
import scalaam.util.MonoidImplicits._

trait AdaptiveArgumentSensitivityPolicy3 extends AdaptiveArgumentSensitivity {
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
  // do a simple loop check when you have too many components in a single stracktrace
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = {
    super.onNewComponent(cmp,call)
    val callStack = calledBy(cmp)
    val prevCmps: Set[Component] = callStack.collect(c => view(c) match {
      case cll: Call[ComponentContext] if cll.clo._1.idn == call.clo._1.idn => c
    })
    val updatedCmps = prevCmps + cmp
    if (updatedCmps.size > limit) {
      joinComponents(updatedCmps)
    }
  }
  // we need to update the `calledBy` data structure whenever the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    calledBy = updateMap(update, updateSet(update))(calledBy)
  }
  // we instrument the intra-analysis to:
  // - perform the necessary bookkeeping for 'calledBy' whenever a function is called
  // - check if the analysis needs to be adapted on new function calls
  override def intraAnalysis(cmp: Component) = new AdaptiveArgIntraPolicy3(cmp)
  class AdaptiveArgIntraPolicy3(component: Component) extends AdaptiveSchemeModFIntra(component) {
    override def call(cmp: Component) = {
      registerCall(component,cmp)
      super.call(cmp)
    }
  }
}