package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.util.MonoidImplicits._

trait AdaptiveArgumentSensitivityPolicy2 extends AdaptiveArgumentSensitivity {
  // parameterized by a simple budget
  // the analysis can have at most "budget" components at any time
  val budget: Int
  // track the number of components per closure
  private var closureCmps = Map[lattice.Closure, Set[Component]]()
  def adaptOnNewComponent(cmp: Component, call: Call): Boolean = {
    // update the set of components per closure
    val updatedCmps = closureCmps.get(call.clo).getOrElse(Set()) + cmp
    closureCmps += (call.clo -> updatedCmps)
    return false
  }
  override def adaptAnalysis() = {
    super.adaptAnalysis()
    // if the budged is exceeded, adapt the analysis until the budget is satisfied
    if (allComponents.size > budget) {
      val (topClosure, cmps) = closureCmps.maxBy(_._2.size)
      val callArgs = cmps.map(view(_).asInstanceOf[Call].ctx.args)
      joinArgs(topClosure, callArgs, scala.math.max(1, cmps.size - 1))
      updateAnalysis()
    } 
  }
  // we need to update the `closureCmps` data structure when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    closureCmps = updateMap(updateClosure(update),updateSet(update))(closureCmps)
  }
}