package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.util.MonoidImplicits._

trait AdaptiveArgumentSensitivityPolicy1 extends AdaptiveArgumentSensitivity {
  // parameterized by a simple limit
  // every closure can only have at most "limit" components
  val limit: Int
  // track the number of components per closure
  private var closureCmps = Map[lattice.Closure, Set[Component]]()
  def adaptOnNewComponent(cmp: Component, call: Call): Boolean = {
    val Call(clo, _, _) = call
    // update the set of components per closure
    val updatedCmps = closureCmps.get(clo).getOrElse(Set()) + cmp
    closureCmps += (clo -> updatedCmps)
    // if there are too many components => do something about it!
    if (limit < updatedCmps.size) {
      val callArgs = updatedCmps.map(view(_).asInstanceOf[Call].ctx.args)
      joinArgs(clo, callArgs, limit)
      return true
    } else { // otherwise, not arguments need to be excluded
      return false
    }
  }
  // we need to update the `closureCmps` data structure when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    closureCmps = updateMap(updateClosure(update),updateSet(update))(closureCmps)
  }
}