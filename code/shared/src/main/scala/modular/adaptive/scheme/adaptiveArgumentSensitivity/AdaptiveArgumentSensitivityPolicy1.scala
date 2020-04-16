package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.util.MonoidImplicits._

trait AdaptiveArgumentSensitivityPolicy1 extends AdaptiveArgumentSensitivity {
  // parameterized by a simple limit
  // every closure can only have at most "limit" components
  val limit: Int
  // track the number of components per closure
  private var closureCmps = Map[lattice.Closure, Set[Component]]()
  override def onNewComponent(cmp: Component, call: Call)= {
    // update the set of components per closure
    val updatedCmps = closureCmps.get(call.clo).getOrElse(Set()) + cmp
    closureCmps += (call.clo -> updatedCmps)
    // if there are too many components => do something about it!
    if (limit < updatedCmps.size) {
      joinComponents(updatedCmps)
    }
  }
  // we need to update the `closureCmps` data structure when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    closureCmps = updateMap(updateClosure(update),updateSet(update))(closureCmps)
  }
}