package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.util.MonoidImplicits._

trait AdaptiveArgumentSensitivityPolicy1 extends AdaptiveArgumentSensitivity
                                            with AdaptiveArgumentSelection {
  // parameterized by a simple limit
  // every closure can only have at most "limit" components
  val limit: Int
  // keep track of which parameters need to be excluded for which closures
  private var excludedArgs = Map[lattice.Closure, Set[Identifier]]()
  private def excludeArgs(clo: lattice.Closure, prs: Set[Identifier]) =
   excludedArgs += (clo -> (excludedArgs.getOrElse(clo,Set()) ++ prs))
  def filterArgs(clo: lattice.Closure, args: ArgumentMapping) = args -- excludedArgs.getOrElse(clo,Set())
  // track the number of components per closure
  private var closureCmps = Map[lattice.Closure, Set[Component]]()
  def adaptOnNewComponent(cmp: Component, call: Call): Boolean = {
    val Call(clo, _, _) = call
    // update the set of components per closure
    val prvCmps = closureCmps.get(clo).getOrElse(Set())
    val newCmps = prvCmps + cmp
    closureCmps += (clo -> newCmps)
    // if there are too many components => do something about it!
    if (limit < newCmps.size) {
      val callArgs = newCmps.map(view(_).asInstanceOf[Call].ctx.args)
      val excludedArgs = dropArgs(clo, callArgs, limit)
      excludeArgs(clo, excludedArgs) // naive policy: drop all of the argument-sensitivity
      return true
    } else { // otherwise, not arguments need to be excluded
      return false
    }
  }
  // we need to update the `closureCmps` data structure when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    excludedArgs = updateMap(updateClosure(update), (s: Set[Identifier]) => s)(excludedArgs)
    closureCmps = updateMap(updateClosure(update),updateSet(update))(closureCmps)
  }
}