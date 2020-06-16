package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

trait AdaptiveArgumentSensitivityPolicy2 extends AdaptiveArgumentSensitivity {
  // parameterized by a simple budget
  // the analysis can have at most "budget" components at any time
  val budget: Int
  override def adaptAnalysis() = {
    super.adaptAnalysis()
    // if the budged is exceeded, adapt the analysis until the budget is satisfied
    if (visited.size > budget) {
      val cmps = cmpsPerFn.maxBy(_._2.size)._2
      joinComponents(cmps)
    } 
  }
}