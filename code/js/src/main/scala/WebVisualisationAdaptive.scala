package scalaam.web

import scalaam.modular.adaptive.AdaptiveModAnalysis

// Scala.js-related imports
import scala.scalajs.js

object WebVisualisationAdaptive {
  val d3 = js.Dynamic.global.d3
  lazy val __NODE_COLORS__ = List("blue", "green", "yellow", "red")
  lazy val __NO_OF_COLORS__ = __NODE_COLORS__.length
  lazy val __COLOR_SCALE__  = d3.scaleOrdinal()
                                  .domain(d3.range(__NO_OF_COLORS__))
                                  .range(__NODE_COLORS__)
}

class WebVisualisationAdaptive(override val analysis: AdaptiveModAnalysis[_]) extends WebVisualisation(analysis) {

  override def refreshDataAfterStep(cmp: analysis.Component,
                                    dps: Set[analysis.Component]) =
    if (analysis.adapted) {
      analysis.adapted = false
      super.refreshData()
    } else {
      super.refreshDataAfterStep(cmp,dps)
    }
}
