package scalaam.web

import scalaam.core._
import scalaam.modular.adaptive._

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

trait WebAdaptiveAnalysis[Expr <: Expression] extends AdaptiveModAnalysis[Expr] {
  var webvis: WebVisualisationAdaptive = null
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    webvis.adapted = true
  }
}

class WebVisualisationAdaptive(override val analysis: WebAdaptiveAnalysis[_]) extends WebVisualisation(analysis) {

  // give the analysis a pointer to the visualisation
  analysis.webvis = this

  var adapted = false

  override def displayText(cmp: analysis.Component) = analysis.deref(cmp).toString()

  override def refreshDataAfterStep(cmp: analysis.Component,
                                    dps: Set[analysis.Component]) =
    if (this.adapted) {
      this.adapted = false
      super.refreshData()
    } else {
      super.refreshDataAfterStep(cmp,dps)
    }
}
