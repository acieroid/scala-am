package scalaam.modular

import scalaam.core._
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Queue

abstract class ModAnalysis[Expr <: Exp](program: Expr) {

  // parameterized by a 'intra-component' representation
  type IntraComponent
  def initial: IntraComponent

  // keep track of all components in the analysis
  private val components = Set[IntraComponent]()

  // an intra-analysis of a component can cause effects that impact the analysis of other components
  // an intra-analysis therefore can:
  // - register a dependency on an effect (e.g., when it reads an address)
  // - trigger an effect (e.g., when it writes to an address)
  protected trait Effect
  // here, we track which components depend on which effects
  private val deps = Map[Effect,Set[IntraComponent]]().withDefaultValue(Set())
  protected def addDep(component: IntraComponent, effect: Effect) = deps.get(effect) match {
    case None => deps(effect) = Set(component)
    case Some(components) => components.add(component)
  }

  // parameterized by an 'intra-component analysis'
  protected def intraAnalysis(component: IntraComponent): IntraAnalysis
  protected abstract class IntraAnalysis(val component: IntraComponent) {
    // keep track of effects triggered by this intra-analysis
    private[ModAnalysis] val effects = Set[Effect]()
    protected def pushEffect(eff: Effect) = effects.add(eff)
    protected def pullEffect(eff: Effect) = addDep(component,eff)
    // keep track of new components called by this intra-analysis
    private[ModAnalysis] val newComponents = Set[IntraComponent]()
    protected def spawn(component: IntraComponent): Unit =
      if (components.add(component)) {
        newComponents.add(component)
      }
    // analyses the given component
    def analyze(): Unit
  }

  // the inter-analysis loop
  def analyze(): Unit = {
    val visited = Set[IntraComponent]()
    val worklist = Queue[IntraComponent](initial)
    while (!worklist.isEmpty) {
      val component = worklist.dequeue()
      if (visited.add(component)) {
        val intra = intraAnalysis(component)
        intra.analyze() // do the intra-analysis
        val succs = intra.newComponents ++ intra.effects.flatMap(deps)
        succs.foreach(succ => visited.remove(succ))
        succs.foreach(succ => worklist.enqueue(succ))
      }
    }
  }
}
