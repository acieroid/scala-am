package scalaam.modular

import scalaam.core._
import scala.collection.mutable._


abstract class ModAnalysis[Expr <: Expression](program: Expr) {

  // parameterized by a 'intra-component' representation
  type IntraComponent
  val initial: IntraComponent

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
    // keep track of components called by this intra-analysis
    private[ModAnalysis] val components = Set[IntraComponent]()
    protected def spawn(cmp: IntraComponent): Unit = components.add(cmp)
    // analyses the given component
    def analyze(): Unit
  }

  // inter-analysis using a simple worklist algorithm
  val worklist = Set[IntraComponent](initial)
  val analysed = Set[IntraComponent]()
  val allComponents = Set[IntraComponent](initial)
  val componentDeps = Map[IntraComponent,Set[IntraComponent]]()
  def finished = worklist.isEmpty
  def step() = {
    // take the next component
    val current = worklist.head
    worklist.remove(current)
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyze()
    // add the successors to the worklist
    val newComponents = intra.components.filterNot(analysed)
    val componentsToUpdate = intra.effects.flatMap(deps)
    val succs = newComponents ++ componentsToUpdate
    succs.foreach(succ => worklist.add(succ))
    // update the analysis
    analysed += current
    allComponents ++= newComponents
    componentDeps(current) = intra.components
  }
  def analyze() = while(!finished) { step() }
}
