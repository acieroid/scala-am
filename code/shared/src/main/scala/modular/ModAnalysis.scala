package scalaam.modular

import core.Annotations.mutable
import scalaam.core._

import scala.collection.mutable._


abstract class ModAnalysis[Expr <: Expression](program: Expr) {

  // parameterized by a 'intra-component' representation
  type IntraComponent
  val initialComponent: IntraComponent

  // an intra-analysis of a component can cause dependencies that impact the analysis of other components
  // an intra-analysis therefore can:
  // - register a dependency (e.g., when an address is read)
  // - trigger  a dependency (e.g., when an address is written)
  protected trait Dependency
  // here, we track which components depend on which dependencies
  @mutable private val deps = Map[Dependency,Set[IntraComponent]]().withDefaultValue(Set())
  protected def addDependency(component: IntraComponent, dep: Dependency) = deps.get(dep) match {
    case None => deps(dep) = Set(component)
    case Some(components) => components.add(component)
  }

  // parameterized by an 'intra-component analysis'
  protected def intraAnalysis(component: IntraComponent): IntraAnalysis
  protected abstract class IntraAnalysis(val component: IntraComponent) {
    // keep track of dependencies triggered by this intra-analysis
    @mutable private[ModAnalysis] val dependencies = Set[Dependency]()
    protected def triggerDependency(dep: Dependency) = dependencies.add(dep)          // Trigger a dependency.
    protected def registerDependency(dep: Dependency) = addDependency(component, dep)  // Register a dependency.
    // keep track of components called by this intra-analysis
    @mutable private[ModAnalysis] val components = Set[IntraComponent]()
    protected def newComponent(cmp: IntraComponent): Unit = components.add(cmp)
    // analyses the given component
    def analyze(): Unit
  }

  // inter-analysis using a simple worklist algorithm
  @mutable val work          = Set[IntraComponent](initialComponent)
  @mutable val visited       = Set[IntraComponent]()
  @mutable val allComponents = Set[IntraComponent](initialComponent)
  @mutable val componentDeps = Map[IntraComponent, Set[IntraComponent]]()
  def finished(): Boolean = work.isEmpty
  def step(): Unit = {
    // take the next component
    val current = work.head
    work.remove(current)
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyze()
    // add the successors to the worklist but only if they have not been visited before
    val newComponents      = intra.components.filterNot(visited)
    val componentsToUpdate = intra.dependencies.flatMap(deps)
    val todo = newComponents ++ componentsToUpdate
    work ++= todo
    // update the analysis
    visited += current
    allComponents ++= newComponents
    componentDeps(current) = intra.components
  }
  def analyze(): Unit = while(!finished()) { step() }
}
