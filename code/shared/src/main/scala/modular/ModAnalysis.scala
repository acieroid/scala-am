package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.Annotations._

abstract class ModAnalysis[Expr <: Expression](prog: Expr) {

  // parameterized by a 'intra-component' representation
  type Component
  def initialComponent: Component

  // Retrieve a (possibly modified) version of the program
  def program: Expr = prog

  // an intra-analysis of a component can cause dependencies that impact the analysis of other components
  // an intra-analysis therefore can:
  // - register a dependency on an effect (e.g., when it reads an address)
  // - trigger an effect (e.g., when it writes to an address)
  protected trait Dependency
  // here, we track which components depend on which effects
  @mutable var deps: Map[Dependency,Set[Component]] =
    Map[Dependency,Set[Component]]().withDefaultValue(Set.empty)
  protected def addDep(target: Component, dep: Dependency): Unit =
    deps += (dep -> (deps(dep) + target))

  // parameterized by an 'intra-component analysis'
  protected def intraAnalysis(component: Component): IntraAnalysis
  protected abstract class IntraAnalysis(val component: Component) {
    // keep track of dependencies triggered by this intra-analysis
    @mutable private[ModAnalysis] var deps = Set[Dependency]()
    protected def triggerDependency(dep: Dependency): Unit = deps += dep
    protected def registerDependency(dep: Dependency): Unit = addDep(component, dep)
    // keep track of components called by this intra-analysis
    @mutable private[ModAnalysis] var components = Set[Component]()
    protected def spawn(cmp: Component): Unit = components += cmp
    // analyses the given component
    def analyze(): Unit
  }

  // technically, it is also possible to trigger a dependency in the inter-analysis itself
  protected def triggerDependency(dep: Dependency): Unit = addToWorkList(deps(dep))

  implicit def cmpOrdering: Ordering[Component]
  protected def addToWorkList(cmps: Iterable[Component]) = {
    val sorted = cmps.toList.sorted
    workList = workList.add(sorted)
  }

  // keep track of all components in the analysis
  @mutable var allComponents: Set[Component]                  = Set(initialComponent)
  // keep track of the 'main dependencies' between components (currently, only used for the web visualisation)
  @mutable var dependencies:  Map[Component, Set[Component]]  = Map().withDefaultValue(Set.empty)
  // inter-analysis using a simple workListlist algorithm
  @mutable var workList:      WorkList[Component]             = WorkList(initialComponent)
  @mutable var visited:       Set[Component]                  = Set()
  @mutable var intra:         IntraAnalysis                   = null
  @mutable var newComponents: Set[Component]                  = Set()
  @mutable var componentsToUpdate: Set[Component]             = Set()
  def finished(): Boolean = workList.isEmpty
  def step(): Unit = {
    // take the next component
    val current = workList.head
    workList = workList.tail
    // do the intra-analysis
    intra = intraAnalysis(current)
    intra.analyze()
    // add the successors to the worklist
    newComponents = intra.components.filterNot(visited)
    componentsToUpdate = intra.deps.flatMap(deps)
    val succs = newComponents ++ componentsToUpdate
    // update the analysis
    addToWorkList(succs)
    visited += current
    allComponents ++= newComponents
    dependencies += (current -> (dependencies.getOrElse(current,Set()) ++ intra.components))
  }
  def analyze(timeout: Timeout.T = Timeout.none): Unit =
    while(!finished() && !timeout.reached) {
      step()
    }
}
