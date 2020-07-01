package scalaam.modular

import scalaam.core._
import scalaam.util.SmartHash
import scalaam.util.benchmarks.Timeout

abstract class ModAnalysis[Expr <: Expression](prog: Expr) { inter =>

  // parameterized by a 'intra-component' representation
  type Component
  def initialComponent: Component

  // Retrieve a (possibly modified) version of the program
  def program: Expr = prog

  // some form of "worklist" is required to keep track of which components need to be (re-)analyzed
  // this method is responsible for adding a given component to that worklist
  def addToWorkList(cmp: Component): Unit 

  // the intra-analysis of a component can discover new components
  // when we discover a component that has not yet been analyzed, we add it to the worklist
  // concretely, we keep track of a set `visited` of all components that have already been visited
  var visited: Set[Component] = Set(initialComponent)
  def spawn(cmp: Component) =
    if (!visited(cmp)) { // TODO[easy]: a mutable set could do visited.add(...) in a single call
      visited += cmp
      addToWorkList(cmp)
    }

  // an intra-analysis of a component can read ("register") or write ("trigger") dependencies
  // a dependency represents a part of the global analysis state (such as a location in the global analysis' store)
  // in essence, whenever a dependency is triggered, all registered components for that dependency need to be re-analyzed
  protected trait Dependency extends SmartHash
  // here, we track which components depend on which effects
  var deps: Map[Dependency,Set[Component]] = Map[Dependency,Set[Component]]().withDefaultValue(Set.empty)
  protected def register(target: Component, dep: Dependency): Unit = deps += (dep -> (deps(dep) + target))
  protected def trigger(dep: Dependency) = deps(dep).foreach(addToWorkList)

  // parameterized by an 'intra-component analysis'
  protected def intraAnalysis(component: Component): IntraAnalysis
  protected abstract class IntraAnalysis(val component: Component) { intra => 
    // keep track of:
    // - a set R of dependencies read by this intra-analysis
    // - a set W of dependencies written by this intra-analysis
    // - a set C of components discovered by this intra-analysis
    protected var R = Set[Dependency]()
    protected var W = Set[Dependency]()
    protected var C = Set[Component]()
    protected def register(dep: Dependency): Unit = R += dep
    protected def trigger(dep: Dependency): Unit  = W += dep
    protected def spawn(cmp: Component): Unit     = C += cmp
    // analyses the given component
    // should only update *local* state and not modify the global analysis state directly
    def analyze(timeout: Timeout.T = Timeout.none): Unit
    // pushes the local changes to the global analysis state
    def commit(): Unit = {
      R.foreach(inter.register(component,_))
      W.foreach(dep => if(commit(dep)) inter.trigger(dep))
      C.foreach(inter.spawn)
    }
    def commit(dep: Dependency): Boolean = false  // `ModAnalysis` has no knowledge of dependencies it can commit
  }

  // specific to the worklist algorithm!
  def finished(): Boolean                               // <= check if the analysis is finished
  def analyze(timeout: Timeout.T = Timeout.none): Unit  // <= run the analysis (with given timeout)
}