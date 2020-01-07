package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.Annotations._
import scalaam.util.MonoidImplicits._

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

  // keep track of all components in the analysis
  @mutable var allComponents: Set[Component]                  = Set(initialComponent)
  protected def registerNewComponents(cmps: Set[Component])   = allComponents ++= cmps

  // keep track of the 'main dependencies' between components (currently, only used for the web visualisation)
  @mutable var dependencies:  Map[Component, Set[Component]]  = Map()

  // inter-analysis using a simple worklist algorithm
  @mutable var work:          Set[Component]                  = Set(initialComponent)
  @mutable var visited:       Set[Component]                  = Set()
  def finished(): Boolean = work.isEmpty
  def step(): Unit = {
    // take the next component
    val current = work.head
    work -= current
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyze()
    // add the successors to the worklist
    val newComponents = intra.components.filterNot(visited)
    val componentsToUpdate = intra.deps.flatMap(deps)
    val succs = newComponents ++ componentsToUpdate
    // update the analysis
    work ++= succs
    visited += current
    dependencies += (current -> intra.components)
    registerNewComponents(newComponents)
  }
  def analyze(timeout: Timeout.T = Timeout.none): Unit =
    while(!finished() && !timeout.reached) {
      step()
    }
}

abstract class IncrementalModAnalysis[Expr <: Expression](program: Expr) extends ModAnalysis(program)
                                                                            with IndirectComponents[Expr] {

  // A module refers to the lexical, static counterpart of a component (i.e. to a function definition).
  type Module = Identity // TODO: How to coop with changes of position upon source code changes?

  // Type of 'actual components'.
  type ComponentData <: LinkedComponent
  /** A linked component has a reference to its lexical module.
   *  By knowing from which module a component stems, it should become easier to find the components affected by a change.
   **/
  trait LinkedComponent {
    def module: Module // Reference to the position of the statical module in which this component has its roots.
  }

  // Map static modules to dynamic components.
  @mutable var mMap: Map[Module, Set[Component]] = Map()

  trait IntraAnalysis extends super.IntraAnalysis {
    override def spawn(cmp: Component): Unit = {
      super.spawn(cmp)
      mMap = mMap + (cmp.module -> (mMap(cmp.module) + cmp))
    }
  }

  // TODO: should we just implement an 'update' method that adds the required components to the work list and updates all state to account for the program changes?

  /**
   * Reanalyses a given program starting from the set of components for which a change was detected.
   * Does not assume the work list to be empty, but will result in the work list being empty.
   **/
  def reanalyze(components: Set[Component], timeout: Timeout.T = Timeout.none): Unit = {
    // We can make use of the visited set and all data from the previous analysis that are still present.
    // Work is not necessarily empty when reanalyze is called (due to the step method which is publicly available).
    // However, when this procedure terminates, work is guaranteed to be empty.
    work ++= components
    analyze(timeout)
  }

  /**
   * Updates the state of the analysis, including all components. TODO
   */
  def updateAnalysisState(): Unit = {}
}

abstract class AdaptiveModAnalysis[Expr <: Expression](program: Expr) extends ModAnalysis(program) {

  // parameterized by an alpha function, which further 'abstracts' components
  // alpha can be used to drive an adaptive strategy for the analysis
  protected def alpha(cmp: Component): Component
  // dependencies might require further abstraction too; subclasses can override this as needed ...
  protected def alphaDep(dep: Dependency): Dependency = dep
  // based on this definition of alpha, we can induce 'compound versions' of this function
  def alphaSet[A](alphaA: A => A)(set: Set[A]): Set[A] = set.map(alphaA)
  def alphaMap[K, V : Monoid](alphaK: K => K, alphaV: V => V)(map: Map[K,V]): Map[K,V] =
    map.foldLeft(Map[K,V]().withDefaultValue(Monoid[V].zero)) { case (acc,(key,vlu)) =>
      val keyAbs = alphaK(key)
      acc + (keyAbs -> Monoid[V].append(acc(keyAbs),alphaV(vlu)))
    }

  // when alpha changes, we need to call this function to update the analysis' components
  def onAlphaChange(): Unit = {
    work            = alphaSet(alpha)(work)
    visited         = alphaSet(alpha)(visited)
    allComponents   = alphaSet(alpha)(allComponents)
    dependencies    = alphaMap(alpha,alphaSet(alpha))(dependencies)
    deps            = alphaMap(alphaDep,alphaSet(alpha))(deps)
  }
}

trait AlphaCaching[Expr <: Expression] extends AdaptiveModAnalysis[Expr] {

  // keep a cache between a component and its most recent abstraction
  private var cache = Map[Component,Component]()
  // look in the cache first, before applying a potentially complicated alpha function
  abstract override def alpha(cmp: Component): Component = cache.get(cmp) match {
    case Some(cmpAbs) => cmpAbs
    case None =>
      val cmpAbs = super.alpha(cmp)
      cache = cache + (cmp -> cmpAbs)
      cmpAbs
  }
  // when alpha is updated, the cache needs to be cleared
  override def onAlphaChange(): Unit = {
    cache = Map[Component,Component]()
    super.onAlphaChange()
  }
}
