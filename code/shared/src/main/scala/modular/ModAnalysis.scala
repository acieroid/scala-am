package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.Annotations._
import scalaam.util.MonoidImplicits._

abstract class ModAnalysis[Expr <: Expression](var prog: Expr) {

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

  protected def processDependency(dep: Dependency): Unit = work ++= deps(dep)
  protected def processDependencies(deps: Set[Dependency]): Unit =
    deps.foreach(processDependency)
  protected def processNewComponents(cmps: Set[Component]): Unit = {
    work ++= cmps
    allComponents ++= cmps
  }

  // keep track of all components in the analysis
  @mutable var allComponents: Set[Component]                  = Set(initialComponent)
  // keep track of the 'main dependencies' between components (currently, only used for the web visualisation)
  @mutable var dependencies:  Map[Component, Set[Component]]  = Map().withDefaultValue(Set.empty)
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
    processNewComponents(newComponents)
    processDependencies(intra.deps)
    // update the analysis
    visited += current
    dependencies += (current -> intra.components)
  }
  def analyze(timeout: Timeout.T = Timeout.none): Unit =
    while(!finished() && !timeout.reached) {
      step()
    }
}

abstract class IncrementalModAnalysis[Expr <: Expression](var progr: Expr) extends ModAnalysis(progr)
                                                                              with MutableIndirectComponents[Expr] {

  // A module refers to the lexical, static counterpart of a component (i.e. to a function definition).
  type Module = Identity

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

  def updateResults(newProgram:      Expr,
                    moduleDelta:     Map[Module, Option[(Module, Expr)]],
                    modifiedModules: Set[Module],
                    timeout:         Timeout.T = Timeout.none): Unit = {

    // Assume the previous analysis has been finished.
    // This assumption is not really necessary. Hence, in theory, the program could be updated and reanalysed before a prior analysis has finished.
    if (work.nonEmpty) throw new Exception("Previous analysis has not terminated yet.")

    // Update the content of the 'program' variable.
    setProgram(newProgram)
    // Update the ComponentData for all components.
    updateState(moduleDelta)
    // Look which components need reanalysis.
    val toUpdate = computeWork(modifiedModules)
    // Perform the actual reanalysis. We can make use of the visited set and all data from the previous analysis that are still present.
    work ++= toUpdate
    analyze(timeout)
  }

  /**
   * Updates the state of the analysis, including all components.
   * @param moduleDelta A map of old modules to corresponding new expressions of the module (if a corresponding module exist).
   */
  private def updateState(moduleDelta: Map[Module, Option[(Module, Expr)]]): Unit = {
    /* This function updates the internal state of the analysis to account for updates to modules/components.
       The following should be updated:
        * ComponentData
       The following should not be updated (mostly due to the component indirection):
        * Effects (only contain addresses).
        * Addresses (contain component pointers so automatically ok).
        * The dependency map (contains component pointers).
        * The store (maps addresses to values).
        * The MainComponent (ok if the variable "program" is updated).

       IMPORTANT
       The mapping of old modules to new modules must be as such that the lexical addresses are not broken! Hence, a module
       may not be moved to another scope. Inner functions must be rechecked when surrounding functions bind more or less variables,
       but this should follow from the fact that the lexical addresses of the variables change (i.e. the change distiller should work
       on the lexically addressed code!).

       --

       State corresponding to no longer existing modules can be removed. This should avoid spurious computations for modules/components that are no longer in the program.
    */
    allComponents.foreach(c => moduleDelta(c.module).map(m => updateComponent(c, m._2)))

    // All modules for which no updated version is available.
    val removedModules: Set[Module] = moduleDelta.filter(_._2.isEmpty).keySet
    val removedComponents : Set[Component] = removedModules.flatMap(mMap)
    // 'dependencies' is currently only used for the webviz?
    dependencies = dependencies -- removedComponents
    // R/W dependencies.
    deps = deps.view.mapValues(_.diff(removedComponents)).toMap
    allComponents = allComponents -- removedComponents
  }

  /**
   * Computes which components should be reanalysed.<br>
   * These are:<br>
   * * Components corresponding to a module whose body has changed (body excl. inner function definitions? TODO).<br>
   * These are not:<br>
   * * New modules. These should be analysed automatically by default when they are referenced somewhere else?<br>
   * * Deleted modules. TODO Can information (dependencies, ...) containing these modules be deleted? Can the corresponding components be deleted? Probably they should.
   * @param modifiedModules A set of new modules whose body has been modified. TODO: inner definitions?
   */
  private def computeWork(modifiedModules: Set[Module]): Set[Component] = {
    allComponents.filter(c => modifiedModules.contains(c.module))
  }

  // Methods to be implemented/overridden in subclasses.

  /** Updates the content of the 'program' variable. */
  def setProgram(newProgram: Expr): Unit = prog = newProgram

  /**
   * Updates a component given the expression corresponding to the updated module.
   * Since an indirection is used, the corresponding internal data structures should be updated.
   */
  def updateComponent(c: Component, exp: Expr): Unit
}

abstract class AdaptiveModAnalysis[Expr <: Expression](program: Expr) extends ModAnalysis(program) {

  var adapted = false
  // parameterized by an alpha function, which further 'abstracts' components
  // alpha can be used to drive an adaptive strategy for the analysis
  def alpha(cmp: Component): Component
  // dependencies might require further abstraction too; subclasses can override this as needed ...
  protected def alphaDep(dep: Dependency): Dependency = dep
  // based on this definition of alpha, we can induce 'compound versions' of this function
  def alphaSet[A](alphaA: A => A)(set: Set[A]): Set[A] = set.map(alphaA)
  def alphaMap[K,V](alphaV: V => V)(map: Map[K,V]): Map[K,V] = map.view.mapValues(alphaV).toMap
  def alphaMap[K, V : Monoid](alphaK: K => K, alphaV: V => V)(map: Map[K,V]): Map[K,V] =
    map.foldLeft(Map[K,V]().withDefaultValue(Monoid[V].zero)) { case (acc,(key,vlu)) =>
      val keyAbs = alphaK(key)
      acc + (keyAbs -> Monoid[V].append(acc(keyAbs),alphaV(vlu)))
    }

  // when alpha changes, we need to call this function to update the analysis' components
  def onAlphaChange(): Unit = {
    adapted         = true  // hoist the flag
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
