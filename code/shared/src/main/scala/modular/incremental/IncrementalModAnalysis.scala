package scalaam.modular.incremental

import scalaam.core._
import scalaam.modular._
import scalaam.util.Annotations.mutable
import scalaam.util.Timeout

abstract class IncrementalModAnalysis[Expr <: Expression](var prog: Expr) extends ModAnalysis(prog)
                                                                            with MutableIndirectComponents[Expr] {

  override def program = prog
  /**
      allComponents : Set[Component]
      cMap : Component -> ComponentData
      cMapR : ComponentData -> Component
      dependencies : Component -> Set[Component]
      deps : Dependency -> Set[Component]

      Addr : ComponentAddr + ReturnAddr
      Closure : SchemeExpr x Env
      Component : ComponentPointer
      ComponentAddr : Component x LocalAddr
      ComponentData : Module + (Module x Closure x Option[String] x Context)
      ComponentPointer : Int
      Context : _ + List[Value] (parameteriseerbaar)
      Dependency : Addr
      Identity : UUID
      LocalAddr[C] : (Identifier x Identity) + (SchemeExpr x Identity x C) + (String x Identity)
      Module : Identity
      ReturnAddr : Component
      SchemeEnv : Component (parent pointer)
      SchemeExpr : Identity x Label x List[SchemeExpr] x ...
  */

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

  def updateAnalysis(newProgram: Expr, timeout: Timeout.T = Timeout.none): Unit = {
    // Assume the previous analysis has been finished.
    // This assumption is not really necessary. Hence, in theory, the program could be updated and reanalysed before a prior analysis has finished.
    if (work.nonEmpty) throw new Exception("Previous analysis has not terminated yet.")

    // Update the content of the 'program' variable.
    setProgram(newProgram)

    // TODO: here, the change distiller should be run (after the program has been lexically addressed).

    updateAnalysis(newProgram, ???, ???, timeout)
  }

  private def updateAnalysis(newProgram:      Expr,
                             moduleDelta:     Map[Module, Option[(Module, Expr)]],
                             modifiedModules: Set[Module],
                             timeout:         Timeout.T): Unit = {

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
        * Lexical addresses ("environment") to account for the new identities of identifiers.
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

       TODO: Identifiers contain 'identity information'. This should be updated in the environments so that the identity information matches the new identifiers.
       TODO: How is identity information related to the identity of components? Should this identity information be updated everywhere to make components identical?
            --> Partially: although a component indirection is used, component duplication may appear if component expressions are not updated.
       TODO: How to handle changes of scope? Does it suffice to just update the parent pointer?

       --

       State corresponding to no longer existing modules can be removed. This should avoid spurious computations for modules/components that are no longer in the program.
    */
    allComponents.foreach(c => moduleDelta(c.module).map(m => updateComponent(c, m._2)))
    // TODO: update env.

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
