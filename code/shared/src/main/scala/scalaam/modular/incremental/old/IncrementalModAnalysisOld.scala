package scalaam.modular.incremental.old

/*
abstract class IncrementalModAnalysis[Expr <: Expression](var prog: Expr) extends ModAnalysis(prog)
                                                                             with SequentialWorklistAlgorithm[Expr]
                                                                             with MutableIndirectComponents[Expr] {

  import scalaam.modular.components.MutableIndirectComponents
  import scalaam.util.Annotations.mutable
  import scala.concurrent.duration.Duration

  type OldIdn = Identity
  type NewIdn = Identity

  override def program: Expr = prog
  /**
      allComponents : Set[Component]
      cMap : Component -> ComponentData
      cMapR : ComponentData -> Component
      deps : Dependency -> Set[Component]

      Addr : ComponentAddr + ReturnAddr
      Closure : SchemeExpr x Env
      Component : ComponentPointer
      ComponentAddr : Component x LocalAddr
      ComponentData : Module + (Module x Closure x Option[String] x Context)
      ComponentPointer : Int
      Context : _ + List[Value] (parameteriseerbaar)
      Dependency : Addr
      Identity : Long
      LocalAddr[C] : (Identifier x Identity) + (SchemeExpr x Identity x C) + (String x Identity)
      Module : Identity
      ReturnAddr : Component
      SchemeEnv : Component (parent pointer)
  */

  // A module refers to the lexical, static counterpart of a component (i.e. to a function definition).
  type Module = Identity

  // Type of 'actual components'.
  type ComponentData
  /** A linked component has a reference to its lexical module.
   *  By knowing from which module a component stems, it should become easier to find the components affected by a change.
   **/
  def module(cmp: Component): Module

  // Map static modules to dynamic components.
  @mutable var mMap: Map[Module, Set[Component]] = Map()
  // Map static modules to their corresponding expression. TODO is this map useful?
  @mutable var eMap: Map[Module, Expr] = Map()

  trait IntraAnalysis extends super.IntraAnalysis {
    override def spawn(cmp: Component): Unit = {
      super.spawn(cmp)
      mMap = mMap + (module(cmp) -> (mMap(module(cmp)) + cmp))
      if (!eMap.contains(module(cmp))) eMap = eMap + (module(cmp) -> componentExpression(cmp)) // If test can be removed if needed.
    }
  }

  /*
  def updateAnalysis(newProgram: Expr, timeout: Timeout.T = Timeout.none): Unit = {
    // Assume the previous analysis has been finished.
    // This assumption is not really necessary. Hence, in theory, the program could be updated and reanalysed before a prior analysis has finished.
    if (!workList.isEmpty) throw new Exception("Previous analysis has not terminated yet.")

    // Update the content of the 'program' variable.
    setProgram(newProgram)

    // TODO: here, the change distiller should be run (after the program has been lexically addressed).

    updateAnalysis(newProgram, ???, ???, ???, timeout)
  }
   */

  // Convenience class (allows to keep the arguments of functions for now).
  case class UpdateArgs(
                         newProgram:      Expr, // The new version of the program.
                         oldModules:      Set[Module], // The set of modules in the old program.
                         //moduleDelta:     Map[Module, Option[(Module, Expr)]], // Maps old modules to updated ones (if present). // Can be derived from newIdentities.
                         modifiedModules: Set[Module], // The set of modules to be reanalysed. (Represented by the new module information.) TODO can we determine this here?
                         newIdentities:   Map[OldIdn, NewIdn], // Maps old identities to new ones. If no corresponding new identity exists, no entry should be present.
                         newExpressions:  Map[NewIdn, Expr], // Maps new identities to the corresponding expressions.
                         timeout:         Duration // A timeout for the reanalysis
                       )

  private def updateAnalysis(info: UpdateArgs): Unit = {
    updateState(info) // TODO: should this be done 'a priori', or could components & return addresses also be updated 'by need'?
    reRun(info)
  }

  /** Reruns the analysis after the analysis state has been updated. */
  private def reRun(args: UpdateArgs): Unit = {
    // Look which components need reanalysis.
    val toUpdate = computeWork(args.modifiedModules)

    // Perform the actual reanalysis. We can make use of the visited set and all data from the previous analysis that are still present.
    // TODO: should all updated components be added? Maybe not, but since the store already contains data, the updated components may not be analysed if we don't.
    // TODO: the new Main component should be added as well, and be analyzed first (in case new functions are defined).
    workList = workList.addAll(toUpdate)
    analyze(Timeout.start(args.timeout))
  }

  /**
   * Updates the state of the analysis, including all components.
   */
  //@param moduleDelta A map of old modules to corresponding new expressions of the module (if a corresponding module exist).
  private def updateState(args: UpdateArgs): Unit = {
    /* This function updates the internal state of the analysis to account for updates to modules/components.
       In the following parts, expressions should be updated:
          * Components: both the expression and context,
          * The "prog" variable,
          * Addresses.
          * Lexical addresses ("environment") to account for the new identities of identifiers,
          * Effects (contain addresses),
          * The store (maps addresses to values).
          Updates should happen to identities, expressions, ...
       Should not be updated:
          * The dependency map (contains component pointers),
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
       TODO: Is it easier to change the data structures to account for new identities or to change the expression to contain the old identities?

       --

       State corresponding to no longer existing modules can be removed. This should avoid spurious computations for modules/components that are no longer in the program.
    */

    // Update the content of the 'program' variable.
    setProgram(args.newProgram)

   // allComponents.foreach(c => moduleDelta(c.mod).map(m => updateComponent(c, m._2)))
    updateIdentities(args.newIdentities)

    // All modules for which no updated version is available.
    //val removedModules: Set[Module] = moduleDelta.filter(_._2.isEmpty).keySet
    //val removedComponents : Set[Component] = removedModules.flatMap(mMap)
    // 'dependencies' is currently only used for the webviz?
    //dependencies = dependencies -- removedComponents
    // R/W dependencies.
    //deps = deps.view.mapValues(_.diff(removedComponents)).toMap
    //allComponents = allComponents -- removedComponents
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
    visited.filter(c => modifiedModules.contains(module(c)))
  }

  // Methods to be implemented/overridden in subclasses.

  /** Updates the content of the 'program' variable. */
  def setProgram(newProgram: Expr): Unit = prog = newProgram

  /**
   * Updates a component given the expression corresponding to the updated module.
   * Since an indirection is used, the corresponding internal data structures should be updated.
   */
  def updateComponent(c: Component, exp: Expr): Unit

  /** Returns the lambda expression of the given component. */
  def componentExpression(c : Component): Expr

  /**
   * Updates all identities in the analysis' data structures to correspond with the identities assigned to the new AST.
   */
  def updateIdentities(newIdentities: Map[Identity, Identity]): Unit = {}
}
*/