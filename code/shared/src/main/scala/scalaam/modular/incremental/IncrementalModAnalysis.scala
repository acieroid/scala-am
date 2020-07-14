package scalaam.modular.incremental

import scalaam.core._
import scalaam.language.change._
import scalaam.language.change.CodeVersion._
import scalaam.modular._
import scalaam.util.benchmarks.Timeout

trait IncrementalModAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr] { //} with MutableIndirectComponents[Expr] { // MutableIndirectComponents are no longer needed, as there are no updates to modules (the original program already contains the changes).

  var version: Version = Old
  private var affectedCheck: Set[Component] = Set() // Note that actually, this makes no sense as in the initial version of the program, you don't have change information. Can be used to check correctness.
  private var mapping: Map[Expr, Set[Component]] = Map().withDefaultValue(Set()) // Keeps track of which components depend on an expression.

  /** Register a component as being affected by a program change.
   *  This is needed e.g. for ModConc, where affected components cannot be determined lexically/statically.
   *  This method should be called when a change expression is analysed.
   *  Synchronisation should be applied when the analysis is run concurrently!
   */
  @deprecated("Should only be used for verification (correctness) or be removed completely.")
  def registerAffected(cmp: Component): Unit = affectedCheck = affectedCheck + cmp

  /** Register that a component is depending on a given expression in the program.
   * This is needed e.g. for ModConc, where affected components cannot be determined lexically/statically.
   * This method should be called for any expression that is analysed.
   * Synchronisation should be applied when the analysis is run concurrently!
   */
  def registerComponent(expr: Expr, component: Component): Unit = mapping = mapping + (expr -> (mapping(expr) + component))

  /** Queries the program for `change` expressions and returns the expressions (within the given) that were affected by the change. */
  def findUpdatedExpressions(expr: Expr): Set[Expr] = expr match {
    case e: ChangeExp[Expr] => Set(e.old) // Assumption: change expressions are not nested.
    case e => e.subexpressions.asInstanceOf[List[Expr]].flatMap(findUpdatedExpressions).toSet
  }

  /** Perform an incremental analysis of the updated program, starting from the previously obtained results. */
  def updateAnalysis(timeout: Timeout.T): Unit = {
    version = New // Make sure the new program version is analysed upon reanalysis (i.e. 'apply' the changes).
    val affected = findUpdatedExpressions(program).flatMap(mapping)

    assert(affected == affectedCheck)
    /*
    if (! (affected == affectedCheck)) {
      System.err.println("Expected: ")
      affectedCheck.foreach(e => System.err.println(s"* $e"))
      System.err.println("Found: ")
      affected.foreach(e => System.err.println(s"* $e"))
      System.err.println("Mapping: ")
      throw new Exception("Non-matching sets when detecting affected components.")
    }
    */

    affected.foreach(addToWorkList) // Affected should be cleared when there are multiple successive incremental analysis steps.
    analyze(timeout)
  }
}