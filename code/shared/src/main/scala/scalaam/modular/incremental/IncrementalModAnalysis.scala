package scalaam.modular.incremental

import scalaam.core._
import scalaam.language.change._
import scalaam.language.change.CodeVersion._
import scalaam.modular._
import scalaam.util.benchmarks.Timeout

trait IncrementalModAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr] { //} with MutableIndirectComponents[Expr] { // MutableIndirectComponents are no longer needed, as there are no updates to modules (the original program already contains the changes).

  var version: Version = Old
  private var mapping: Map[Expr, Set[Component]] = Map().withDefaultValue(Set()) // Keeps track of which components depend on an expression.

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
  /*
  def findUpdatedExpressions(expr: Expr): Set[Expr] = {
    var work: List[Expr] = List(expr)
    var resu: List[Expr] = List()
    while (work.nonEmpty) {
      val fst :: rest = work
      work = rest
      fst match {
        case e: ChangeExp[Expr] => resu = e.old :: resu
        case e => work = SmartAppend.sappend(work, e.subexpressions.asInstanceOf[List[Expr]])
      }
    }
    resu.toSet
  }
  */

  /** Perform an incremental analysis of the updated program, starting from the previously obtained results. */
  def updateAnalysis(timeout: Timeout.T): Unit = {
    version = New // Make sure the new program version is analysed upon reanalysis (i.e. 'apply' the changes).
    val affected = findUpdatedExpressions(program).flatMap(mapping)
    affected.foreach(addToWorkList) // Affected should be cleared when there are multiple successive incremental analysis steps.
    analyze(timeout)
  }
}