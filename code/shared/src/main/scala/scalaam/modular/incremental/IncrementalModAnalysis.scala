package scalaam.modular.incremental

import scalaam.core._
import scalaam.language.change.CodeVersion._
import scalaam.modular._
import scalaam.util.benchmarks.Timeout

trait IncrementalModAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr] { //} with MutableIndirectComponents[Expr] { // MutableIndirectComponents are no longer needed, as there are no updates to modules (the original program already contains the changes).

  var version: Version = Old
  private var affected: Set[Component] = Set()

  /** Register a component as being affected by a program change.
   *  This is needed e.g. for ModConc, where affected components cannot be determined lexically/statically.
   *  This method should be called when a change expression is analysed.
   *  Synchronisation should be applied when the analysis is run concurrently!
   */
  def registerAffected(cmp: Component): Unit = affected = affected + cmp

  /** Perform an incremental analysis of the updated program, starting from the previously obtained results. */
  def updateAnalysis(timeout: Timeout.T): Unit = {
    version = New // Make sure the new program version is analysed upon reanalysis (i.e. 'apply' the changes).
    affected.foreach(addToWorkList)
    analyze(timeout)
  }
}