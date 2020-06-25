package scalaam.modular

import scalaam.core._
import scalaam.util.benchmarks.Timeout

trait SequentialWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] {
  // we can choose what kind of worklist to pick
  def emptyWorkList: WorkList[Component] 
  // adding elements to the worklist
  var workList: WorkList[Component] = emptyWorkList.add(initialComponent)
  def addToWorkList(cmp: Component) = workList = workList.add(cmp)
  def finished(): Boolean = workList.isEmpty
  // a single step in the worklist algorithm iteration
  def step(timeout: Timeout.T): Unit = {
    // take the next component
    val current = workList.head
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyze(timeout)
    intra.commit()
    workList = workList.tail // By doing this here, we ensure the analysis can be correctly restarted after a timeout.
  }
  // step until worklist is empty or timeout is reached
  def analyze(timeout: Timeout.T = Timeout.none): Unit =
    while(!finished() && !timeout.reached) {
      step(timeout)
    }
}

trait LIFOWorklistAlgorithm[Expr <: Expression] extends SequentialWorklistAlgorithm[Expr] {
  def emptyWorkList = LIFOWorkList()
}

trait FIFOWorklistAlgorithm[Expr <: Expression] extends SequentialWorklistAlgorithm[Expr] {
  def emptyWorkList = FIFOWorkList()
}

trait RandomWorklistAlgorithm[Expr <: Expression] extends SequentialWorklistAlgorithm[Expr] {
  def emptyWorkList = RandomWorkList()
}