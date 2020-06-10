package scalaam.modular

import scalaam.core._
import scalaam.util.benchmarks.Timeout

trait SequentialWorkListAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] {
  // we can choose what kind of worklist to pick
  def emptyWorkList: WorkList[Component] 
  // adding elements to the worklist
  var workList: WorkList[Component] = emptyWorkList.add(initialComponent)
  def addToWorkList(cmp: Component) = workList = workList.add(cmp)
  def finished(): Boolean = workList.isEmpty
  // a single step in the worklist algorithm iteration
  def step(): Unit = {
    // take the next component
    val current = workList.head
    workList = workList.tail
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyze()
    intra.commit()
  }
  // step until worklist is empty or timeout is reached
  def analyze(timeout: Timeout.T = Timeout.none): Unit =
    while(!finished() && !timeout.reached) {
      step()
    }
}

trait LIFOWorklistAlgorithm[Expr <: Expression] extends SequentialWorkListAlgorithm[Expr] {
  def emptyWorkList = LIFOWorkList()
}

trait FIFOWorklistAlgorithm[Expr <: Expression] extends SequentialWorkListAlgorithm[Expr] {
  def emptyWorkList = FIFOWorkList()
}
