package scalaam.modular

import scalaam.core._
import scalaam.util.benchmarks.Timeout
import scala.collection.mutable.PriorityQueue

trait SequentialWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] {
  // we can choose what kind of worklist to pick
  def emptyWorkList: WorkList[Component] 
  // adding elements to the worklist
  var workList: WorkList[Component] = emptyWorkList.add(initialComponent)
  def addToWorkList(cmp: Component) = workList = workList.add(cmp)
  def finished(): Boolean = workList.isEmpty
  // a single step in the worklist algorithm iteration
  var intraCount: Long = 0L
  def step(timeout: Timeout.T): Unit = {
    // take the next component
    val current = workList.head
    workList = workList.tail 
    // do the intra-analysis
    intraCount = intraCount + 1
    val intra = intraAnalysis(current)
    intra.analyze(timeout)
    if (timeout.reached) {
      // analysis timed out => we need to add it to the worklist again
      addToWorkList(current)
    } else {
      // analysis finished properly => commit its changes to the global analysis state
      intra.commit()
    }
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

// TODO: use an immutable priority queue, or reuse SequentialWorklistAlgorithm differently here
trait PriorityQueueWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] {
  // choose the priority ordering of components
  implicit val ordering: Ordering[Component]
  // worklist is a priority queue
  var worklistSet: Set[Component] = Set(initialComponent)
  lazy val worklist: PriorityQueue[Component] = PriorityQueue(initialComponent)
  def push(cmp: Component) = 
    if(!worklistSet.contains(cmp)) {
      worklistSet += cmp
      worklist += cmp
    }
  def pop(): Component = {
    val cmp = worklist.dequeue()
    worklistSet -= cmp
    cmp
  }
  def addToWorkList(cmp: Component): Unit = push(cmp)
  def finished(): Boolean = worklist.isEmpty
  // a single step in the worklist algorithm iteration
  def step(timeout: Timeout.T): Unit = {
    // take the next component
    val current = pop()
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyze(timeout)
    if (timeout.reached) {
      // analysis timed out => we need to add it to the worklist again
      addToWorkList(current)
    } else {
      // analysis finished properly => commit its changes to the global analysis state
      intra.commit()
    }
  } 
  // step until worklist is empty or timeout is reached
  def analyze(timeout: Timeout.T = Timeout.none): Unit =
    while(!finished() && !timeout.reached) {
      step(timeout)
    }
}

trait CallDepthFirstWorklistAlgorithm[Expr <: Expression] extends PriorityQueueWorklistAlgorithm[Expr] {
  var depth: Map[Component, Int] = Map.empty.withDefaultValue(0)
  lazy val ordering: Ordering[Component] = Ordering.by(depth)
  override def spawn(cmp: Component, from: Component): Unit = {
    if (!visited(cmp)) { // TODO[easy]: a mutable set could do visited.add(...) in a single call
      visited += cmp
      depth += cmp -> (depth(from) + 1)
      addToWorkList(cmp)
    }
  }
}