package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

object ConcurrencyHelpers {
  def withLock[V](lck: ReentrantLock)(bdy: => V): V = 
    try {
      lck.lock()
      bdy 
    } finally {
      lck.unlock()
    }
  implicit def makeRunnable(f: => Unit): Runnable = new Runnable() {
    def run(): Unit = f
  }
}

trait ParallelWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] { inter =>

  import ConcurrencyHelpers._

  def workers: Int = Runtime.getRuntime.availableProcessors() // <- number of workers for the threadpool
  var workerThreadPool: ExecutorService = _                   // <- we only instantiate this upon calling `analyze`
  var currentTimeout: Timeout.T = _                           // <- we only set the timeout upon calling `analyze`

  val globalStateLock = new ReentrantLock() // <- a lock that must be taken before modifying the global analysis state
                                            // TODO[maybe]: could be turned into a R/W lock if necessary

  //
  // RESULTS
  //

  trait Result
  case class Completed(intra: ParallelIntra)  extends Result
  case class TimedOut(cmp: Component)         extends Result
  
  var results = Set.empty[Result]

  def pop(): Result = synchronized {
    while(results.isEmpty) { wait() }
    val next = results.head
    results = results.tail
    next
  }

  def push(res: Result) = synchronized {
    if(results.isEmpty) { notify() }
    results += res
  }

  //
  // WORKER
  //

  private def work(cmp: Component): Unit = {
    val intra = withLock(globalStateLock)(intraAnalysis(cmp))
    intra.analyze(currentTimeout)
    if(currentTimeout.reached) {
      push(TimedOut(cmp))
    } else {
      push(Completed(intra))
    }
  }

  //
  // ANALYSIS COORDINATION
  //

  // two sets to keep track of
  // - components that need to be analyzed later on (i.e., upon the next `analyze` call)
  // - components that are currently scheduled for analysis (i.e., during the current `analyze` call)
  var todo: Set[Component] = Set(initialComponent)
  var queued: Set[Component] = Set.empty

  def addToWorkList(cmp: Component): Unit =
    if (!queued.contains(cmp)) {
      queued += cmp
      workerThreadPool.execute(work(cmp))
    }

  private def processTimeout(cmp: Component): Unit = {
    todo += cmp
    queued -= cmp
  }

  private def processTerminated(intra: ParallelIntra): Unit = {
    withLock(globalStateLock) {
      intra.commit()
    }
    if(intra.isDone) {
      queued -= intra.component
    } else {
      workerThreadPool.execute(work(intra.component))
    }
  }

  def finished(): Boolean = todo.isEmpty

  def analyze(timeout: Timeout.T): Unit = 
    if(!finished()) {
      currentTimeout = timeout
      workerThreadPool = Executors.newFixedThreadPool(workers)
      todo.foreach(addToWorkList)
      todo = Set.empty
      while(queued.nonEmpty) {
        pop() match {
          case Completed(intra) => processTerminated(intra)
          case TimedOut(cmp) => processTimeout(cmp)
        }
      }
      workerThreadPool.shutdown()
    }

  //
  // INTRA-ANALYSIS
  //
  
  // keep track for every dependency of its "version number"
  var depVersion = Map[Dependency, Int]().withDefaultValue(0)

  // Used for the construction of a new intra-component analysis
  // May only be called when holding the lock, as constructing an analysis entails reading the global analysis state
  def intraAnalysis(component: Component): ParallelIntra
  trait ParallelIntra extends IntraAnalysis { intra =>
    val depVersion = inter.depVersion
    override def commit(dep: Dependency): Boolean =
      if (super.commit(dep)) {
        inter.depVersion += dep -> (inter.depVersion(dep) + 1)
        true
      } else {
        false
      }
    def isDone = R.forall(dep => inter.depVersion(dep) == intra.depVersion(dep))
  }
}
