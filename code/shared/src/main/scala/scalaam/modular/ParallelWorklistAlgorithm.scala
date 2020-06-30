package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

import java.util.concurrent.locks.ReentrantLock

trait ParallelWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] { inter =>

  // determine how many workers need to be used in the parallel algorithm
  def workers: Int = Runtime.getRuntime().availableProcessors()
  // create a threadpool
  lazy val threadPool = new PausableThreadPoolExecutor(workers)
  // the currently active timeout
  var currentTimeout: Timeout.T = Timeout.none

  // for thread synchronization
  /** This lock manages synchronisation for all shared state of the analysis: the global store and dependency sets. This lock should be held whenever reading/writing this state. */
  val lock = new ReentrantLock() // TODO: might be replaced by synchronisation on the intra-process anlaysis (inter).
  val done = lock.newCondition() // TODO: might use the condition variable of inter?
  // Convenience method for holding the lock.
  def withLock[A](blk: => A) =
    try {
      lock.lock()
      blk
    } finally {
      lock.unlock()
    }

  // keep track for every dependency of its "version number"
  var depVersion = Map[Dependency, Int]().withDefaultValue(0)

  // adding to the worklist = adding to the threadpool
  // also keep track of which components are currently already queued
  /** Queue should only be accesses while holding the lock. */
  var queued = Set[Component]()

  /** Queue should only be accesses while holding the lock. */
  def addToWorkList(cmp: Component): Unit =
    if (!queued.contains(cmp)) {
      queued += cmp
      threadPool.execute(new IntraAnalysisWorker(cmp))
    }

  private class IntraAnalysisWorker(cmp: Component) extends Runnable {
    def run(): Unit = {
      while (true) { // <- may need to analyze a component multiple times if its dependencies keep changing
        val intra = withLock(intraAnalysis(cmp))
        intra.analyze(currentTimeout) // <- this is where we can run things in parallel!
        if(currentTimeout.reached) {
          // analysis timed out => need to re-analyze the component
          withLock {
            addToWorkList(cmp)
            return
          }
        } else {
          // analysis finished => commit changes to the global analysis state
          withLock {
            intra.commit()
            if (intra.isDone) {
              queued -= cmp
              if (queued.isEmpty) done.signal()
              return
            }
          }
        }
      }
    }
  }

  /** Used for the construction of a new intra-component analysis. May only be called when holding the lock, as this may use the global state. */
  def intraAnalysis(component: Component): ParallelIntra
  trait ParallelIntra extends IntraAnalysis { intra =>
    val depVersion = inter.depVersion

    /** commit may only be called while holding the lock. */
    override def commit(dep: Dependency): Boolean =
      if (super.commit(dep)) {
        inter.depVersion += dep -> (inter.depVersion(dep) + 1)
        true
      } else {
        false
      }

    /** isDone may only be called while holding the lock. */
    def isDone: Boolean = R.forall(dep => inter.depVersion(dep) == intra.depVersion(dep))
  }

  def finished(): Boolean = withLock { queued.isEmpty }
  def analyze(timeout: Timeout.T): Unit = withLock { // Lock required for calling await/awaitNanos.
    if (!finished() && !timeout.reached) {
      currentTimeout = timeout
      threadPool.resume()
      timeout.timeLeft match {
        case Some(nanos) => done.awaitNanos(nanos)
        case None        => done.await()
      }
      if (finished()) {
        threadPool.shutdown()
      } else {
        threadPool.pause() // Disallows new tasks to be run, but WILL CONTINUE RUNNING EXISTING TASKS.
      }
    }
  }

  // immediately add the first component to the (paused) threadpool
  addToWorkList(initialComponent)
}
