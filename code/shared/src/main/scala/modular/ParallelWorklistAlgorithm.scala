package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

import java.util.concurrent.locks.ReentrantLock

trait ParallelWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] { inter => 

    // determine how many workers need to be used in the parallel algorithm
    def workers: Int = Runtime.getRuntime().availableProcessors()
    // create a threadpool 
    lazy val threadPool = new PausableScheduledThreadPoolExecutor(workers)

    // for thread synchronization
    val lock = new ReentrantLock()
    val done = lock.newCondition()
    def withLock[A](blk: => A) =
        try {
            lock.lock()
            blk
        } finally {
            lock.unlock()
        }

    // keep track for every dependency of its "version number"
    var depVersion = Map[Dependency,Int]().withDefaultValue(0)

    // adding to the worklist = adding to the threadpool
    // also keep track of which components are currently already queued
    var queued = Set[Component]()
    def addToWorkList(cmp: Component): Unit =
        if(!queued.contains(cmp)) {
            queued += cmp
            threadPool.execute(new IntraAnalysisWorker(cmp))
        } 

    private class IntraAnalysisWorker(cmp: Component) extends Runnable {
        def run(): Unit = {
            while(true) { // <- may need to analyze a component multiple times if its dependencies keep changing
                val intra = withLock(intraAnalysis(cmp))
                intra.analyze() // <- this is where we can run things in parallel!
                withLock {
                    intra.commit()
                    if(intra.isDone) { 
                        queued -= cmp
                        if(queued.isEmpty) {
                            done.signal()
                        }
                        return ()
                    }
                }
            }             
        }
    }

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
        def isDone: Boolean = R.forall(dep => inter.depVersion(dep) == intra.depVersion(dep))
    }

    def finished() = withLock { queued.isEmpty }
    def analyze(timeout: Timeout.T) = withLock {
        if(!finished() && !timeout.reached) {
            threadPool.resume()
            timeout.timeLeft match {
                case Some(nanos)    => done.awaitNanos(nanos)
                case None           => done.await()
            }
            if(finished) {
                threadPool.shutdown()
            } else {
                threadPool.pause()
            }
        }
    }

    // immediately add the first component to the (paused) threadpool
    addToWorkList(initialComponent)
}
