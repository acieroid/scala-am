package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

import java.util.concurrent.locks._
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

object ConcurrencyHelpers {
  def withLock[V](lck: Lock)(bdy: => V): V = 
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

trait ParallelWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] 
                                                       with GlobalStore[Expr] { inter =>

  import ConcurrencyHelpers._

  def workers: Int = Runtime.getRuntime.availableProcessors() // <- number of workers for the threadpool
  var workerThreadPool: ExecutorService = _                   // <- we only instantiate this upon calling `analyze`
  var currentTimeout: Timeout.T = _                           // <- we only set the timeout upon calling `analyze`

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

  type GlobalState = (Map[Addr, Value],                 // <- store
                      Map[Dependency, Int],             // <- depVersion
                      Map[Dependency,Set[Component]],   // <- deps
                      Set[Component])                   // <- visited
  @volatile var latest: GlobalState = _

  private def work(cmp: Component): Unit = {
    val intra = intraAnalysis(cmp)
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
    intra.commit()
    latest = (store, depVersion, deps, visited)
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
      latest = (store, depVersion, deps, visited)
      workerThreadPool = Executors.newFixedThreadPool(workers)
      todo.foreach(addToWorkList)
      todo = Set.empty
      while(queued.nonEmpty) {
        //println(s"QUEUED: ${queued.size} ; RESULTS: ${results.size}")
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
  def intraAnalysis(component: Component): ParallelIntra with GlobalStoreIntra
  trait ParallelIntra extends IntraAnalysis with GlobalStoreIntra { intra =>
    val (latestStore, depVersion, deps, visited) = latest
    store = latestStore
    var toCheck = Set[Dependency]()
    override def commit(dep: Dependency): Boolean =
      if (super.commit(dep)) {
        inter.depVersion += dep -> (inter.depVersion(dep) + 1)
        true
      } else {
        false
      }
    override def register(dep: Dependency): Unit = {
      toCheck += dep
      if(!deps(dep)(component)) {
        R += dep  // only register dependencies that are not yet registered for that component
      }
    }
    override def spawn(cmp: Component): Unit =
      if(!visited(cmp)) {
        C += cmp
      }
    def isDone = toCheck.forall(dep => inter.depVersion(dep) == intra.depVersion(dep))
  }
}
