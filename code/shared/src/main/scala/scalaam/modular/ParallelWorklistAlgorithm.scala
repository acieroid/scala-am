package scalaam.modular

import scalaam.core._
import scalaam.util.benchmarks.Timeout
import scala.collection.mutable.PriorityQueue

class PriorityThreadPool(n: Int) {

  private val workerThreads: List[Worker] = List.tabulate(n)(spawnWorker)
  private lazy val queue: PriorityQueue[Task] = PriorityQueue.empty(Ordering.by(_.priority))

  def submit(priority: Int)(bdy: => Unit) = {
    val task = Task(() => bdy, priority)
    queue.synchronized {
      queue.enqueue(task)
      queue.notify()
    }
  }

  def terminate() = workerThreads.foreach { t =>
    t.interrupt()
    t.join()
  }

  private def pop() = queue.synchronized {
    while(queue.isEmpty) {
      queue.wait()
    }
    queue.dequeue()
  }

  private def spawnWorker(i: Int) = {
    val worker = new Worker(i)
    worker.start()
    worker
  }

  private case class Task(run: () => Unit, priority: Int)

  private class Worker(i: Int) extends Thread(s"worker-thread-$i") {
    override def run(): Unit = 
      try {
        while(true) { pop().run() }
      } catch {
        case _: InterruptedException => ()
      }
  }
}

trait ParallelWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] 
                                                       with GlobalStore[Expr] { inter =>
  
  def workers: Int // <- number of workers for the threadpool
  val pool: PriorityThreadPool = new PriorityThreadPool(workers)                                                     
  def closePool() = pool.terminate()

  var currentTimeout: Timeout.T = _                             // <- we only set the timeout upon calling `analyze`

  //
  // WORKERS
  //

  @volatile var depth: Map[Component, Int] = Map.empty.withDefaultValue(0)                                                   

  def submit(cmp: Component) = pool.submit(depth(cmp)) {
    val intra = intraAnalysis(cmp)
    intra.analyze(currentTimeout)
    if(currentTimeout.reached) {
      pushResult(TimedOut(cmp))
    } else {
      pushResult(Completed(intra))
    }
  }

  //
  // RESULTS
  //

  sealed trait Result                                                                                    
  case class Completed(intra: ParallelIntra)  extends Result
  case class TimedOut(cmp: Component)         extends Result 
  
  object ResultsMonitor
  var results: List[Result] = Nil

  def popResult(): Result = ResultsMonitor.synchronized {
    while(results.isEmpty) { ResultsMonitor.wait() }
    val result = results.head
    results = results.tail 
    result
  }
  def pushResult(res: Result) = ResultsMonitor.synchronized {
    results = res :: results
    ResultsMonitor.notify()
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
      submit(cmp)
    }

  override def spawn(cmp: Component, from: Component): Unit = {
    if (!visited(cmp)) { // TODO[easy]: a mutable set could do visited.add(...) in a single call
      visited += cmp
      depth += cmp -> (depth(from) + 1)
      queued += cmp
      submit(cmp)
    }
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
      submit(intra.component)
    }
  }

  def finished(): Boolean = todo.isEmpty

  def analyze(timeout: Timeout.T): Unit = 
    try {
      // initialize timeout and initial analysis state
      currentTimeout = timeout
      latest = (store, depVersion, deps, visited)
      // fill the worklist with initial items
      todo.foreach(addToWorkList)
      todo = Set.empty
      // main workflow: continuously commit analysis results
      while(queued.nonEmpty) {
        //println(s"QUEUED: ${queued.size} ; RESULTS: ${results.size}")
        popResult() match {
          case Completed(intra) => processTerminated(intra)
          case TimedOut(cmp) => processTimeout(cmp)
        }
      }
    } finally {
      closePool() // wait for all workers to finish
    }

  //
  // INTRA-ANALYSIS
  //

  type GlobalState = (Map[Addr, Value],                 // <- store
                      Map[Dependency, Int],             // <- depVersion
                      Map[Dependency,Set[Component]],   // <- deps
                      Set[Component])                   // <- visited
  @volatile var latest: GlobalState = _
  
  // keep track for every dependency of its "version number"
  var depVersion = Map[Dependency, Int]().withDefaultValue(0)

  // Used for the construction of a new intra-component analysis
  // May only be called when holding the lock, as constructing an analysis entails reading the global analysis state
  def intraAnalysis(component: Component): ParallelIntra
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
