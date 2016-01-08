object BenchmarksConfig {
  case class Configuration(workers: Int = 1, timeout: Option[Long] = None)

  val parser = new scopt.OptionParser[Configuration]("scala-am") {
    head("scala-am", "0.0")
    opt[Int]('w', "workers") action { (x, c) => c.copy(workers = x) } text("Number of workers to run the benchmarks on (1 by default)")
    opt[Config.Time]('t', "timeout") action { (x, c) => c.copy(timeout = Some(x.nanoSeconds)) } text("Timeout (none by default)")
  }
}

/* From http://stackoverflow.com/questions/7539831/scala-draw-table-to-console */
object Tabulator {
  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else cell.toString.length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRows(rowSeparator: String, rows: Seq[String]): String = (
    rowSeparator ::
    rows.head ::
    rowSeparator ::
    rows.tail.toList :::
    rowSeparator ::
    List()).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = (for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString("+", "+", "+")
}

object Benchmarks {
  case class MachineConfig(name: String, exploration: ExplorationType.Value, concrete: Boolean, timeout: Option[Long]) {
    override def toString = s"[$name, $exploration, $concrete]"
  }
  case class MachineOutput(time: Double, states: Int, timedOut: Boolean) {
    override def toString = if (timedOut) { s"/, ${states}+" } else { f"$time%.2f, $states" }
  }

  val programs = Set(
    "counter-race", "fs", "incdec", "indexer", "mutex-cas", "pcounter", "philosophers", "producer", "simple"
  )

  import akka.actor.{ActorRef, ActorSystem, Props, Actor, Inbox }
  import scala.concurrent.duration._

  case class Computation(config: MachineConfig)
  case class Result(in: MachineConfig, out: MachineOutput)
  case class AddWork(items: Iterable[MachineConfig])
  case class SendWork(actor: ActorRef)

  trait TimestampWrapper {
    type T
    val isTimestamp: Timestamp[T]
  }

  class Worker extends Actor {
    def compute(config: MachineConfig): MachineOutput = config match {
      case MachineConfig(name, exploration, concrete, timeout) =>
        val lattice = if (concrete) { ConcreteLattice } else { TypeSetLattice }
        implicit val isAbstractValue = lattice.isAbstractValue
        val time = if (concrete) {
          new TimestampWrapper {
            type T = ConcreteTimestamp
            val isTimestamp = implicitly[Timestamp[T]]
          }
        } else {
          new TimestampWrapper {
            type T = CFA.ZeroCFA
            val isTimestamp = implicitly[Timestamp[T]]
          }
        }
        implicit val isTimestamp = time.isTimestamp

        val machine = new ConcurrentAAM[SchemeExp, lattice.L, ClassicalAddress, time.T, ContextSensitiveTID](exploration)
        val sem = new ConcurrentSchemeSemantics[lattice.L, ClassicalAddress, time.T, ContextSensitiveTID]
        val program = Main.fileContent(s"concurrent/$name.scm")
        if (program != null || program.size > 0) {
          val output = machine.eval(sem.parse(program), sem, false, timeout)
          MachineOutput(output.time, output.numberOfStates, output.timedOut)
        } else {
          MachineOutput(0, 0, false) // TODO: error output
        }
    }

    def receive = {
      case Computation(config) =>
        val answer = compute(config)
        sender ! Result(config, answer)
    }
  }

  val system = ActorSystem("scala-am-benchmarks")

  import scala.collection.mutable.{Queue, Map => MutableMap}
  class Dispatcher extends Actor {
    val work: Queue[MachineConfig] = Queue()
    val concreteResults: MutableMap[String, MutableMap[ExplorationType.Value, MachineOutput]] = MutableMap[String, MutableMap[ExplorationType.Value, MachineOutput]]()
    val abstractResults: MutableMap[String, MutableMap[ExplorationType.Value, MachineOutput]] = MutableMap[String, MutableMap[ExplorationType.Value, MachineOutput]]()
    var computing: Int = 0

    private def printResults(results: MutableMap[String, MutableMap[ExplorationType.Value, MachineOutput]]) = {
      println(Tabulator.format(List("program", "one", "all", "reduced") :: results.toList.map({
        case (name, res) => name :: List(ExplorationType.OneInterleaving,
          ExplorationType.AllInterleavings, ExplorationType.InterferenceTracking).map(expl =>
          res.get(expl) match {
          case Some(out) => out.toString
          case None => "x"
          })
      })))
    }
    private def sendWork(actor: ActorRef) = if (!work.isEmpty) {
      actor ! Computation(work.dequeue)
      computing += 1
    } else if (computing == 0) {
      /* no more work to do, nothing is computing, stop */
      println("Concrete: ")
      printResults(concreteResults)
      println("Abstract: ")
      printResults(abstractResults)
      system.shutdown
    }
    def receive = {
      case AddWork(items) => work ++= items
      case SendWork(actor) => sendWork(actor)
      case Result(in, out) =>
        computing -= 1
        val results = if (in.concrete) concreteResults else abstractResults
        results.get(in.name) match {
          case Some(m) => m += in.exploration -> out
          case None => results += in.name -> MutableMap(in.exploration -> out)
        }
        println(s"$in: $out")
        sendWork(sender)
    }
  }

  def main(args: Array[String]) {
    BenchmarksConfig.parser.parse(args, BenchmarksConfig.Configuration()) match {
      case Some(config) =>
        println(config)
        val work = programs.flatMap(name =>
          Set(ExplorationType.AllInterleavings, ExplorationType.OneInterleaving, ExplorationType.InterferenceTracking).flatMap(expl =>
            Set(true, false).map(concrete =>
              MachineConfig(name, expl, concrete, config.timeout))))
        val workers = (1 to config.workers).map(i => system.actorOf(Props[Worker], s"worker-$i"))
        val dispatcher = system.actorOf(Props[Dispatcher], "dispatcher")
        dispatcher ! AddWork(work)
        workers.foreach(dispatcher ! SendWork(_))
      case None => ()
    }
  }
}
