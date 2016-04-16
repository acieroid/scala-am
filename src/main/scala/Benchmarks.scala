object BenchmarksConfig {
  case class Configuration(workers: Int = 1, timeout: Option[Long] = None, random: Int = 10, skipAll: Boolean = false, skipAbstract: Boolean = false, skipConcrete: Boolean = false, skipOne: Boolean = false, skipDPOR: Boolean = false, bound: Option[Int] = None)

  val parser = new scopt.OptionParser[Configuration]("scala-am") {
    head("scala-am", "0.0")
    opt[Int]('w', "workers") action { (x, c) => c.copy(workers = x) } text("Number of workers to run the benchmarks on (1 by default)")
    opt[Int]('r', "random") action { (x, c) => c.copy(random = x) } text("Number of random interleavings explored (10 by default)")
    opt[Int]('b', "bound") action { (x, c) => c.copy(bound = Some(x)) } text("Bound for interference tracking (none by default))")
    opt[Unit]('s', "skip-all") action { (x, c) => c.copy(skipAll = true) } text("Skip computing all interleavings")
    opt[Unit]('a', "skip-abstract") action { (x, c) => c.copy(skipAbstract = true) } text("Skip computing in the abstract")
    opt[Unit]('c', "skip-concrete") action { (x, c) => c.copy(skipConcrete = true) }
    opt[Unit]('o', "skip-one") action { (x, c) => c.copy(skipOne = true) }
    opt[Unit]('d', "skip-dpor") action { (x, c) => c.copy(skipDPOR = true) }
    opt[Config.Time]('t', "timeout") action { (x, c) => c.copy(timeout = Some(x.nanoSeconds)) } text("Timeout (none by default)")
  }
}

object AbstractLattice extends BoundedIntLattice(1000, false)

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

case class MachineConfig(program: String, machine: Config.Machine.Value, address: Config.Address.Value, lattice: Config.Lattice.Value, concrete: Boolean, exploration: ExplorationType, timeout: Option[Long]) {
  override def toString = s"[$program, $machine, $exploration, $address, $lattice]"
}

abstract class Benchmarks(dir: String, inputs: Set[MachineConfig], classify: MachineConfig => String, nworkers: Int) {
  val now = java.util.Calendar.getInstance.getTime
  val timeformat = new java.text.SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
  val stdout = scala.Console.out
  val fileout = new java.io.FileOutputStream(new java.io.File(s"benchmarks-${timeformat.format(now)}.log"))
  val logging = new java.io.OutputStream { override def write(b: Int) { stdout.write(b); fileout.write(b) } }

  case class MachineOutput(time: Double, states: Int, timedOut: Boolean) {
    override def toString = if (timedOut) { s"/, ${states}+" } else { f"$time%.2f, $states" }
  }

  import akka.actor.{ActorRef, ActorSystem, Props, Actor, Inbox }
  import scala.concurrent.duration._

  case class Computation(config: MachineConfig)
  case class Result(in: MachineConfig, out: MachineOutput)
  case class AddWork(items: Iterable[MachineConfig])
  case class SendWork(actor: ActorRef)

  class Worker extends Actor {
    def compute(config: MachineConfig): MachineOutput =  {
      val lattice: Lattice = config.lattice match {
        case Config.Lattice.Concrete => ConcreteLattice
        case Config.Lattice.ConcreteNew => new ConcreteLatticeNew(true)
        case Config.Lattice.TypeSet => new TypeSetLattice(false)
          case Config.Lattice.BoundedInt => new BoundedIntLattice(1000, true)
      }
      implicit val isAbstractValue = lattice.isAbstractValue
      val time: TimestampWrapper = if (config.concrete) ConcreteTimestamp else ZeroCFA
      implicit val isTimestamp = time.isTimestamp

      val address: AddressWrapper = config.address match {
        case Config.Address.Classical => ClassicalAddress
        case Config.Address.ValueSensitive => ValueSensitiveAddress
      }
      implicit val isAddress = address.isAddress

      val machine = config.machine match {
        case Config.Machine.AAM => new AAM[SchemeExp, lattice.L, address.A, time.T]
        case Config.Machine.AAMMonoGlobalStore => new AAMMonoGlobalStore[SchemeExp, lattice.L, address.A, time.T]
        case Config.Machine.ConcreteMachine => new ConcreteMachine[SchemeExp, lattice.L, address.A, time.T]
        case Config.Machine.AAC => new AAC[SchemeExp, lattice.L, address.A, time.T]
        case Config.Machine.Free => new Free[SchemeExp, lattice.L, address.A, time.T]
        case Config.Machine.ConcurrentAAM => new ConcurrentAAM[SchemeExp, lattice.L, address.A, time.T, ContextSensitiveTID](config.exploration)
        case Config.Machine.ConcurrentAAMGlobalStore => new ConcurrentAAMGlobalStore[SchemeExp, lattice.L, address.A, time.T, ContextSensitiveTID](config.exploration)
      }

      val sem = if (config.machine == Config.Machine.ConcurrentAAM || config.machine == Config.Machine.ConcurrentAAMGlobalStore) {
        new ConcurrentSchemeSemantics[lattice.L, address.A, time.T, ContextSensitiveTID]
      } else {
        new SchemeSemantics[lattice.L, address.A, time.T]
      }

      val program = Main.fileContent(s"$dir/${config.program}.scm")
      if (program != null || program.size > 0) {
        try {
          val output = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) { } }) {
            machine.eval(sem.parse(program), sem, false, config.timeout)
          }
          MachineOutput(output.time, output.numberOfStates, output.timedOut)
        } catch {
          case e: Throwable => {
            println(s"Benchmark ${config.program} failed! (config: $config, error: $e)")
              MachineOutput(0, 0, false)
          }
        }
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

  case class Results(results: Map[String, Map[String, MachineOutput]], /* maps program name to input classification to program name to output */
    seen: Set[String] /* classes seen */
  ) {
    def add(in: MachineConfig, out: MachineOutput): Results =
      this.copy(results = results + (in.program -> (results(in.program) + (classify(in) -> out))),
      seen = seen + classify(in))
    def print {
      import scala.math.Ordering.String._
      if (!seen.isEmpty) {
        val keys = seen.toList.sortBy(x => x)
        val tab = ("benchmarks" :: keys) :: results.toList.sortBy({ case (name, _) => name }).map({
          case (name, res) => name :: keys.map(k => res.get(k) match {
            case Some(out) => out.toString
            case None => "x"
          })
        })
        println(Tabulator.format(tab))
      }
    }
  }
  object Results {
    def apply(): Results = Results(Map[String, Map[String, MachineOutput]]().withDefaultValue(Map[String, MachineOutput]()), Set())
  }

  class Dispatcher(bound: Option[Int]) extends Actor {
    import scala.collection.immutable.Queue

    private case class State(computing: Int, work: Queue[MachineConfig], results: Results)

    private def sendWork(actor: ActorRef, state: State): Receive = state.work.dequeueOption match {
      case Some((item, remainingWork)) =>
        actor ! Computation(item)
        val newState = state.copy(computing = state.computing + 1, work = remainingWork)
        active(newState)
      case None => {
        if (state.computing == 0) {
          scala.Console.withOut(logging) {
            /* no more work to do, nothing is computing, stop */
            state.results.print
          }
          system.shutdown
        }
        active(state)
      }
    }
    private def active(state: State): Receive = {
      case AddWork(items) => context.become(active(state.copy(work = state.work ++ items)))
      case SendWork(actor) => context.become(sendWork(actor, state))
      case Result(in, out) =>
        scala.Console.withOut(logging) { println(s"$in: $out (${state.work.size + state.computing} remaining)") }
        val newState = state.copy(computing = state.computing-1, results = state.results.add(in, out))
        scala.Console.withOut(logging) { newState.results.print }
        context.become(sendWork(sender, newState))
    }

    def receive = active(State(0, Queue[MachineConfig](), Results()))
  }

  def run() {
    import sys.process._
    import scala.util.{Try, Success, Failure}
    import scala.language.postfixOps

    val work = inputs
    val commit = Try(("git log -1" !!).split('\n').head.split(' ')(1)).getOrElse("unknown")
    scala.Console.withOut(logging) { println(s"Running benchmarks for commit $commit") }
    println(s"Scheduling ${work.size} items of work")
    val workers = (1 to nworkers).map(i => system.actorOf(Props(new Worker), s"worker-$i"))
    val dispatcher = system.actorOf(Props(new Dispatcher(None)), "dispatcher")
    dispatcher ! AddWork(work)
    workers.foreach(dispatcher ! SendWork(_))
  }
  def main(args: Array[String]) = run()
}

object MonoBenchmarks extends Benchmarks("test/", {
  val programs = Set("ack", "blur", "church", "collatz", "count", "cpstak", "dderiv", "divrec", "eta", "fact", "fib", "gcipd", "grid", "inc", "kcfa2", "kcfa3", "loop2", "mceval", "mut-rec", "mj09", "nqueens", "primtest", "regex", "rotate", "rsa", "scm2java", "sq", "takl", "widen")
  import Config._
  val timeout = Some(TimeParser.parse("10s").nanoSeconds)
  programs.flatMap(p =>
    Set(MachineConfig(p, Machine.AAM, Address.Classical, Lattice.TypeSet, false, OneInterleaving, timeout),
      MachineConfig(p, Machine.AAMMonoGlobalStore, Address.Classical, Lattice.TypeSet, false, OneInterleaving, timeout)))
  }, (config => config.machine match {
    case Config.Machine.AAM => "AAM"
    case Config.Machine.AAMMonoGlobalStore => "AAM+GS"
  }), 2)
