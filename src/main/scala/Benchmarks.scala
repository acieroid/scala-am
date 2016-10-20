import scala.concurrent.duration.Duration
import Util._

case class MachineConfig(program: String, machine: Config.Machine.Value = Config.Machine.AAM, address: Config.Address.Value = Config.Address.Classical, lattice: Config.Lattice.Value = Config.Lattice.TypeSet, concrete: Boolean = false) {
  override def toString = s"[$program, $machine, $address, $lattice]"
}

abstract class Benchmarks(dir: String, inputs: Iterable[MachineConfig], classify: MachineConfig => String) {
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

  class Worker(timeout: Option[Long]) extends Actor {
    def compute(config: MachineConfig): MachineOutput =  {
      val lattice: SchemeLattice = config.lattice match {
        case Config.Lattice.Concrete => new ConcreteLattice(true)
        case Config.Lattice.TypeSet => new TypeSetLattice(false)
        case Config.Lattice.BoundedInt => new BoundedIntLattice(1000, true)
      }
      implicit val isSchemeLattice = lattice.isSchemeLattice
      val time: TimestampWrapper = if (config.concrete) ConcreteTimestamp else ZeroCFA
      implicit val isTimestamp = time.isTimestamp

      val address: AddressWrapper = config.address match {
        case Config.Address.Classical => ClassicalAddress
        case Config.Address.ValueSensitive => ValueSensitiveAddress
      }
      implicit val isAddress = address.isAddress

      val machine = config.machine match {
        case Config.Machine.AAM => new AAM[SchemeExp, lattice.L, address.A, time.T]
        case Config.Machine.AAMGlobalStore => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](AAMKAlloc)
        case Config.Machine.ConcreteMachine => new ConcreteMachine[SchemeExp, lattice.L, address.A, time.T]
        case Config.Machine.AAC => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](AACKAlloc)
        case Config.Machine.Free => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](P4FKAlloc)
      }

      val sem = new SchemeSemantics[lattice.L, address.A, time.T](new SchemePrimitives[address.A, lattice.L])

      val program = fileContent(s"$dir/${config.program}.scm")
      if (program != null || program.size > 0) {
        try {
          val output = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) { } }) {
            machine.eval(sem.parse(program), sem, false, timeout)
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

  /* Avoids deprecated warning when using Java 8, and call the shutdown method if an older version is used */
  def terminate(system: ActorSystem) = try {
    system.getClass.getMethod("terminate").invoke(system)
  } catch {
    case _: NoSuchMethodException =>
      system.getClass.getMethod("shutdown").invoke(system)
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
          terminate(system)
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

  def run(nworkers: Int, timeout: Option[Long]) {
    import sys.process._
    import scala.util.{Try, Success, Failure}
    import scala.language.postfixOps

    val work = inputs
    val commit = Try(("git log -1" !!).split('\n').head.split(' ')(1)).getOrElse("unknown")
    scala.Console.withOut(logging) { println(s"Running benchmarks for commit $commit with timeout $timeout") }
    println(s"Scheduling ${work.size} items of work")
    val workers = (1 to nworkers).map(i => system.actorOf(Props(new Worker(timeout)), s"worker-$i"))
    val dispatcher = system.actorOf(Props(new Dispatcher(None)), "dispatcher")
    dispatcher ! AddWork(work)
    workers.foreach(dispatcher ! SendWork(_))
  }
  def main(args: Array[String]) {
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) => run(config.workers, config.timeout.map(_.toNanos))
      case None => terminate(system)
    }
  }
}

object SimpleBenchmarks extends Benchmarks("test", {
  val programs = List("ack", "blur", "church", "collatz", "count", "cpstak", "dderiv", "divrec", "eta", "fact", "fib", "gcipd", "grid", "inc", "kcfa2", "kcfa3", "loop2", "mceval", "mut-rec", "mj09", "nqueens", "primtest", "regex", "rotate", "rsa", "scm2java", "sq", "takl", "widen")
  import Config._
  programs.flatMap(p => Set(MachineConfig(p, machine = Machine.AAM)))
  }, (config => config.machine match {
    case Config.Machine.AAM => "AAM"
  }))

object MonoBenchmarks extends Benchmarks("test", {
  val programs = List("ack", "blur", "church", "collatz", "count", "cpstak", "dderiv", "divrec", "eta", "fact", "fib", "gcipd", "grid", "inc", "kcfa2", "kcfa3", "loop2", "mceval", "mut-rec", "mj09", "nqueens", "primtest", "regex", "rotate", "rsa", "scm2java", "sq", "takl", "widen")
  import Config._
  programs.flatMap(p =>
    Set(MachineConfig(p, machine = Machine.AAM),
      MachineConfig(p, machine = Machine.AAMGlobalStore)))
}, (config => config.machine match {
  case Config.Machine.AAM => "AAM"
  case Config.Machine.AAMGlobalStore => "AAM+GS"
}))

object AAMAACP4FBenchmarks extends Benchmarks("test", {
  val programs = List("ack", "blur", "church", "collatz", "count", "cpstak", "dderiv", "divrec", "eta", "fact", "fib", "gcipd", "grid", "inc", "kcfa2", "kcfa3", "loop2", "mceval", "mut-rec", "mj09", "nqueens", "primtest", "regex", "rotate", "rsa", "scm2java", "sq", "takl", "widen")
  import Config._
  programs.flatMap(p =>
    Set(MachineConfig(p, machine = Machine.AAMGlobalStore),
      MachineConfig(p, machine = Machine.AAC),
      MachineConfig(p, machine = Machine.Free)))
}, (config => config.machine match {
  case Config.Machine.AAMGlobalStore => "AAM"
  case Config.Machine.AAC => "AAC"
  case Config.Machine.Free => "P4F"
}))

/*
object ConcurrentMonoBenchmarks extends Benchmarks("concurrent", {
  val programs = List("count2", "count3", "count4", "count5", "count6", "count7", "count8", "count9", "count10", "count11", "count12", "count13", "count14", "count15",
    "dekker", "fact2",
    "atomicityviolation", "atomicityviolation2", "mysqlatomicity", "orderviolation", "writewriteorderviolation",
    "fs2", "fs3", "fs4", "fs5", "fs6", "fs7", "fs8", "fs9", "fs10", "fs11", "fs12", "fs13", "fs14", "fs15",
    "incdec2", "incdec3", "incdec4", "incdec5", "incdec6",
    "indexer2", "indexer3", "indexer4", "indexer5", "indexer6", "indexer7", "indexer8", "indexer9", "indexer10", "indexer11", "indexer12", "indexer13", "indexer14", "indexer15",
    "mutex2", "mutex3", "mutex4", "mutex5", "mutex6",
    "pcounter2", "pcounter3", "pcounter4", "pcounter5", "pcounter6", "pcounter7", "pcounter8", "pcounter9", "pcounter10", "pcounter11", "pcounter12", "pcounter13", "pcounter14", "pcounter15",
    "philosophers2", "philosophers3", "philosophers4", "philosophers5", "philosophers6",
    "producer",
    "race2", "race3", "race4", "race5", "race6",
    "readers2",
    "lastzero2"
  )
  import Config._
  programs.flatMap(p =>
    Set(MachineConfig(p, machine = Machine.ConcurrentAAM),
      MachineConfig(p, machine = Machine.ConcurrentAAMGlobalStore)))
}, (config => config.machine match {
  case Config.Machine.ConcurrentAAM => "base"
  case Config.Machine.ConcurrentAAMGlobalStore => "base+GS"
}))

object ConcurrentExplorationBenchmarks extends Benchmarks("concurrent", {
  val programs = List("count2", "count3", "count4",
    "dekker", "fact2",
    "atomicityviolation", "atomicityviolation2", "mysqlatomicity", "orderviolation", "writewriteorderviolation",
    "fs2", "fs3", "fs4", "indexer2", "indexer3", "indexer4",
    "incdec2", "incdec3", "incdec3", "incdec4",
    "mutex2", "mutex3", "mutex4",
    "pcounter2", "pcounter3", "pcounter4",
    "philosophers2", "philosophers3", "producer", "race2", "race3", "race4", "race5",
    "readers2", "lastzero2"
  )
  import Config._
  programs.flatMap(p =>
    Set(MachineConfig(p, machine = Machine.ConcurrentAAMGlobalStore, exploration = OneInterleaving),
      MachineConfig(p, machine = Machine.ConcurrentAAMGlobalStore, exploration = AllInterleavings),
      MachineConfig(p, machine = Machine.ConcurrentAAMGlobalStore, exploration = InterferenceTrackingNaive),
      MachineConfig(p, machine = Machine.ConcurrentAAMGlobalStore, exploration = InterferenceTrackingPath(Some(2))))
  )},
  (config => config.exploration match {
    case OneInterleaving => "one"
    case AllInterleavings => "all"
    case InterferenceTrackingNaive => "naive"
    case InterferenceTrackingSet => "set"
    case InterferenceTrackingPath(Some(n)) => s"path($n)"
    case InterferenceTrackingPath(None) => "path"
  }))
 */
