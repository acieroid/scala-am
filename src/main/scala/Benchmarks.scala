object BenchmarksConfig {
  case class Configuration(workers: Int = 1, timeout: Option[Long] = None, random: Int = 10)

  val parser = new scopt.OptionParser[Configuration]("scala-am") {
    head("scala-am", "0.0")
    opt[Int]('w', "workers") action { (x, c) => c.copy(workers = x) } text("Number of workers to run the benchmarks on (1 by default)")
    opt[Int]('r', "random") action { (x, c) => c.copy(random = x) } text("Number of random interleavings explored (10 by default)")
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
  case class MachineOutput(time: Double, states: Int, timedOut: Boolean, finalValues: Set[Any]) {
    override def toString = if (timedOut) { s"/, ${states}+" } else { f"$time%.2f, $states" }
  }

  val programs = Set(
    "race2", "race3", "race4", "race5", "race6",
    "fs2", "fs3", "fs4", "fs5", "fs6",
    "incdec2", "incdec3", "incdec4", "incdec5", "incdec6",
    "indexer2", "indexer3", "indexer4", "indexer5", "indexer6",
    "mutex2", "mutex3", "mutex4", "mutex5", "mutex6",
    "pcounter2", "pcounter3", "pcounter4", "pcounter5", "pcounter6",
    "philosophers2", "philosophers3", "philosophers4", "philosophers5", "philosophers6",
    "producer", "simple"
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
          val output = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) { } }) {
            machine.eval(sem.parse(program), sem, false, timeout)
          }
          MachineOutput(output.time, output.numberOfStates, output.timedOut, output.finalValues.map(x => x))
        } else {
          MachineOutput(0, 0, false, Set[Any]()) // TODO: error output
        }
    }

    def receive = {
      case Computation(config) =>
        val answer = compute(config)
        sender ! Result(config, answer)
    }
  }

  val system = ActorSystem("scala-am-benchmarks")

  class Dispatcher extends Actor {
    import scala.collection.immutable.Queue

    val now = java.util.Calendar.getInstance.getTime
    val timeformat = new java.text.SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val stdout = scala.Console.out
    val fileout = new java.io.FileOutputStream(new java.io.File(s"benchmarks-${timeformat.format(now)}.log"))
    val logging = new java.io.OutputStream { override def write(b: Int) { stdout.write(b); fileout.write(b) } }

    private def printResults(results: Map[String, Map[ExplorationType.Value, MachineOutput]]) = {
      import scala.math.Ordering.String._
      println(Tabulator.format(List("program", "one", "all", "reduced") :: results.toList.sortBy({ case (name, _) => name }).map({
        case (name, res) => name :: List(ExplorationType.OneInterleaving,
          ExplorationType.AllInterleavings, ExplorationType.InterferenceTracking).map(expl =>
          res.get(expl) match {
            case Some(out) => out.toString
            case None => "x"
          })
      })))
    }

    private def err(msg: String) = println(s"${scala.io.AnsiColor.RED}$msg${scala.io.AnsiColor.RESET}")

    private def subsumes(abs: Set[Any], conc: Set[Any]): Boolean = {
      conc.forall(c => {
        val a2 = c.asInstanceOf[ConcreteLattice.Element] match {
          case ConcreteLattice.ConcreteInt(v) => TypeSetLattice.isAbstractValue.inject(v)
          case ConcreteLattice.ConcreteFloat(v) => TypeSetLattice.isAbstractValue.inject(v)
          case ConcreteLattice.ConcreteString(v) => TypeSetLattice.isAbstractValue.inject(v)
          case ConcreteLattice.ConcreteChar(v) => TypeSetLattice.isAbstractValue.inject(v)
          case ConcreteLattice.ConcreteSymbol(v) => TypeSetLattice.isAbstractValue.injectSymbol(v)
          case ConcreteLattice.ConcreteBool(v) => TypeSetLattice.isAbstractValue.inject(v)
          case ConcreteLattice.Bottom => TypeSetLattice.isAbstractValue.bottom
          case ConcreteLattice.ConcreteError(v) => TypeSetLattice.isAbstractValue.error(TypeSetLattice.isAbstractValue.inject(v))
          /* Benchmarks shouldn't return closures, thread ids, locks, vectors, so we ignore these values */
          case _ => { err(s"Cannot convert $c to abstract"); TypeSetLattice.isAbstractValue.bottom }
        }
        abs.exists(a => TypeSetLattice.isAbstractValue.subsumes(a.asInstanceOf[TypeSetLattice.L], a2))
      })
    }

    private def checkResults(state: State) = {
      /* invariants being checked, for programs that didn't time out
       1. result from OneInterleaving is contained in AllInterleavings (for concrete and abstract)
       2. result from InterferenceTracking completely matches results from AllInterleavings (for concrete and abstract)
       3. result from InterferenceTracking contains result from OneInterleaving (for concrete and abstract)
       For programs that did time out in AllInterleavings but not in InterferenceTracking, the following invariants are checked instead
       4. results explored in AllInterleavings should be contained in InterferenceTracking (if InterferenceTracking didn't time out)
       5. result exploed in OneInterleaving should be contained in InterferenceTracking
       For programs where OneInterleaving did time out, nothing can be checked.
       Also:
       6. For programs where InterferenceTracking did time out, we check that AllInterleavings did time out as well.
       7. result for some exploration in the abstract subsumes results for another exploration in the concrete
       8. results from every RandomInterleaving run are contained in InterferenceTracking
       */
      programs.foreach(name => {
        println(s"Checking results for $name")
        val conc = state.concreteResults(name)
        val abs = state.abstractResults(name)
        val oneconc = conc(ExplorationType.OneInterleaving)
        val oneabs = abs(ExplorationType.OneInterleaving)
        val allconc = conc(ExplorationType.AllInterleavings)
        val allabs = abs(ExplorationType.AllInterleavings)
        val redconc = conc(ExplorationType.InterferenceTracking)
        val redabs = abs(ExplorationType.InterferenceTracking)
        val randconc = conc(ExplorationType.RandomInterleaving)
        val randabs = abs(ExplorationType.RandomInterleaving)
        println(s"Number of final values: allconc: ${allconc.finalValues.size}, redconc: ${redconc.finalValues.size}, allabs: ${allabs.finalValues.size}, redabs: ${redabs.finalValues.size}")
        /* Concrete */
        if (!oneconc.timedOut && !allconc.timedOut && !redconc.timedOut) {
          if (!oneconc.finalValues.subsetOf(allconc.finalValues)) // 1
            err(s"$name (concrete): one interleaving (${oneconc.finalValues}) not contained in all interleavings (${allconc.finalValues})")
          if (!(redconc.finalValues == allconc.finalValues)) // 2
            err(s"$name (concrete): reduced interleavings (${redconc.finalValues}) do not match all interleavings (${allconc.finalValues})")
          if (!oneconc.finalValues.subsetOf(redconc.finalValues)) // 3
            err(s"$name (concrete): one interleaving (${oneconc.finalValues}) not contained in reduced interleavings (${redconc.finalValues})")
        } else if (allconc.timedOut && !oneconc.timedOut && !redconc.timedOut) {
          if (!allconc.finalValues.subsetOf(redconc.finalValues)) // 4
            err(s"$name (concrete): all explored interleavings (with timeout, ${allconc.finalValues}) not contained in reduced interleavings (${redconc.finalValues})")
          if (!oneconc.finalValues.subsetOf(redconc.finalValues)) // 5
            err(s"$name (concrete): one interleaving (${oneconc.finalValues}) not contained in reduced interleavings (${redconc.finalValues})")
        } else if (redconc.timedOut) {
          if (!allconc.timedOut)
            err("s$name (concrete): reduced interleavings timed out, but all interleavings did not")
        }
        /* Abstract */
        if (!oneabs.timedOut && !allabs.timedOut && !redabs.timedOut) {
          if (!oneabs.finalValues.subsetOf(allabs.finalValues)) // 1
            err(s"$name (abstract): one interleaving (${oneabs.finalValues}) not contained in all interleavings (${allabs.finalValues})")
          if (!(redabs.finalValues == allabs.finalValues)) // 2
            err(s"$name (abstract): reduced interleavings (${redabs.finalValues}) do not match all interleavings (${allabs.finalValues})")
          if (!oneabs.finalValues.subsetOf(redabs.finalValues)) // 3
            err(s"$name (abstract): one interleaving (${oneabs.finalValues}) not contained in reduced interleavings (${redabs.finalValues})")
        } else if (allabs.timedOut && !oneabs.timedOut && !redabs.timedOut) {
          if (!allabs.finalValues.subsetOf(redabs.finalValues)) // 4
            err(s"$name (abstract): all explored interleavings (with timeout, ${allabs.finalValues}) not contained in reduced interleavings (${redabs.finalValues})")
          if (!oneabs.finalValues.subsetOf(redabs.finalValues)) // 5
            err(s"$name (abstract): one interleaving (${oneabs.finalValues}) not contained in reduced interleavings (${redabs.finalValues})")
        } else if (redabs.timedOut) {
          if (!allabs.timedOut)
            err("s$name (abstract): reduced interleavings timed out, but all interleavings did not")
        }
        /* Concrete <-> Abstract */
        if (!oneabs.timedOut && !oneconc.timedOut && !subsumes(oneabs.finalValues, oneconc.finalValues))
          err("$name (one): abstract (${oneabs.finalValues}) does not subsume concrete (${oneconc.finalValues})")
        if (!allabs.timedOut && !allconc.timedOut && !subsumes(allabs.finalValues, allconc.finalValues))
          err("$name (all): abstract (${allabs.finalValues}) does not subsume concrete (${allconc.finalValues})")
        if (!redabs.timedOut && !redconc.timedOut && !subsumes(redabs.finalValues, redconc.finalValues))
          err("$name (red): abstract (${redabs.finalValues}) does not subsume concrete (${redconc.finalValues})")
        /* Random (8) */
        if (!redabs.timedOut && !randabs.finalValues.subsetOf(redabs.finalValues))
          err("$name (abstract): randomly explored paths explored values (${randabs.finalValues}) that weren't explored in reduced interleavings (${redabs.finalValues})")
        if (!redconc.timedOut && !randconc.finalValues.subsetOf(redconc.finalValues))
          err("$name (concrete): randomly explored paths explored values (${randconc.finalValues}) that weren't explored in reduced interleavings (${redconc.finalValues})")
      })
    }

    private type Results = Map[String, Map[ExplorationType.Value, MachineOutput]]
    private case class State(computing: Int, work: Queue[MachineConfig], concreteResults: Results, abstractResults: Results)

    private def updateResults(results: Results, in: MachineConfig, out: MachineOutput): Results = results.get(in.name) match {
      case Some(m) => results + (in.name -> (m + (in.exploration -> (in.exploration match {
        case ExplorationType.RandomInterleaving => m.get(ExplorationType.RandomInterleaving) match {
          case Some(res) => res.copy(finalValues = res.finalValues ++ out.finalValues)
          case None => out
        }
        case _ => out
      }))))
      case None => results + (in.name -> Map(in.exploration -> out))
    }

    private def sendWork(actor: ActorRef, state: State): Receive = state.work.dequeueOption match {
      case Some((item, remainingWork)) =>
        actor ! Computation(item)
        val newState = state.copy(computing = state.computing + 1, work = remainingWork)
        active(newState)
      case None => {
        if (state.computing == 0) {
          scala.Console.withOut(logging) {
            /* no more work to do, nothing is computing, stop */
            checkResults(state)
            printResults(state.concreteResults)
            printResults(state.abstractResults)
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
        scala.Console.withOut(logging) { println(s"$in: $out") }
        context.become(sendWork(sender, state.copy(computing = state.computing-1,
          concreteResults = if (in.concrete) updateResults(state.concreteResults, in, out) else state.concreteResults,
          abstractResults = if (in.concrete) state.abstractResults else updateResults(state.abstractResults, in, out))))
    }

    def receive = active(State(0, Queue[MachineConfig](),
      Map[String, Map[ExplorationType.Value, MachineOutput]](), Map[String, Map[ExplorationType.Value, MachineOutput]]()))
  }

  def main(args: Array[String]) {
    BenchmarksConfig.parser.parse(args, BenchmarksConfig.Configuration()) match {
      case Some(config) =>
        val work = programs.toList.flatMap(name =>
          (List(ExplorationType.AllInterleavings, ExplorationType.OneInterleaving, ExplorationType.InterferenceTracking) ++
            List.fill(config.random)(ExplorationType.RandomInterleaving)).flatMap(expl =>
            Set(true, false).map(concrete =>
              MachineConfig(name, expl, concrete, config.timeout))))
        println(s"Scheduling ${work.size} items of work")
        val workers = (1 to config.workers).map(i => system.actorOf(Props[Worker], s"worker-$i"))
        val dispatcher = system.actorOf(Props[Dispatcher], "dispatcher")
        dispatcher ! AddWork(work)
        workers.foreach(dispatcher ! SendWork(_))
      case None => ()
    }
  }
}
