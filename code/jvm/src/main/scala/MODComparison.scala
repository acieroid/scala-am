package scalaam.cli

import java.text.SimpleDateFormat
import java.util.concurrent.TimeoutException
import java.util.Calendar
import java.io.{BufferedWriter, FileWriter}

import au.com.bytecode.opencsv.CSVWriter
import scalaam.core._
import scalaam.language.scheme.SchemeInterpreter.Value
import scalaam.language.scheme.SchemeInterpreter.Value._
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme._

import scala.concurrent.duration._

object MODComparison extends App {

  type Machine = ModAnalysis[SchemeExp] with FullArgumentSensitivity with ConstantPropagationDomain

  val defaultTimeout: Duration = Duration(2, MINUTES)

  def readFile(file: String): SchemeExp = {
    val f   = scala.io.Source.fromFile(file)
    val exp = SchemeParser.parse(f.getLines().mkString("\n"))
    f.close()
    exp
  }

  var writer: CSVWriter =  _

  // Avoid output being buffered.
  def display(data: String): Unit = {
    print(data)
    Console.out.flush()
    writer.writeNext(data.trim)
    writer.flush()
  }

  def displayErr(data: String): Unit = {
    System.err.print(data)
    System.err.flush()
    writer.writeNext(data.trim)
    writer.flush()
  }

  val outputDir: String           = "./out/"
  val  calendar: Calendar         = Calendar.getInstance()
  val    format: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd-HH'h'mm")
  val detformat: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")

  /** Creates a fileName including the given name, suffix and a timestamp. */
  def ts(name: String, suffix: String): String =
    outputDir + format.format(calendar.getTime) + "_" + name + suffix

  def checkSubsumption[A <: Address, L](v: Set[Value], machine: Machine, lat: SchemeLattice[L, SchemeExp, A], abs: L): Boolean = v.forall {
    case Value.Undefined(_) => true
    case Value.Unbound(_)   => true
    case Clo(_, _)          => lat.getClosures(abs).nonEmpty
    case Primitive(p)       =>
      machine.schemeSemantics.primitives.get(p.name) match {
        case None       => false
        case Some(prim) => lat.subsumes(abs, lat.primitive(prim))
      }
    case Str(s)             => lat.subsumes(abs, lat.string(s))
    case Symbol(s)          => lat.subsumes(abs, lat.symbol(s))
    case Integer(i)         => lat.subsumes(abs, lat.number(i))
    case Real(r)            => lat.subsumes(abs, lat.real(r))
    case Bool(b)            => lat.subsumes(abs, lat.bool(b))
    case Character(c)       => lat.subsumes(abs, lat.char(c))
    case Nil                => lat.subsumes(abs, lat.nil)
    case Cons(_, _)         => lat.getPointerAddresses(abs).nonEmpty
    case Vector(_)          => lat.getPointerAddresses(abs).nonEmpty
    case v                  => throw new Exception(s"Unknown concrete value type: $v")
  }

  def check[A <: Address, L](name: String, v: Set[Value], p: Position, machine: Machine, lat: SchemeLattice[L, SchemeExp, A], abs: Option[L]): Unit = abs match {
    case None       => display(s"No corresponding abstract value for $v at $p.\n")
    case Some(absv) => if (!checkSubsumption(v, machine, lat, absv)) display(s"$name: subsumption check failed: $v > $absv at $p.\n")
  }

  def forMachine(name: String, machine: Machine, cMap: Map[Position, Set[Value]], timeout: Duration): Unit = try {
    displayErr(s"* $name\n  - started:  ${detformat.format(calendar.getTime)}\n")
    machine.analyze(Timeout.duration(timeout))
    displayErr(s"  - finished: ${detformat.format(calendar.getTime)}\n")

    val deps  = machine.deps
    val store = machine.store

    val pMap: Map[Position, machine.Value] = store.groupBy({_._1 match {
      case machine.GlobalAddr(addr)       => addr.pos()
      case machine.ComponentAddr(_, addr) => addr.pos()
      case _                              => Position.none
    }}).mapValues(_.values.foldLeft(machine.lattice.bottom)((a, b) => machine.lattice.join(a, b)))

    display(s"Number of components: ${machine.allComponents.size}.\n")
    display(s"Store keyset size ${store.keySet.size}.\n")
    display(s"Dependency keyset size: ${deps.keySet.size}.\n")

    for (elem <- cMap.keySet)
      check(name, cMap(elem), elem, machine, machine.lattice, pMap.get(elem))
  } catch {
    case _: TimeoutException => displayErr(s"$name timed out!\n")
    case e: Throwable => e.printStackTrace()
      writer.writeNext("*** Stacktrace omitted ***")
      writer.flush()
  }

  def forFile(file: String, timeout: Duration = defaultTimeout): Unit = try {
    displayErr(file + "\n")

    val program = readFile(file)
    val machines: List[(String, Machine)] = List(
      ("bigStep",   new ModAnalysis(program) with FullArgumentSensitivity with ConstantPropagationDomain with BigStepSchemeModFSemantics),
      ("smallStep", new ModAnalysis(program) with FullArgumentSensitivity with ConstantPropagationDomain with SmallStepSchemeModFSemantics),
    )

    // For every position, cMap keeps the concrete values encountered by the concrete interpreter.
    var cMap: Map[Position, Set[Value]] = Map().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((p, v) => cMap = cMap + (p -> (cMap(p) + v)), false)
    displayErr(s"* concrete\n  - started:  ${detformat.format(calendar.getTime)}\n")
    interpreter.run(SchemeUndefiner.undefine(List(program)), Timeout.duration(timeout))
    displayErr(s"  - finished: ${detformat.format(calendar.getTime)}\n")

    display(s"  -> Inferred ${cMap.keySet.size} positions to check values.\n")

    machines.foreach(m => forMachine(m._1, m._2, cMap, timeout))

    display("\n")

  } catch {
    case _: TimeoutException => displayErr("Concrete machine timed out!\n")
    case e: Throwable => e.printStackTrace()
      displayErr("***\n")
  }

  val output: String = ts("MODComparison",  ".txt")
  val out = new BufferedWriter(new FileWriter(output))
  writer = new CSVWriter(out, ',', CSVWriter.NO_QUOTE_CHARACTER)

  scalaam.cli.Benchmarks.WeiChenRompf2019.foreach(forFile(_))
  writer.close()
}