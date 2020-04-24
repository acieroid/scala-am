package scalaam.cli.benchmarks

import scalaam.io._
import scalaam.io.Writer._
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity._
import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity.Sensitivity._
import scalaam.modular.scheme._
import scalaam.util._

object Performance extends App {

  val warmup = 3
  val actual = 15

  var results: Map[String, Map[String, Int]] = Map().withDefaultValue(Map())

  setDefaultWriter(open("benchOutput/results.txt"))

  abstract class Analysis(p: SchemeExp) extends ModAnalysis(p)with SmallStepSemantics with ConstantPropagationDomain with StandardSchemeModFSemantics

  def newAnalysis(p: SchemeExp, s: Sensitivity): Analysis = s match {
    case S_0_0           => new Analysis(p) with S_0_0
    case S_CS_0          => new Analysis(p) with S_CS_0
    case S_2CS_0         => new Analysis(p) with S_2CS_0
    case S_10CS_0        => new Analysis(p) with S_10CS_0
    case S_FA_0          => new Analysis(p) with S_FA_0
    case S_2FA_0         => new Analysis(p) with S_2FA_0
    case S_10FA_0        => new Analysis(p) with S_10FA_0
    case S_CSFA_0        => new Analysis(p) with S_CSFA_0
    case S_2AcyclicCS_0  => new Analysis(p) with S_2AcyclicCS_0
    case S_10AcyclicCS_0 => new Analysis(p) with S_10AcyclicCS_0
  }

  def run(file: String, s: Sensitivity): Double = {
    val program = SchemeParser.parse(Reader.loadFile(file))

    var times: List[Long] = List()

    // Warm-up.
    print(s"* Warmup (${warmup}) - ")
    for (i <- 1 to warmup) {
      print(s"$i ")
      // TODO: Add System.gc() here?
      newAnalysis(program, s).analyze()
    }

    System.gc()

    print(s"\n* Time (${actual}) - ")
    for (i <- 1 to actual) {
      print(s"$i ")
      val analysis = newAnalysis(program, s)
      System.gc()
      val t = Timer.timeOnly({analysis.analyze()})
      times = t :: times
    }

    val m = Metrics.all(times)
    println(s"\n      Mean time: ${m.mea / 1000000}ms")
    println(s"      Min  time: ${m.min / 1000000}ms")
    println(s"      Max  time: ${m.max / 1000000}ms")
    println(s"      Med  time: ${m.med / 1000000}ms")
    println(s"         Stddev: ${m.std / 1000000}ms")

    m.mea
  }

  val benchmarks: List[String] = SchemeBenchmarks.standard.toList.take(1)

  def measure(): Unit = {
    benchmarks.foreach { b =>
      Sensitivity.values.foreach { s =>
        try {
          println(s"***** $b / $s *****")
          val time: Double = run(b, s)
          results = results + (b -> ((results(b) + (s.toString -> (time / 1000000).toInt))))
        } catch {
          case e: Exception => writeln(s"Running $b resulted in an exception: ${e.getMessage}")
          case e: VirtualMachineError => writeln(s"Running $b resulted in an error: ${e.getMessage}")
        }
      }
    }
  }

  measure()
  val table = TableWriter.writeTable(results, "Benchmark", benchmarks, Sensitivity.values.toList.map(_.toString), "T")
  write(table)
  closeDefaultWriter()
}
