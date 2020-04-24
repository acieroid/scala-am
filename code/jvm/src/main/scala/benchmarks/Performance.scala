package scalaam.cli.benchmarks

import scalaam.core._
import scalaam.io.Reader
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

  def run(file: String, s: Sensitivity): Unit = {
    val program = SchemeParser.parse(Reader.loadFile(file))

    var times: List[Long] = List()

    // Warm-up.
    write(s"* Warmup (${warmup}) - ")
    for (i <- 1 to warmup) {
      write(s"$i ")
      // TODO: Add System.gc() here?
      newAnalysis(program, s).analyze()
    }

    System.gc()

    write(s"\n* Time (${actual}) - ")
    for (i <- 1 to actual) {
      write(s"$i ")
      val analysis = newAnalysis(program, s)
      System.gc()
      val t = Timer.timeOnly({analysis.analyze()})
      times = t :: times
    }

    val m = Metrics.all(times)
    writeln(s"\n      Mean time: ${m.mea / 1000000}ms")
    writeln(s"      Min  time: ${m.min / 1000000}ms")
    writeln(s"      Max  time: ${m.max / 1000000}ms")
    writeln(s"      Med  time: ${m.med / 1000000}ms")
    writeln(s"         Stddev: ${m.std / 1000000}ms")

  }

  val benchmarks: List[String] = List()

  def measure(): Unit = {
    benchmarks.foreach { b =>
      Sensitivity.values.foreach { s =>
        writeln(s"***** $b / $s *****")
        run(b, s)
      }
    }
  }

  measure()
  closeDefaultWriter()
}