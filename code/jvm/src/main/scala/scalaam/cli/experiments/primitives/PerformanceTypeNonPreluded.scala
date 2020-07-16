package scalaam.cli.experiments.primitives

import scalaam.bench.scheme.SchemeBenchmarkPrograms
import scalaam.cli.experiments._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.scheme.modf.SchemeModFCompoundSensitivities.SeparateLowHighSensitivity._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.util.benchmarks.Timeout

import scala.concurrent.duration._

object PerformanceType extends PerformanceBenchmarks {
  override def analysisTime = Timeout.start(Duration(20, MINUTES))
  import scalaam.language.scheme.primitives._
  abstract class AnalysisWithManualPrimitives(p: SchemeExp) extends SimpleSchemeModFAnalysis(p) with LIFOWorklistAlgorithm[SchemeExp] {
    override lazy val program = {
      val originalProgram = p
      // We exclude all defined primitives from the preludation
      val preludedProgram = SchemePrelude.addPrelude(originalProgram, primitives.allPrimitives.map(_.name).toSet)
      // Set up initial environment and install the primitives in the global store.
      ??? //TODO: code below is broken
      /*
      primitives.allPrimitives.foreach { p =>
        val addr = ComponentAddr(initialComponent, PrmAddr(p.name))
        store += (addr -> lattice.primitive(p))
      }
      val initialBindings = primitives.allPrimitives.map(_.name).toSet
      SchemeLexicalAddresser.translateProgram(preludedProgram, initialBindings)
      */
    }
    lazy val valueLattice = new TypeSchemeLattice[Addr]
    type Value = valueLattice.L
    lazy val lattice = valueLattice.schemeLattice
    override lazy val primitives: SchemePrimitives[Value, Addr] = valueLattice.Primitives
  }

  abstract class AnalysisWithPreludedPrimitives(p: SchemeExp) extends SimpleSchemeModFAnalysis(p) with LIFOWorklistAlgorithm[SchemeExp] {
    override lazy val program = {
      val originalProgram = p
      // We exclude all defined primitives from the preludation
      val preludedProgram = SchemePrelude.addPrelude(originalProgram, primitives.allPrimitives.map(_.name).toSet)
      // Set up initial environment and install the primitives in the global store.
      ??? // old code below, compiles but does not run properly
      /*
      primitives.allPrimitives.foreach { p =>
        val addr = ComponentAddr(initialComponent, PrmAddr(p.name))
        store += (addr -> lattice.primitive(p))
      }
      val initialBindings = primitives.allPrimitives.map(_.name).toSet
      SchemeLexicalAddresser.translateProgram(preludedProgram, initialBindings)
      */
    }
    lazy val valueLattice = new TypeSchemeLattice[Addr]
    type Value = valueLattice.L
    lazy val lattice = valueLattice.schemeLattice
  }
  def benchmarks = SchemeBenchmarkPrograms.other
  def analyses = List(
    (p => new AnalysisWithPreludedPrimitives(p) with S_CSFA_0, "P"),
    (p => new AnalysisWithManualPrimitives(p) with S_CSFA_0, "M")
  )
  def main(args: Array[String]) = run()
}
