package scalaam.cli.evaluation.primitives

import scalaam.cli.benchmarks._
import scalaam.io._
import scalaam.io.Writer._
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity._
import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity.Sensitivity._
import scalaam.modular.scheme._
import scalaam.util._
import scala.concurrent.duration._

object PerformanceType extends Performance {
  def analysisTimeout() = Timeout.start(Duration(20, MINUTES))
  import scalaam.core._
  import scalaam.language.scheme.primitives._
  abstract class AnalysisWithManualPrimitives(p: SchemeExp) extends ModAnalysis(p)with BigStepSemantics with StandardSchemeModFSemantics {
    override lazy val program = {
      val originalProgram = p
      // We exclude all defined primitives from the preludation
      val preludedProgram = SchemePrelude.addPrelude(originalProgram, primitives.allPrimitives.map(_.name).toSet)
      // Set up initial environment and install the primitives in the global store.
      primitives.allPrimitives.foreach { p =>
        val addr = ComponentAddr(initialComponent, PrmAddr(p.name))
        store += (addr -> lattice.primitive(p))
      }
      val initialBindings = primitives.allPrimitives.map(_.name).toSet
      SchemeLexicalAddresser.translateProgram(preludedProgram, initialBindings)
    }
    lazy val valueLattice = new TypeSchemeLattice[Addr, Component]
    type Value = valueLattice.L
    lazy val lattice = valueLattice.schemeLattice
    override val primitives: SchemePrimitives[Value, Addr] = new SchemeLatticePrimitives[Value, Addr] {
      override def allPrimitives = super.allPrimitives ++ List(
        `equal?`,
        `length`,
        `not`,
        `cddr`, `cadr`,
        `caddr`, `cdddr`, `caadr`, `cdadr`,
        `cadddr`,
      )
      class SimplePrim(val name: String, ret: Value) extends SchemePrimitive[Value, Addr] {
        def call(fexp: SchemeExp, args: List[(SchemeExp, Value)], store: Store[Addr, Value], alloc: SchemeAllocator[Addr]): MayFail[(Value, Store[Addr, Value]), Error] =
          MayFail.success((ret, store))
      }
      object `equal?` extends SimplePrim("equal?", valueLattice.Inject.bool)
      object `length` extends SimplePrim("length", valueLattice.Inject.num)
      object `not` extends SimplePrim("not", valueLattice.Inject.bool)
      object `cadr` extends Store1Operation("cadr", { (x, store) =>
        dereferenceAddrs(lattice.cdr(x), store).flatMap(v =>
          dereferenceAddrs(lattice.car(v), store).map((_, store)))
      })
      object `cddr` extends Store1Operation("cddr", { (x, store) =>
        dereferenceAddrs(lattice.cdr(x), store).flatMap(v =>
          dereferenceAddrs(lattice.cdr(v), store).map((_, store)))
      })
      object `caddr` extends Store1Operation("caddr", { (x, store) =>
        dereferenceAddrs(lattice.cdr(x), store).flatMap(v =>
          dereferenceAddrs(lattice.cdr(v), store).flatMap(v =>
            dereferenceAddrs(lattice.car(v), store).map((_, store))))
      })
      object `caadr` extends Store1Operation("caadr", { (x, store) =>
        dereferenceAddrs(lattice.cdr(x), store).flatMap(v =>
          dereferenceAddrs(lattice.car(v), store).flatMap(v =>
            dereferenceAddrs(lattice.car(v), store).map((_, store))))
      })
      object `cdadr` extends Store1Operation("cdadr", { (x, store) =>
        dereferenceAddrs(lattice.cdr(x), store).flatMap(v =>
          dereferenceAddrs(lattice.car(v), store).flatMap(v =>
            dereferenceAddrs(lattice.cdr(v), store).map((_, store))))
      })
      object `cdddr` extends Store1Operation("cdddr", { (x, store) =>
        dereferenceAddrs(lattice.cdr(x), store).flatMap(v =>
          dereferenceAddrs(lattice.cdr(v), store).flatMap(v =>
            dereferenceAddrs(lattice.cdr(v), store).map((_, store))))
      })
      object `cadddr` extends Store1Operation("cadddr", { (x, store) =>
        dereferenceAddrs(lattice.cdr(x), store).flatMap(v =>
          dereferenceAddrs(lattice.cdr(v), store).flatMap(v =>
            dereferenceAddrs(lattice.cdr(v), store).flatMap(v =>
              dereferenceAddrs(lattice.car(v), store).map((_, store)))))
      })
    }
  }

  abstract class AnalysisWithPreludedPrimitives(p: SchemeExp) extends ModAnalysis(p)with BigStepSemantics with StandardSchemeModFSemantics {
    override lazy val program = {
      val originalProgram = p
      // We exclude all defined primitives from the preludation
      val preludedProgram = SchemePrelude.addPrelude(originalProgram, primitives.allPrimitives.map(_.name).toSet)
      // Set up initial environment and install the primitives in the global store.
      primitives.allPrimitives.foreach { p =>
        val addr = ComponentAddr(initialComponent, PrmAddr(p.name))
        store += (addr -> lattice.primitive(p))
      }
      val initialBindings = primitives.allPrimitives.map(_.name).toSet
      SchemeLexicalAddresser.translateProgram(preludedProgram, initialBindings)
    }
    lazy val valueLattice = new TypeSchemeLattice[Addr, Component]
    type Value = valueLattice.L
    lazy val lattice = valueLattice.schemeLattice
  }

  def analyses = List(
    (p => new AnalysisWithPreludedPrimitives(p) with S_CSFA_0, "P"),
    (p => new AnalysisWithManualPrimitives(p) with S_CSFA_0, "M")
  )
}
