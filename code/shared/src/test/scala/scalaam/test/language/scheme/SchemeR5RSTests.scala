package scalaam.test.language.scheme

import org.scalatest.propspec.AnyPropSpec
import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.scheme.lattices.SchemeLattice
import scalaam.language.scheme.primitives._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.modular._
import scalaam.test.{PrimitiveTest, SchemeR5RSBenchmarks}
import scalaam.util.benchmarks.Timeout

import scala.concurrent.duration._

trait SchemeR5RSTests extends AnyPropSpec {

  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics
  type V
  type L = SchemeLattice[V, _, _ <: Address, _ <: Primitive]

  def analysis(text: SchemeExp): Analysis

  def testExpr(program: String, answer: Any): Unit = {
    val text = SchemeParser.parse(program)
    val a = analysis(text)
    val l = a.lattice.asInstanceOf[L]

    import l.Injector._

    a.analyze(Timeout.start(Duration(30 , SECONDS)))
    // All R5RS tests should terminate, no matter the analysis, because they're so simple.
    assert(a.finished(), s"Analysis of $program should finish within the given time bound out.")
    val result = a.finalResult.asInstanceOf[V]
    assert(l.subsumes(result, answer), s"Primitive computation test failed on program: $program with result $result.")
  }

  SchemeR5RSBenchmarks.bench.foreach { case (e, a) => property (s"Primitive in $e is correct.", PrimitiveTest) { testExpr(e, a) } }
}


class SchemeInterpreterR5RSCorrectnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) =
    // Not really clean, we only want a proper ConstantPropagationLattice definition
    new SimpleSchemeModFAnalysis(text)
                          with SchemeConstantPropagationDomain
                          with SchemeModFNoSensitivity
                          with LIFOWorklistAlgorithm[SchemeExp]

  override def testExpr(program: String, answer: Any): Unit = {
    val text = SchemeParser.parse(program)
    val a = analysis(text)
    val l = a.lattice.asInstanceOf[L]

    import l.Injector._

    val interpreter = new SchemeInterpreter((_: Identity, _: SchemeInterpreter.Value) => (), false)
    val v = interpreter.run(SchemeUndefiner.undefine(List(SchemePrelude.addPrelude(text))), Timeout.start(Duration(30, SECONDS)))
    val result = v match {
      case SchemeInterpreter.Value.Nil => l.nil
      case SchemeInterpreter.Value.Str(s) => l.string(s)
      case SchemeInterpreter.Value.Symbol(s) => l.symbol(s)
      case SchemeInterpreter.Value.Integer(n) => l.number(n)
      case SchemeInterpreter.Value.Real(r) => l.real(r)
      case SchemeInterpreter.Value.Bool(b) => l.bool(b)
      case SchemeInterpreter.Value.Character(c) => l.char(c)
      case _ => ???
    }
    assert(l.subsumes(result, answer), s"Primitive computation test failed on program: $program with result $result.")
  }

}

class ConcreteBigStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new SimpleSchemeModFAnalysis(text)
                                                        with SchemeConstantPropagationDomain
                                                        with SchemeModFNoSensitivity
                                                        with LIFOWorklistAlgorithm[SchemeExp]
}

class TypeBigStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new SimpleSchemeModFAnalysis(text)
                                                        with SchemeTypeDomain
                                                        with SchemeModFNoSensitivity
                                                        with LIFOWorklistAlgorithm[SchemeExp]
}
