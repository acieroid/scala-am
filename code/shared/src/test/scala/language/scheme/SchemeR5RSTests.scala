package scalaam.test.language.scheme

import org.scalatest.propspec.AnyPropSpec

import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme._
import scalaam.util.Timeout
import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._


import scala.concurrent.duration._

trait SchemeR5RSTests extends AnyPropSpec {

  import SchemeR5RSBenchmarks._

  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def analysis(text: SchemeExp): Analysis

  def testExpr(program: String, answer: L => V): Unit = {
    val text = SchemeParser.parse(program)
    val a = analysis(text)
    val l = a.lattice.asInstanceOf[SchemeR5RSBenchmarks.L]

    a.analyze(Timeout.start(Duration(30 , SECONDS)))
    // All R5RS tests should terminate, no matter the analysis, because they're so simple.
    assert(a.finished(), s"Analysis of $program should finish within the given time bound out.")
    val result = a.store.getOrElse(a.ReturnAddr(a.initialComponent), a.lattice.bottom).asInstanceOf[V]
    assert(l.subsumes(result, answer(l)), s"Primitive computation test failed on program: $program with result $result.")
  }

  SchemeR5RSBenchmarks.bench.foreach { case (e, a) => property (s"Primitive in $e is correct.") { testExpr(e, a) } }
}


class SchemeInterpreterR5RSCorrectnessTests extends SchemeR5RSTests {
  import SchemeR5RSBenchmarks._
  def analysis(text: SchemeExp) =
    // Not really clean, we only want a proper ConstantPropagationLattice definition
    new ModAnalysis(text) with BigStepSemantics
    with ConstantPropagationDomain
    with NoSensitivity
    with StandardSchemeModFSemantics

  override def testExpr(program: String, answer: L => V): Unit = {
    val text = SchemeParser.parse(program)
    val a = analysis(text)
    val l = a.lattice.asInstanceOf[SchemeR5RSBenchmarks.L]

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
    assert(l.subsumes(result, answer(l)), s"Primitive computation test failed on program: $program with result $result.")
  }

}

class ConcreteBigStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new ModAnalysis(text) with BigStepSemantics
    with ConstantPropagationDomain
    with NoSensitivity
    with StandardSchemeModFSemantics
}

class ConcreteSmallStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new ModAnalysis(text) with SmallStepSemantics
    with ConstantPropagationDomain
    with NoSensitivity
    with StandardSchemeModFSemantics
}

class TypeBigStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new ModAnalysis(text) with BigStepSemantics
    with TypeDomain
    with NoSensitivity
    with StandardSchemeModFSemantics
}

class TypeSmallStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new ModAnalysis(text) with SmallStepSemantics
    with TypeDomain
    with NoSensitivity
    with StandardSchemeModFSemantics
}
