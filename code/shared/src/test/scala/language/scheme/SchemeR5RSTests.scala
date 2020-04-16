package scalaam.test.language.scheme

import org.scalatest.propspec.AnyPropSpec
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme._
import scalaam.util.Timeout

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
    assume(a.finished(), s"Analysis of $program timed out.")
    val result = a.store.getOrElse(a.ReturnAddr(a.initialComponent), a.lattice.bottom).asInstanceOf[V]
    assert(l.subsumes(result, answer(l)), s"Primitive computation test failed on program: $program with result $result.")
  }

  SchemeR5RSBenchmarks.bench.foreach { case (e, a) => property (s"Primitive in $e is correct.") { testExpr(e, a) } }
}

class ConcreteBigStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new ModAnalysis(text) with BigStepSemantics
    with PowersetDomain
    with NoSensitivity
    with StandardSchemeModFSemantics
}

class ConcreteSmallStepModFSoundnessTests extends SchemeR5RSTests {
  def analysis(text: SchemeExp) = new ModAnalysis(text) with SmallStepSemantics
    with PowersetDomain
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