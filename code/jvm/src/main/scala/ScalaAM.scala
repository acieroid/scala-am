package scalaam.cli

import incremental.GumTreeDiff
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = {
    testLex("test/fact.scm")
  }

  def testLex(file: String): Unit = {
    val txt = loadFile(file)
    val prg = SchemeParser.parse(txt)
    val analysis = new ModAnalysis(prg) with SmallStepSchemeModFSemantics
                                        with ConstantPropagationDomain
                                        with FullArgumentSensitivity
    analysis.analyze()
    debugResults(analysis)
  }

  type Machine = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def debugResults(machine: Machine): Unit = {
    machine.store.foreach {
      case (machine.ReturnAddr(cmp),result) =>
        println(s"$cmp => $result")
      case _ => {}
    }
  }

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }
}

object DiffMain {
  def main(args: Array[String]): Unit = {
    val prg1 = SchemeParser.parse("(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 2))\n         (fib (- n 1)))))\n\n(fib 10)")
    val prg2 = SchemeParser.parse("(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n\n(fib 10)")
    println(prg1)
    println(prg2)
    println()
    val map = GumTreeDiff.map(prg1, prg2)
    map.foreach(println)
  }
}