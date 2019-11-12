package scalaam.cli

import scalaam.core._
import scalaam.util._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = {
    testLex("test/mceval.scm")
  }

  def testLex(file: String): Unit = {
    val txt = loadFile(file)
    val prg = SchemeParser.parse(txt)
    for(_ <- 1 to 10) {
      val analysis = new ModAnalysis(prg) with BigStepSchemeModFSemantics
        with ConstantPropagationDomain
        with NoSensitivity
      System.err.println(Timer.time(analysis.analyze())._1)
    }
    //debugResults(analysis)
  }

  type Machine = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def debugResults(machine: Machine) = {
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
