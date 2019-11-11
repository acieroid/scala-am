package scalaam.cli

import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = {
    testLex("test/sat.scm")
  }

  def testLex(file: String) = {
    val txt = loadFile(file)
    val prg = SchemeParser.parse(txt)
    val analysis = new ModAnalysis(prg) with BigStepSchemeModFSemantics
                                        with ConstantPropagationDomain
                                        with FullArgumentSensitivity
    analysis.analyze()
    debugResults(analysis)
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
