package scalaam.cli

import scalaam.util._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.language.sexp._

object Main {

  def main(args: Array[String]): Unit = {
    val lexical = new SExpLexer
    val text = "3.14"
    val input = new scala.util.parsing.input.CharArrayReader(text.toCharArray)
    val output = lexical.number(input)
    println(output)
  }

  def testLex(file: String): Unit = {
    val txt = loadFile(file)
    val prg = SchemeParser.parse(txt)
    val analysis = new ModAnalysis(prg) with SmallStepSchemeModFSemantics
                                        with ConstantPropagationDomain
                                        with NoSensitivity
    analysis.analyze()
    println(analysis.store.keys.size)
    //debugResults(analysis)
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
