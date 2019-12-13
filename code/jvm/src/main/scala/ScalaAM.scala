package scalaam.cli

import incremental.GumTreeDiff
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.util.Timeout

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = """
      (define lst '(1 2 3))
      (define p `(,@lst 4 5))
      (length p)
    """
    val prg = SchemeParser.parse(txt)
    val analysis = new ModAnalysis(prg) with SmallStepSchemeModFSemantics
                                        with ConstantPropagationDomain
                                        with NoSensitivity
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

object DiffMain extends App {
  import scala.concurrent.duration.Duration

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }

  def analyze(text: SchemeExp) = {
    val analyzer = new ModAnalysis(text) with SmallStepSchemeModFSemantics
      with ConstantPropagationDomain
      with NoSensitivity
    analyzer.analyze()//Timeout.start(Duration(2, "MINUTES")))
    println(s"Number of components: ${analyzer.allComponents.size}")
  }

  val prg1 = SchemeParser.parse(loadFile("./test/grid.scm"))
  /*
  val prg2 = SchemeParser.parse(loadFile("./test/grid-scrambled.scm"))
  println(prg1)
  //prg1.subexpressions.foreach(v => println(v.height + " " + v))
  println(prg2)
  println()
  //val map = GumTreeDiff.computeMapping(prg1, prg2)
  //map.foreach(println)
  //println(map.keySet.size)
  println(prg1.hash)
  println(prg2.hash)
  println(prg1.eql(prg2))
  */
  analyze(prg1)
}
