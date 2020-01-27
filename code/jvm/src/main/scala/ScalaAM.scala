package scalaam.cli

import scalaam.core.Identity
import scalaam.core.Identity.Position
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = loadFile("test/mceval.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new AdaptiveModAnalysis(prg)
                                          with AdaptiveSchemeModFSemantics
                                          with BigStepSemantics
                                          with AdaptiveConstantPropagationDomain
                                          with SimpleAdaptiveArgumentSensitivity {
      val limit = 5
      override def alphaValue(v: Value) = super.alphaValue(v)
    }
    analysis.analyze()
    debugResults(analysis)
  }

  type SchemeModFAnalysis = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def debugResults(machine: SchemeModFAnalysis): Unit = {
    machine.store.foreach {
      case (machine.ReturnAddr(cmp),result) =>
        println(s"$cmp => $result")
      case _ =>
    }
  }
}

object Incrementor extends App {

  type Analysis = IncrementalModAnalysis[SchemeExp] with SmallStepSemantics with ConstantPropagationDomain with NoSensitivity with IncrementalSchemeModFSemantics
  var analyzer: Analysis = _

  def analyze(file: String): Unit = analyze(SchemeParser.parse(FileUtil.loadFile(file)))

  private def analyze(text: SchemeExp): Unit  = {
    analyzer = new IncrementalModAnalysis(text) with SmallStepSemantics
                                                    with ConstantPropagationDomain
                                                    with NoSensitivity
                                                    with IncrementalSchemeModFSemantics
    analyzer.analyze()//Timeout.start(Duration(2, "MINUTES")))
    println(s"Number of components: ${analyzer.allComponents.size}")
  }

  def reanalyse(text: SchemeExp): Unit = analyzer.updateAnalysis(text)

  val a = ModuleDifferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort.scm")))
  val b = ModuleDifferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort.scm")))
  println(ModuleDifferencer.mapModules(a, b).map{ case (a, b) => (a.name, b.name) })

}

object FileUtil {

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }
}

object DiffMain extends App {

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }

  def analyze(text: SchemeExp): Unit  = {
    val analyzer = new ModAnalysis(text) with SmallStepSemantics
                                          with StandardSchemeModFSemantics
                                          with ConstantPropagationDomain
                                          with NoSensitivity
    analyzer.analyze()//Timeout.start(Duration(2, "MINUTES")))
    println(s"Number of components: ${analyzer.allComponents.size}")

    val absID: Map[Position, analyzer.Value] = analyzer.store.groupBy({_._1 match {
      case analyzer.ComponentAddr(_, addr) => addr.idn().pos
      case _                        => Identity.none.pos
    }}).view.mapValues(_.values.foldLeft(analyzer.lattice.bottom)((x,y) => analyzer.lattice.join(x,y))).toMap
    println(absID.keys.size)
  }

  val prg1 = SchemeParser.parse(loadFile("./test/church.scm"))
  println(prg1.idn.pos)
  /**
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
