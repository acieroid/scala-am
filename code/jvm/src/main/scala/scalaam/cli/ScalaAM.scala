package scalaam.cli

import java.io.File

import scalaam.core.Address
import scalaam.language.CScheme._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives.{SchemeLatticePrimitives, SchemePrelude, SchemePrimitives}
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.modular.scheme.modconc._
import scalaam.modular.scheme.ssmodconc._
import scalaam.util.{Reader, SmartUnion}
import scalaam.util.benchmarks.{Timeout, Timer}
import scalaam.language.change.CodeVersion._
import scalaam.modular.incremental.scheme.AnalysisBuilder.{IncrementalModConcAnalysis, IncrementalSchemeModFAnalysis}

import scala.concurrent.duration._
import scala.util.Random

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/concurrentScheme/threads/qsort.scm")
    val prg = CSchemeParser.parse(txt)
    val analysis = new SimpleSchemeModConcAnalysis(prg)
                                        with SchemeModConcNoSensitivity
                                        with SchemeConstantPropagationDomain
                                        with LIFOWorklistAlgorithm[SchemeExp] { mc =>
      var i = 0
      override def step(t: Timeout.T = Timeout.none): Unit = {
        i = i + 1
        val cmp = workList.head
        println(s"[$i] $cmp")
        super.step(t)
      }
      def modFAnalysis(intra: SchemeModConcIntra) = new InnerModFAnalysis(intra)
                                                        with SchemeModFNoSensitivity
                                                        with RandomWorklistAlgorithm[SchemeExp] {
        lazy val primitives: SchemePrimitives[Value,Address] = mc.primitives
        var j = 0
        override def step(t: Timeout.T): Unit = {
          j = j + 1
          val cmp = workList.head
          println(s"[$i.$j] $cmp")
          super.step(t)
        } 
      }
    }
    analysis.analyze(Timeout.start(Duration(30,SECONDS)))
    //debugClosures(analysis)
    debugResults(analysis, true)
  }

  type SchemeAnalysis = ModAnalysis[SchemeExp] with GlobalStore[SchemeExp]
  def debugResults(machine: SchemeAnalysis, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (ReturnAddr(cmp, idn), result) if cmp == machine.initialComponent || printMore =>
        println(s"$cmp => $result")
      case _ => ()
    }
}

object Run extends App {
  val text = Reader.loadFile("test/concurrentScheme/threads/pc.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), true)
  val res = interpreter.run(CSchemeUndefiner.undefine(List(SchemePrelude.addPrelude(CSchemeParser.parse(text), Set("newline", "display")))), Timeout.none, New)
  println(res)
}

object Analyze extends App {
  def one(bench: String, timeout: () => Timeout.T): Unit = {
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new ModAnalysis(text)
      with kCFAModConc
      with SchemeConstantPropagationDomain
      with LIFOWorklistAlgorithm[SchemeExp] {
      val k = 1

      override def intraAnalysis(component: SmallStepModConcComponent): IntraAnalysis = new IntraAnalysis(component) with kCFAIntra
    }
    a.analyze(timeout())
    val r = a.finalResult
    println(r)
  }

  val bench: List[String] = List(
  "test/changes/cscheme/threads/mcarlo.scm"
  )

  bench.foreach({b =>
    try {
      print(b + " => ")
      val t0 = System.currentTimeMillis()
      one(b, () => Timeout.start(Duration(2, MINUTES)))
      val t1 = System.currentTimeMillis()
      println(s"    in ${(t1 - t0)}ms")
    } catch {
      case t: Throwable => println(s"Raised exception.")
        System.err.println(t.getMessage)
        t.printStackTrace()//t.getStackTrace.take(10).foreach(System.err.println)
        System.err.flush()
    }
  })

}

object IncrementalRun extends App {

  def modconcAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new IncrementalModConcAnalysis(text)
    a.analyze(timeout())
    val store1 = a.store
    a.updateAnalysis(timeout())
    val store2 = a.store
    store2.keySet.foreach { k =>
      val v1 = store1.getOrElse(k, a.lattice.bottom)
      if (store2(k) != v1)
        println(s"$k: $v1 -> ${store2(k)}")

    }
  }

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new IncrementalSchemeModFAnalysis(text)
    a.analyze(timeout())
    val store1 = a.store
    a.updateAnalysis(timeout())
    val store2 = a.store
    store2.keySet.foreach { k =>
      val v1 = store1.getOrElse(k, a.lattice.bottom)
      if (store2(k) != v1)
        println(s"$k: $v1 -> ${store2(k)}")

    }
  }

  val modConcbenchmarks: List[String] = List(
    "test/changes/cscheme/threads/mcarlo2.scm"
    //"test/changes/cscheme/threads/sudoku.scm",
    //"test/changes/cscheme/threads/pc.scm",
    //"test/changes/cscheme/threads/stm.scm"
   )
  val    modFbenchmarks: List[String] = List("test/changes/scheme/ring-rotate.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modConcbenchmarks.foreach { bench =>
    modconcAnalysis(bench, standardTimeout)
  }
  modFbenchmarks.foreach { bench =>
    modfAnalysis(bench, standardTimeout)
  }
}

object SimpleTimingTest extends App {

  type Analysis = ModAnalysis[SchemeExp] with GlobalStore[SchemeExp]

  def analysis(program: SchemeExp): Analysis = new ModAnalysis(program)
    with kCFAModConc
    with SchemeConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {
    val k = 1
    override def intraAnalysis(component: SmallStepModConcComponent): IntraAnalysis = new IntraAnalysis(component) with SmallStepIntra with kCFAIntra
  }

  def run(benchmark: String): Unit = {
    System.out.print(benchmark + " ")
    System.out.flush()
    val text = CSchemeParser.parse(Reader.loadFile(benchmark))
    val a = analysis(text)
    val to = Timeout.start(Duration(1, MINUTES))
    val time = Timer.timeOnly(a.analyze(to))
    if (to.reached) {
      System.out.println("timed out.")
    } else {
      System.out.println(s"finished in ${time / 1000000}ms.")
    }
  }

  // Kind of warm-up.
  System.err.println("Warm-up")
  System.err.flush()
  SchemeBenchmarks.other.foreach(run)

  // Actual tests.
 // System.err.println("Run")
 // System.err.flush()
 // SchemeBenchmarks.threads.foreach(run)

  // Just copy-paste for this
  object SchemeBenchmarks {

    def files(dir: File): Array[File] = {
      val lst = dir.listFiles()
      if (lst == null) Array()
      else lst
    }

    def fromFolder(directory: String, exclude: String*): Set[String] = {
      val root = new File(directory)
      val base = root.getAbsolutePath.length - directory.length
      files(root).filter(!_.isDirectory).map(_.getAbsolutePath.substring(base)).toSet -- exclude.map(file => s"$directory/$file")
    }

    lazy val other: Set[String] = Set(
      "test/R5RS/gambit/peval.scm",
      "test/R5RS/gambit/earley.scm", // list->vector
      "test/R5RS/gambit/scheme.scm",
      "test/R5RS/gambit/sboyer.scm",
      "test/R5RS/gambit/nboyer.scm",
    )

    lazy val threads: Set[String] = fromFolder("test/concurrentScheme/threads",
      "abp.scm", // Unbound reference: display-recorded.
      "lastzero2.scm", // Uses let*, but should use something like letrec*?
      "phild.scm", // Unbound reference: bool-top
    )
  }
}