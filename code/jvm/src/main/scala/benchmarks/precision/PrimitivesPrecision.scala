package scalaam.cli.benchmarks.precision

import scalaam.cli.benchmarks._
import scalaam.lattice._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme.primitives.SchemePrelude
import scala.concurrent.duration._
import scalaam.util._

object PrimitivesBenchmarks {

  // Subset of SchemeBenchmarks.standard, only the interesting benchmarks
  val standard = List(
    "test/kernighanvanwyk/ack.scm",
    "test/rsa.scm",
    "test/church.scm",
    "test/mceval.scm",
    "test/sat.scm",
    "test/regex.scm",
    "test/rsa.scm",
    //"test/four-in-a-row.scm", // unsound
    "test/sigscheme/mem.scm",
    "test/grid.scm",
    // "test/sigscheme/takr.scm", // has a tendency to result in OOM in concrete mode
  )

  val benchmarks = {
    SchemeBenchmarks.gambit ++
    SchemeBenchmarks.gabriel ++
    SchemeBenchmarks.scp1_compressed ++
    SchemeBenchmarks.icp ++
    // SchemeBenchmarks.ad ++
    List("test/ad/all.scm") ++
    standard ++
    List()
  }
  import scalaam.language.scheme._
  import scalaam.core._

  // Counts the number of distinct primitive usages.
  // This is not exactly the number of primitive calls, because of situations like the following:
  // (define foo +) (foo 1 2) (foo 2 3) // two primitive calls, one distinct usage
  // Finding the exact number of primitive calls would be impossible, but this seems like a good approximation.
  // Another solution is to simply count explicit primitive calls
  def primitiveUsages(exp: SchemeExp): Int = {
    var work: List[Expression] = List(exp)
    var calls: Set[(String, Identity)] = Set()
    while (work.nonEmpty) {
      work.head match {
        case SchemeVar(Identifier(name, pos)) if SchemePrelude.primNames.contains(name) =>
          work = work.tail
          calls = calls + ((name, pos))
        case e => work = e.subexpressions ::: work.tail
      }
    }
    calls.size
  }

  // Count the number of primitives used for a benchmark
  def primitives(exp: SchemeExp): Set[String] = {
    var work: List[Expression] = List(exp)
    var prims: Set[String] = Set()
    while (work.nonEmpty) {
      work.head match {
        case Identifier(name, _) if SchemePrelude.primNames.contains(name) =>
          work = work.tail
          prims = prims + name
        case e => work = e.subexpressions ::: work.tail
      }
    }
    prims
  }

  def main(args: Array[String]) =
    benchmarks.foreach(b => {
      val parsed = SchemeParser.parse(Reader.loadFile(b))
      val usages = PrimitivesBenchmarks.primitiveUsages(parsed)
      val prims = PrimitivesBenchmarks.primitives(parsed)
      println(s"Benchmark $b: $prims (count: ${prims.size}), and $usages usages")
    })
}

abstract class PrimitivesComparison extends AnalysisComparison[
    ConstantPropagation.I,
    ConstantPropagation.R,
    Concrete.B,
    ConstantPropagation.C,
    ConstantPropagation.S,
    Concrete.Sym
] {
  var path: String = "<none>"

  def S_0_0(prg: SchemeExp): Analysis
  def S_CS_0(prg: SchemeExp): Analysis
  def S_2CS_0(prg: SchemeExp): Analysis
  def S_2AcyclicCS_0(prg: SchemeExp): Analysis
  def S_10CS_0(prg: SchemeExp): Analysis
  def S_10AcyclicCS_0(prg: SchemeExp): Analysis
  def S_FA_0(prg: SchemeExp): Analysis
  def S_2FA_0(prg: SchemeExp): Analysis
  def S_10FA_0(prg: SchemeExp): Analysis
  def S_CSFA_0(prg: SchemeExp): Analysis


  override def analysisTimeout() = Timeout.start(Duration(2, MINUTES)) //timeout for (non-base) analyses
  override def concreteTimeout() = Timeout.start(Duration(2, MINUTES))
  override def concreteRuns() = 2
  def baseAnalysis(prg: SchemeExp): Analysis =
    SchemeAnalyses.contextInsensitiveAnalysis(prg)
  def otherAnalyses() = List(
    (S_0_0, "0_0"), // should be equivalent to base analysis
    (S_CS_0, "CS_0"),
    (S_2CS_0, "2CS_0"),
    (S_2AcyclicCS_0, "2AcyclicCS_0"),
    (S_10CS_0, "10CS_0"),
    (S_10AcyclicCS_0, "10AcyclicCS_0"),
    (S_FA_0, "FA_0"),
    (S_2FA_0, "2FA_0"), // does not improve on FA
    (S_10FA_0, "10FA_0"),// does not improve on FA
    (S_CSFA_0, "CSFA_0"), // does not improve on FA, but we want to include them still
    )

  def main(args: Array[String]) = runBenchmarks() // check("test/primtest.scm")

  def check(benchmark: Benchmark) = {
      val txt = Reader.loadFile(benchmark)
      val prg = SchemeParser.parse(txt)
      val con = runInterpreter(prg, path).get
      val abs = runAnalysis(baseAnalysis,"base analysis",prg,path).get
      val allKeys = con.keys ++ abs.keys
      val interestingKeys = allKeys.filter(_.isInstanceOf[RetAddr])
      interestingKeys.foreach { k =>
          println(s"$k -> ${abs.getOrElse(k,"⊥")} ; ${con.getOrElse(k,"⊥")} ")
      }
  }

  def runBenchmarks() = {
    PrimitivesBenchmarks.benchmarks.foreach(b => {
      System.gc()
      path = b
      runBenchmark(b)
    })
    println("Results:")
    println(results.prettyString(format = _.map(_.toString()).getOrElse("TIMEOUT")))
    /**
    println("Benchmark & 0 & CS & 2CS & 2ACS & 10CS & 10ACS & FA & 2FA & 10FA & CSFA & Max \\\\")
    this.results.toSeq.sortBy(_._1).foreach { case (b, r) =>
      val refined_0_0 = r.getOrElse("0_0", Some("T")).getOrElse("T")
      val refined_CS_0 = r.getOrElse("CS_0", Some("T")).getOrElse("T")
      val refined_2CS_0 = r.getOrElse("2CS_0", Some("T")).getOrElse("T")
      val refined_2AcyclicCS_0 = r.getOrElse("2AcyclicCS_0", Some("T")).getOrElse("T")
      val refined_10CS_0 = r.getOrElse("10CS_0", Some("T")).getOrElse("T")
      val refined_10AcyclicCS_0 = r.getOrElse("10AcyclicCS_0", Some("T")).getOrElse("T")
      val refined_FA_0 = r.getOrElse("FA_0", Some("T")).getOrElse("T")
      val refined_2FA_0 = r.getOrElse("2FA_0", Some("T")).getOrElse("T")
      val refined_10FA_0 = r.getOrElse("10FA_0", Some("T")).getOrElse("T")
      val refined_CSFA_0 = r.getOrElse("CSFA_0", Some("T")).getOrElse("T")
      val concrete = r.getOrElse("concrete", Some("T")).getOrElse("T")
      println(s"$b & $refined_0_0 & $refined_CS_0 & $refined_2CS_0 & $refined_2AcyclicCS_0 & $refined_10CS_0 & $refined_10AcyclicCS_0 & $refined_FA_0 & $refined_2FA_0 & $refined_10FA_0 & $refined_CSFA_0 & $concrete\\\\")
    }
    */
  }
}


object PrimitivesComparisonRQ3 extends PrimitivesComparison {
  def S_0_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                   with BigStepSemantics
                                                   with CompoundSensitivities.SeparateLowHighSensitivity.S_0_0
                                                   with ConstantPropagationDomain {
    override def toString() = "0_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_CS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_CS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "CS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_2CS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_2CS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "2CS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_10CS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_10CS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "10CS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_2AcyclicCS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_2AcyclicCS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "2AcyclicCS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_10AcyclicCS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_10AcyclicCS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "10AcyclicCS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_FA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_FA_0
                                                    with ConstantPropagationDomain {
    override def toString() = "FA_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_2FA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                     with BigStepSemantics
                                                     with CompoundSensitivities.SeparateLowHighSensitivity.S_2FA_0
                                                     with ConstantPropagationDomain {
    override def toString() = "2FA_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_10FA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_10FA_0
                                                    with ConstantPropagationDomain {
    override def toString() = "10FA_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_CSFA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                      with BigStepSemantics
                                                      with CompoundSensitivities.SeparateLowHighSensitivity.S_CSFA_0
                                                      with ConstantPropagationDomain {
    override def toString() = "CSFA_0"
    override val primPrecision = SchemePrelude.primNames
  }
}

abstract class PrimitivesComparisonRQ4 extends PrimitivesComparison {
  def isPrim(nam: Option[String]): Boolean
  override def S_0_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                   with BigStepSemantics
                                                   with CompoundSensitivities.SeparateLowHighSensitivity.S_0_0
                                                   with ConstantPropagationDomain {
    override def toString() = "0_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_CS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_CS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "CS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_2CS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_2CS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "2CS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_10CS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_10CS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "10CS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_2AcyclicCS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_2AcyclicCS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "2AcyclicCS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_10AcyclicCS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_10AcyclicCS_0
                                                    with ConstantPropagationDomain {
    override def toString() = "10AcyclicCS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_FA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_FA_0
                                                    with ConstantPropagationDomain {
    override def toString() = "FA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_2FA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                     with BigStepSemantics
                                                     with CompoundSensitivities.SeparateLowHighSensitivity.S_2FA_0
                                                     with ConstantPropagationDomain {
    override def toString() = "2FA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_10FA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                    with BigStepSemantics
                                                    with CompoundSensitivities.SeparateLowHighSensitivity.S_10FA_0
                                                    with ConstantPropagationDomain {
    override def toString() = "10FA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_CSFA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                      with BigStepSemantics
                                                      with CompoundSensitivities.SeparateLowHighSensitivity.S_CSFA_0
                                                      with ConstantPropagationDomain {
    override def toString() = "CSFA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
}

object PrimitivesComparisonRQ4FullPrecision extends PrimitivesComparisonRQ4 {
  def isPrim(nam: Option[String]): Boolean = true
}

object PrimitivesComparisonRQ4AnonymousFunctions extends PrimitivesComparisonRQ4 {
  def isPrim(nam: Option[String]): Boolean = nam match {
    case Some(n) => SchemePrelude.primNames.contains(n)
    case None => true /* analyze anonymous functions with high sensitivity */
  }
}

object PrimitivesComparisonRQ4NamedFunctions extends PrimitivesComparisonRQ4 {
  def isPrim(nam: Option[String]): Boolean = nam.isDefined /* analyze named functions with high sensitivity */
}
