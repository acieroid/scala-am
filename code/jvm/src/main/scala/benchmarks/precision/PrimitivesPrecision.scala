package scalaam.cli.benchmarks.precision

import scalaam.cli.benchmarks._
import scalaam.io.Reader

import scalaam.lattice._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme.primitives.SchemePrelude

import scala.concurrent.duration._
import scalaam.util._

object PrimitivesBenchmarks {
  val gabriel = List(
    "test/gabriel/browse.scm",
    "test/gabriel/diviter.scm",
    "test/gabriel/divrec.scm",
    "test/gabriel/destruc.scm",
    "test/gabriel/dderiv.scm",
    "test/gabriel/deriv.scm",
  )
  val gambit = List(
    "test/gambit/mazefun.scm",
  )

  val scp1Compressed = SchemeBenchmarks.scp1_compressed

  val ad = List(
    // "test/ad/bfirst.scm", // incorrect benchmark
    // "test/ad/bst.scm", // only definitions
    // "test/ad/btree.scm", // only definitions
    "test/ad/dict.scm",
    "test/ad/prioq.scm",
    // "test/ad/queue.scm", // only definitions
    "test/ad/quick.scm",
    // "test/ad/RBtreeADT.scm", // dot notation?
    "test/ad/stack.scm",
    // "test/stspaceCODE.scm" // only definitions
  )

  val icp = List(
    // "test/icp/icp_1c_ontleed.scm", // too slow
    // "test/icp/icp_1c_multiple-dwelling.scm", // stack overflows the concrete interpreter
    // "test/icp/icp_1c_prime-sum-pair.scm", // too slow
    "test/icp/icp_2_aeval.scm",
    "test/icp/icp_3_leval.scm",
    // "test/icp/icp_4_qeval.scm", // define-syntax, apply, eval
    "test/icp/icp_5_regsim.scm",
    // "test/icp/icp_6_stopandcopy_scheme", // vectors
    // "test/icp/icp_7_eceval.scm", // too slow
    "test/icp/icp_8_compiler.scm"
  )

  val benchmarks = gambit ++ gabriel ++ List(
    "test/kernighanvanwyk/ack.scm",
    "test/rsa.scm",
    "test/church.scm",
    "test/mceval.scm",
    "test/sat.scm",
    "test/regex.scm",
    "test/rsa.scm",
    //    "test/sigscheme/takr.scm", // has a tendency to result in OOM in concrete mode
  )

  // Benchmarks with vectors are not properly supported. They could be
  // interesting to support, but a first look at the results shows that it does
  // not deviates from other benchmarks
  val vectorBenchmarks = List(
    "test/four-in-a-row.scm",
    "test/gambit/matrix.scm",
    "test/sigscheme/mem.scm",
    "test/grid.scm",
    "test/gabriel/puzzle.scm",
    "test/ad/abstrct.scm",
    "test/ad/bubsort.scm",
    "test/ad/heap.scm",
    "test/ad/inssort.scm",
    "test/ad/linear.scm",
    "test/ad/list.scm",
    "test/ad/mesort.scm",
    "test/ad/qsort.scm",
    "test/ad/qstand.scm",
    "test/ad/selsort.scm",
  )


  val improvedPrecision = Map(
    "test/church.scm" -> List("plus", "mult", "pred", "sub", "church0", "church1", "church2", "church0?", "church=?"),
    "test/mceval.scm" -> List(
      "foldr", "foldr-aux", "foldl", "foldl-aux",
      "self-evaluating?", "variable?", "tagged-list?", "quoted?", "text-of-quotation", "assignment?",
      "assignment-variable", "assignment-value", "definition?", "definition-variable", "make-lambda",
      "definition-value", "lambda?", "lambda-parameters", "lambda-body", "if?", "if-predicate", "if-consequent",
      "if-alternative", "make-if", "begin?", "begin-actions", "last-exp?", "first-exp", "rest-exps", "mk-begin",
      "sequence->exp", "application?", "operator", "operands", "no-operands?", "rest-operands",
      "cond?", "cond-clauses", "cond-predicate", "cond-else-clause?", "cond-actions", "cond->if",
      "true?", "false?", "make-procedure", "compound-procedure?", "procedure-parameters", "procedure-body",
      "procedure-environment", "enclosing-environment", "first-frame", "make-frame", "frame-variables", "frame-values",
      "add-binding-to-frame!", "extend-environment", "primitive-procedure", "primitive-implementation",
      "expand-clauses", "lookup-variable-value", "set-variable-value!", "define-variable!",
    ),
    "test/regex.scm" -> List(
      "regex-alt?", "regex-seq?", "regex-rep?", "regex-null?", "regex-empty?", "regex-atom?",
      "match-seq", "match-alt", "match-rep", "seq", "alt", "rep", "regex-empty", "regex-derivative", "d/dc",
      "regex-match", "check-expect"
    ),
    "test/rsa.scm" -> List(
      "extended-gcd", "modulo-inverse", "totient", "square", "modulo-power",
      "is-legal-public-exponent?", "private-exponent", "encrypt", "decrypt"
    ),
    "test/scp1/5.20.4.scm" -> List(
      "show", "one", "één-buis?", "geen-buis?"
    ),
    "test/scp1/5.22.scm" -> List(
      "foldr", "foldr-aux", "totaal", "zoek-korting", "total-iter", "loop"
    ),
    "test/scp1/7.9.scm" -> List(
      "blad?", "appel?", "type", "leafs", "all-apples", "conditional-append", "apple-types", "bewerk-boom",
      "leafs-dmv-bewerk", "all-apples-dmv-bewerk", "apple-types-dmv-bewerk"
    ),
    "test/scp1/7.11.scm" -> List(
      "baas", "sub-organigrammen", "hierarchisch?", "hierarchisch?-in",
      "collegas", "collegas-in", "werknemers-in", "werknemers"
    ),
    "test/scp1/7.14.scm" -> List(
      "atom?", "maak-dier", "naam", "eigenschappen", "dier?", "maak-boom", "knoop", "deelbomen", "leeg?", "knoop?",
      "all-kinds", "all-kinds-in", "geef-eigenschappen", "geef-eig-in", "ask?"
    ),
    "test/scp1/8.15.scm" -> List(
      "foldr", "foldr-aux", "foldl", "foldl-aux",
      "maak-buffer", "newValue", "returnSum", "flush", "value",
      "maak-verkeersteller", "newCar", "newHour", "newDay", "loop",
    ),
    "test/scp1/9.12.scm" -> List(
      "find-last", "flatten!", "atom?", "flatten2!",
    ),
    // That will OOM when applied with a too-high sensitivity (e.g., 10ACS)
//    "test/sigscheme/takr.scm" -> List(
//      "tak0", "tak1", "tak2", "tak3", "tak4", "tak5", "tak6", "tak7", "tak8", "tak9", "tak10",
//      "tak11", "tak12", "tak13", "tak14", "tak15", "tak16", "tak17", "tak18", "tak19", "tak20",
//      "tak21", "tak22", "tak23", "tak24", "tak25", "tak26", "tak27", "tak28", "tak29", "tak30",
//      "tak31", "tak32", "tak33", "tak34", "tak35", "tak36", "tak37", "tak38", "tak39", "tak40",
//      "tak41", "tak42", "tak43", "tak44", "tak45", "tak46", "tak47", "tak48", "tak49", "tak50",
//      "tak51", "tak52", "tak53", "tak54", "tak55", "tak56", "tak57", "tak58", "tak59", "tak60",
//      "tak61", "tak62", "tak63", "tak64", "tak65", "tak66", "tak67", "tak68", "tak69", "tak70",
//      "tak71", "tak72", "tak73", "tak74", "tak75", "tak76", "tak77", "tak78", "tak79", "tak80",
//      "tak81", "tak82", "tak83", "tak84", "tak85", "tak86", "tak87", "tak88", "tak89", "tak90",
//      "tak91", "tak92", "tak93", "tak94", "tak95", "tak96", "tak97", "tak98", "tak99",
//    ),
    "test/gambit/peval.scm" -> List(
      "every?", "some?", "map2", "get-last-pair", "alphabetize", "const-expr?", "const-value", "quot",
      "not-constant?", "remove-constant", "extract-constant", "beta-subst", "binding-frame", "bound-expr",
      "add-binding", "for-each!", "arg-pattern", "sum", "product", "reduce-global", "constant-fold-global"
    ),
    "test/gambit/mazefun.scm" -> List(
      "foldr", "foldr-aux", "foldl", "foldl-aux",
      "for", "for-aux", "concat",
      "list-read", "list-write", "list-remove-pos", "duplicates?",
      "make-matrix", "matrix-read", "matrix-write", "matrix-size", "matrix-map",
      "shuffle", "shuffle-aux", "make-maze", "cave-to-maze", "pierce", "pierce-randomly",
      "try-to-pierce", "change-cavity", "change-cavity-aux", "neighboring-cavities"
    ),
    "test/gabriel/browse.scm" -> List(
      "lookup", "get", "put", "append-to-tail!", "tree-copy",
    ),
    "test/gabriel/dderiv.scm" -> List(
      "lookup", "loop", "get", "put", "dderiv", "my+dderiv", "my-dderiv", "*dderiv", "/dderiv"
    ),
    "test/gabriel/deriv.scm" -> List(
      "deriv"
    ),
    "test/gabriel/divrec.scm" -> List(
      "create-n", "loop", "recursive-div2"
    ),
    "test/gabriel/destruc.scm" -> List(
      "append-to-tail!", "destructive"
    ),
    "test/gabriel/diviter.scm" -> List(
      "create-n", "iterate-div2",
    )

  ).withDefaultValue(List())
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
  def baseAnalysis(prg: SchemeExp): Analysis =
    SchemeAnalyses.contextInsensitiveAnalysis(prg)
  def otherAnalyses(prg: SchemeExp) = List(
//    S_0_0(prg), // should be equivalent to base analysis
    S_CS_0(prg),
    S_2CS_0(prg),
    S_2AcyclicCS_0(prg),
    S_10CS_0(prg),
//    S_10AcyclicCS_0(prg), // does not yield interesting results
    S_FA_0(prg),
//    S_2FA_0(prg), // does not improve on FA
//    S_10FA_0(prg), // does not improve on FA
    S_CSFA_0(prg), // does not improve on FA, but we want to include them still
  )

  def main(args: Array[String]) = runBenchmarks() // check("test/primtest.scm")

  def check(benchmark: Benchmark) = {
      val txt = Reader.loadFile(benchmark)
      val prg = SchemeParser.parse(txt)
      val con = runInterpreter(prg, path).get
      val abs = runAnalysis(baseAnalysis(prg),path).get
      val allKeys = con.keys ++ abs.keys
      val interestingKeys = allKeys.filter(_.isInstanceOf[RetAddr])
      interestingKeys.foreach { k =>
          println(s"$k -> ${abs.getOrElse(k,"⊥")} ; ${con.getOrElse(k,"⊥")} ")
      }
  }

  def runBenchmarks() = {
    PrimitivesBenchmarks.scp1Compressed.foreach(b => {
      System.gc()
      path = b
      runBenchmark(b)
    })
    println("Results:")
    println("Benchmark & 0 & CS & 2CS & 2ACS & 10CS & 10ACS & FA & 2FA & 10FA & CSFA & Max \\\\")
    this.results.foreach { case (b, r) =>
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
