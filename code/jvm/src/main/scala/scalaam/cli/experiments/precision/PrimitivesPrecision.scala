package scalaam.cli.experiments.precision

import scalaam.bench.scheme.SchemeBenchmarkPrograms
import scalaam.cli.experiments._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.lattice._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

import scala.concurrent.duration._


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
//    SchemeBenchmarks.gambit ++
//    SchemeBenchmarks.gabriel ++
//    SchemeBenchmarks.scp1_compressed ++
    SchemeBenchmarkPrograms.icp1 ++
    // SchemeBenchmarks.ad ++
//    List("test/ad/all.scm") ++
    standard ++
    List()
  }
  import scalaam.core._
  import scalaam.language.scheme._

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
    ConstantPropagation.B,
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
    val columns = List("0_0", "CS_0", "2CS_0", "2ACS_0", "10CS_0", "10ACS_0", "FA_0", "2FA_0", "10FA_0", "CSFA_0", "concrete")
    println(results.withDefaultValue(None).prettyString(columns = columns, format = _.map(_.toString()).getOrElse("TIMEOUT")))
    println(results.withDefaultValue(None).toLatexString(columns = columns, format = _.map(_.toString()).getOrElse("--")))
  }
}


object PrimitivesComparisonRQ1 extends PrimitivesComparison {
  def S_0_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                   with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_0_0
                                                   with SchemeConstantPropagationDomain
                                                   with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "0_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_CS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_CS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "CS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_2CS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_2CS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "2CS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_10CS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_10CS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "10CS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_2AcyclicCS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_2AcyclicCS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "2AcyclicCS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_10AcyclicCS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_10AcyclicCS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "10AcyclicCS_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_FA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_FA_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "FA_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_2FA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                     with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_2FA_0
                                                     with SchemeConstantPropagationDomain
                                                     with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "2FA_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_10FA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_10FA_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "10FA_0"
    override val primPrecision = SchemePrelude.primNames
  }
  def S_CSFA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                      with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_CSFA_0
                                                      with SchemeConstantPropagationDomain
                                                      with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "CSFA_0"
    override val primPrecision = SchemePrelude.primNames
  }
}


/** Checks that results produced with manual primitives and preluded primitives
  * exactly match, using a simple type lattice */
/*object PrimitivesComparisonRQ2 {
  type Benchmark = String

  sealed trait BaseAddr extends Address { def printable = true }
  case class VarAddr(vrb: Identifier) extends BaseAddr { override def toString = s"<variable $vrb>" }
  case class PrmAddr(nam: String)     extends BaseAddr { override def toString = s"<primitive $nam>" }
  case class CarAddr(idn: Identity)   extends BaseAddr { override def toString = s"<car $idn>" }
  case class CdrAddr(idn: Identity)   extends BaseAddr { override def toString = s"<cdr $idn>" }
  case class RetAddr(idn: Identity)   extends BaseAddr { override def toString = s"<return $idn>" }
  case class PtrAddr(idn: Identity)   extends BaseAddr { override def toString = s"<pointer $idn>" }

  private def convertAddr(analysis: Analysis)(addr: analysis.Addr): BaseAddr = addr match {
    case analysis.ComponentAddr(_, analysis.VarAddr(v)) => VarAddr(v)
    case analysis.ComponentAddr(_, analysis.PrmAddr(n)) => PrmAddr(n)
    case analysis.ComponentAddr(_, analysis.PtrAddr(e)) => PtrAddr(e.idn)
    case analysis.ComponentAddr(_, analysis.CarAddr(e)) => CarAddr(e.idn)
    case analysis.ComponentAddr(_, analysis.CdrAddr(e)) => CdrAddr(e.idn)
    case analysis.ReturnAddr(cmp) => RetAddr(analysis.view(cmp).body.idn)
  }

  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics {
    val valueLattice: TypeSchemeLattice[Addr, Component]
  }
  type BaseValue = baseDomain.L
  val baseDomain = new TypeSchemeLattice[BaseAddr, Unit]
  val baseLattice = baseDomain.schemeLattice

  case class StubPrimitive(name: String) extends SchemePrimitive[BaseValue, BaseAddr] {
    def call(fpos: SchemeExp, args: List[(SchemeExp, BaseValue)], store: Store[BaseAddr,BaseValue], alloc: SchemeAllocator[BaseAddr]) =
      throw new Exception("Stub primitive: call not supported")
  }
  case class LambdaIdnEq(lambda: SchemeLambdaExp) extends SchemeLambdaExp {
    def idn = lambda.idn
    def args = lambda.args
    def body = lambda.body
    def varArgId = lambda.varArgId
    override def hashCode() = lambda.idn.hashCode()
    override def equals(that: Any) = that match {
      case LambdaIdnEq(l) => l.idn == this.lambda.idn
      case _ => false
    }
  }
  private def convertValue(analysis: Analysis)(value: analysis.Value): BaseValue = value match {
    case analysis.valueLattice.L(str, bool, num, char, sym, nil, prims, clos, consCells) =>
      baseDomain.L(str, bool, num, char, sym, nil,
        prims.map(p => StubPrimitive(p.name)) ++ clos.collect( {
          case (_, Some(name)) if SchemePrelude.primNames.contains(name) => StubPrimitive(name)
        }),
        clos
          .map({
            case ((e, env), name) =>
              /* Replace the expression, but keep its position.
               This is to avoid situations where we have the same expression, but it's parsed differently due to preludation (i.e., primitives/global) */
              val e2: SchemeLambdaExp = SchemeLambda(List(), List(SchemeValue(scalaam.language.sexp.ValueNil, e.idn)), e.idn)
              ((e2, ()), name) })
          .filter({
            /* Drops primitives are preluded */
            case (_, Some(name)) => !SchemePrelude.primNames.contains(name)
            case _ => true
          }),
        consCells.map({ case (car, cdr) => (convertAddr(analysis)(car), convertAddr(analysis)(cdr)) }))
  }

  type BaseStore = Map[BaseAddr, BaseValue]

  /**
    * Joins two stores
    * @param b1 a base store
    * @param b2 a base store
    * @return the joined base store
    * */
  protected def join(b1: BaseStore, b2: BaseStore): BaseStore =
    b2.foldLeft(b1) {
      case (acc, (addr2,value2)) =>
        val value1 = acc.getOrElse(addr2, baseLattice.bottom)
        val joined = baseLattice.join(value1,value2)
        acc + (addr2 -> joined)
    }

  /**
    *  Compare two stores, assuming one is more precise than the other
    *  @param b1 the less precise store
    *  @param b2 the more precise store
    *  @return the set of addresses that have been refined in b2 w.r.t. b1
    */
  protected def checkEquality(b1: BaseStore, b2: BaseStore): Unit =
    b1.foreach { case (addr1,value1) =>
      val value2 = b2.getOrElse(addr1,baseLattice.bottom)
      if (value1 != value2) {
        if (value2 == baseLattice.bottom) {
          /* We ignore this case. That's not entirely correct though. It corresponds to
           preluded primitive code in b1, that is not reached in b2 because
           there is no preludation for that primitive */
          ()
        } else {
          println(s"Addresses do not match: $addr1: $value1 vs $value2")
          println(s"Str equal? ${value1.str == value2.str}")
          println(s"Bool equal? ${value1.bool == value2.bool}")
          println(s"Num equal? ${value1.num == value2.num}")
          println(s"Char equal? ${value1.char == value2.char}")
          println(s"Sym equal? ${value1.sym == value2.sym}")
          println(s"Nil equal? ${value1.nil == value2.nil}")
          println(s"Prims equal? ${value1.prims == value2.prims}")
          println(s"Clos equal? ${value1.clos == value2.clos}")
          println(s"Cons equal? ${value1.consCells == value2.consCells}")
          assert(false)
        }
      }
    }

  /**
    *  Given an analysis (that terminated), extract its "base store": a mapping from base addresses to base values
    *  That is, convert the resulting store into one within the (context-insensitive) base domain
    *  @param analysis the analysis from which the results need to be extracted
    *  @return a store in the base domain
    */
  protected def extract(analysis: Analysis): BaseStore =
    analysis.store.groupBy(p => convertAddr(analysis)(p._1)).view
      .filterKeys(!_.isInstanceOf[PrmAddr])
      .mapValues(m => analysis.lattice.join(m.values))
      .mapValues(convertValue(analysis))
      .toMap

  /**
    * Run the analysis on a given program
    *
    * @param analysis a function that creates an analysis for a given program
      * @param program the program to analyze
    * @param name the name for the analysis (used for reporting to the console)
    * @param path the name of / path to the benchmark program to run
    * @param timeout (optional) the timeout
    * @return an option value, being:
    *     - the base store if the analysis terminated
    *     - `None` otherwise
    */
  protected def runAnalysis(analysis: SchemeExp => Analysis, name: String, program: SchemeExp, path: Benchmark, timeout: Timeout.T = Timeout.none): Option[BaseStore] = {
    try {
      val anl = analysis(program)
      println(s"... analysing $path using $name ...")
      anl.analyze(timeout)
      if (anl.finished()) {
        Some(extract(anl))
      } else {
        println("... timed out")
        None
      }
    } catch {
      case e: Exception =>
        println(s"Analyzer failed with exception $e")
        None
      case e: VirtualMachineError =>
        System.gc()
        println(s"Analyzer failed with error $e")
        None
    }
  }

  /** Run a benchmark
    *  @param benchmark the benchmark program to run
    */
  def runBenchmark(benchmark: Benchmark) = {
    val txt = Reader.loadFile(benchmark)
    val prg = SchemeParser.parse(txt)
    forBenchmark(benchmark, prg)
  }


  import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity._
  import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity.Sensitivity._

  def analysis1(p: SchemeExp) = new cli.evaluation.primitives.PerformanceType.AnalysisWithPreludedPrimitives(p) with S_CSFA_0 {
    override def toString = "P"
  }
  def analysis2(p: SchemeExp) = new cli.evaluation.primitives.PerformanceType.AnalysisWithManualPrimitives(p) with S_CSFA_0 {
    override def toString = "M"
  }

  def analysisTimeout() = Timeout.start(Duration(2, MINUTES))

  def forBenchmark(path: Benchmark, program: SchemeExp) = {
    runAnalysis(analysis1, "P", program, path, analysisTimeout) match {
      case Some(result1) =>
        runAnalysis(analysis2, "M", program, path, analysisTimeout) match {
          case Some(result2) =>
            checkEquality(result1, result2)
            println("match!")
          case None => ()
        }
      case None => ()
    }
  }

  // Only non-vector benchmarks
  val benchmarks = List(
    "test/kernighanvanwyk/ack.scm",
    "test/rsa.scm",
    "test/church.scm",
    "test/mceval.scm",
    "test/sat.scm",
    "test/regex.scm",
    "test/rsa.scm",
    "test/gambit/lattice.scm",
    "test/gambit/mazefun.scm",
    "test/gambit/perm9.scm",
    "test/gambit/nqueens.scm",
    "test/gambit/primes.scm",
    "test/gambit/sumloop.scm",
    "test/gabriel/browse.scm",
    "test/gabriel/dderiv.scm",
    "test/gabriel/destruc.scm",
    "test/gabriel/deriv.scm",
    "test/gabriel/diviter.scm",
    "test/gabriel/divrec.scm",
    "test/scp1-compressed/2.scm",
    "test/scp1-compressed/3.scm",
    "test/scp1-compressed/4.scm",
    "test/scp1-compressed/5.scm",
    "test/scp1-compressed/7.scm",
    "test/scp1-compressed/9.scm",
    "test/icp/icp_2_aeval.scm",
    "test/icp/icp_3_leval.scm",
 // "test/icp/icp_5_regsim.scm", // disabled because precision does not match!
    "test/icp/icp_8_compiler.scm"
  )

  def runBenchmarks() = {
    benchmarks.foreach(b => {
      System.gc()
      runBenchmark(b)
    })
  }
  def main(args: Array[String]) =
    runBenchmarks()
}*/

abstract class PrimitivesComparisonRQ3 extends PrimitivesComparison {
  def isPrim(nam: Option[String]): Boolean
  override def S_0_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                   with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_0_0
                                                   with SchemeConstantPropagationDomain
                                                   with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "0_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_CS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_CS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "CS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_2CS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_2CS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "2CS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_10CS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_10CS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "10CS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_2AcyclicCS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_2AcyclicCS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp]{
    override def toString() = "2AcyclicCS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_10AcyclicCS_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_10AcyclicCS_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "10AcyclicCS_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_FA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_FA_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "FA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_2FA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                     with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_2FA_0
                                                     with SchemeConstantPropagationDomain
                                                     with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "2FA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_10FA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_10FA_0
                                                    with SchemeConstantPropagationDomain
                                                    with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "10FA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
  def S_CSFA_0(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                      with SchemeModFCompoundSensitivities.SeparateLowHighSensitivity.S_CSFA_0
                                                      with SchemeConstantPropagationDomain
                                                      with LIFOWorklistAlgorithm[SchemeExp] {
    override def toString() = "CSFA_0"
    override def isPrimitive(nam: Option[String]): Boolean = isPrim(nam)
  }
}

object PrimitivesComparisonRQ3FullPrecision extends PrimitivesComparisonRQ3 {
  def isPrim(nam: Option[String]): Boolean = true
}

object PrimitivesComparisonRQ3AnonymousFunctions extends PrimitivesComparisonRQ3 {
  def isPrim(nam: Option[String]): Boolean = nam match {
    case Some(n) => SchemePrelude.primNames.contains(n)
    case None => true /* analyze anonymous functions with high sensitivity */
  }
}

object PrimitivesComparisonRQ3NamedFunctions extends PrimitivesComparisonRQ3 {
  def isPrim(nam: Option[String]): Boolean = nam.isDefined /* analyze named functions with high sensitivity */
}
