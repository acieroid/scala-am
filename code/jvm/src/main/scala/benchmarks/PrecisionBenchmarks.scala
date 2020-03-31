package scalaam.cli.benchmarks

import scalaam.cli._
import scalaam.core._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scala.concurrent.duration._
import scalaam.lattice._


abstract class PrecisionBenchmarks[
    Num: IntLattice,
    Rea: RealLattice,
    Bln: BoolLattice,
    Chr: CharLattice,
    Str: StringLattice, 
    Smb: SymbolLattice
] {

    type Benchmark = String
    type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics
                                           with AbstractDomain {
        val valueLattice: ModularSchemeLattice[Addr,Component,Str,Bln,Num,Rea,Chr,Smb]
    }

    trait BaseAddr extends Address { def printable = true }
    case class VarAddr(vrb: Identifier) extends BaseAddr {
        override def toString = s"<var $vrb>"
    }
    case class PrmAddr(nam: String) extends BaseAddr {
        override def toString = s"<prim $nam>"
    }
    case class RetAddr(exp: SchemeExp) extends BaseAddr {
        override def toString = s"<return ${exp.idn}>"
    }
    case class PtrAddr(exp: Expression, ctx: Any) extends BaseAddr {
        override def toString = s"<pointer ${exp.idn} ($ctx)>"
    }
    private def convertAddr(analysis: Analysis)(addr: analysis.Addr): BaseAddr = addr match {
        case analysis.ComponentAddr(_, analysis.VarAddr(v)) => VarAddr(v)
        case analysis.ComponentAddr(_, analysis.PrmAddr(n)) => PrmAddr(n)
        case analysis.ComponentAddr(_, analysis.PtrAddr(e,c)) => PtrAddr(e,c)
        case analysis.ReturnAddr(cmp) => RetAddr(analysis.view(cmp).body)
    }

    type BaseValue = baseDomain.L
    val baseDomain = new ModularSchemeLattice[BaseAddr,Unit,Str,Bln,Num,Rea,Chr,Smb]
    val baseLattice = baseDomain.schemeLattice
    case class StubPrimitive(name: String) extends SchemePrimitive[BaseValue, BaseAddr] {
        def call(fexp: SchemeExp, args: List[(SchemeExp, BaseValue)], store: Store[BaseAddr,BaseValue], alloc: SchemeAllocator[BaseAddr]) = 
            throw new Exception("Stub primitive: call not supported")
    }
    private def convertV(analysis: Analysis)(value: analysis.valueLattice.Value): baseDomain.Value = value match {
        case analysis.valueLattice.Bot          => baseDomain.Bot
        case analysis.valueLattice.Nil          => baseDomain.Nil
        case analysis.valueLattice.Bool(b)      => baseDomain.Bool(b)
        case analysis.valueLattice.Int(i)       => baseDomain.Int(i)
        case analysis.valueLattice.Real(r)      => baseDomain.Real(r)
        case analysis.valueLattice.Char(c)      => baseDomain.Char(c)
        case analysis.valueLattice.Str(s)       => baseDomain.Str(s)
        case analysis.valueLattice.Symbol(s)    => baseDomain.Symbol(s)
        case analysis.valueLattice.Prim(p)      => baseDomain.Prim(StubPrimitive(p.name))
        case analysis.valueLattice.Clo(l,_,n)   => baseDomain.Clo(l,(),n)
        case analysis.valueLattice.Cons(a,d)    => baseDomain.Cons(convertAddr(analysis)(a), convertAddr(analysis)(d))
        case analysis.valueLattice.Pointer(a)   => baseDomain.Pointer(convertAddr(analysis)(a))
        case analysis.valueLattice.Vec(s,e,i)   => baseDomain.Vec(s,e.view.mapValues(convertValue(analysis)).toMap,convertValue(analysis)(i)) 
    }
    private def convertValue(analysis: Analysis)(value: analysis.Value): BaseValue = value match {
        case analysis.valueLattice.Element(v)   => baseDomain.Element(convertV(analysis)(v))
        case analysis.valueLattice.Elements(vs) => vs.map(convertV(analysis)) match {
            case vs if vs.size == 1 => baseDomain.Element(vs.head)
            case vs => baseDomain.Elements(vs)
        }
    }
 
    type BaseStore = Map[BaseAddr, BaseValue]

    // the precision comparison is parameterized by:
    // - the base analysis (= lowest precision) to compare to
    // - the other analyses to compare to the base analysis
    def baseAnalysis(prg: SchemeExp): Analysis
    def analyses(prg: SchemeExp): List[Analysis]

    // and can, optionally, be configured in its timeout (default: 2min.)
    def timeoutDuration = Duration(2, MINUTES)


    /**
      *  Given an analysis (that terminated), extract a mapping from base addresses to base values
      *  That is, convert the resulting store into one within the (context-insensitive) base domain
      */ 
    private def extractAll(analysis: Analysis): BaseStore =
       analysis.store.groupBy(p => convertAddr(analysis)(p._1)).view
                     .mapValues(m => analysis.lattice.join(m.values))
                     .mapValues(convertValue(analysis))
                     .toMap    
    /**
      *  Like `extractAll`, but omits the addresses for primitives from the store
      */ 
    private def extract(analysis: Analysis): BaseStore = 
        extractAll(analysis).view.filterKeys(!_.isInstanceOf[PrmAddr]).toMap

    /** 
     *  Compare two stores b1 and b2, assuming b1 is less precise than b2
     *  Returns the set of addresses that have been refined.
     */
    def compareOrdered(b1: BaseStore, b2: BaseStore): Set[BaseAddr] =
        b1.foldLeft(Set.empty[BaseAddr]) { case (acc,(addr1,value1)) =>
            val value2 = b2.getOrElse(addr1,baseLattice.bottom)
            if (value1 != value2) {
                assert(baseLattice.subsumes(value1,value2))
                //println(s"[$addr1] value $value1 has been refined to $value2")
                acc + addr1
            } else {
                acc
            }
        }

    /**
     * A more generic version of 'compareOrdered' to compare any two stores b1 and b2.
     *  @return a pair of:
     *  - the set of addresses whose abstract values have been refined (= are now more precise)
     *  - the set of addresses whose abstract values have lost precision
     */
    def compare(b1: BaseStore, b2: BaseStore): (Set[BaseAddr],Set[BaseAddr]) = {
        val allKeys = b1.keySet ++ b2.keySet
        allKeys.foldLeft((Set.empty[BaseAddr],Set.empty[BaseAddr])) { (acc,addr) => 
            val value1 = b1.getOrElse(addr, baseLattice.bottom)
            val value2 = b2.getOrElse(addr, baseLattice.bottom)
            if (value1 == value2) {
                acc 
            } else if (baseLattice.subsumes(value1,value2)) {
                (acc._1 + addr, acc._2)
            } else if (baseLattice.subsumes(value2,value1)) {
                (acc._1, acc._2 + addr)
            } else { // neither value is more precise than the other
                acc
            }
        }
    }

    /**
      * Run a benchmark
      *
      * @param path the benchmark to run
      * @return a pair of:
      * - the extracted store of the base analysis
      * - for each other analysis, the extracted store of the analyses
      */
    def runBenchmark(path: Benchmark): (BaseStore, Map[Analysis,BaseStore]) = {
        val txt = FileUtil.loadFile(path)
        val prg = SchemeParser.parse(txt)
        // run the base analysis first
        val base = baseAnalysis(prg)
        println(s"... analysing $path using base analysis $base ...")
        base.analyze()
        val baseResult = extract(base)
        // run the other analyses on the benchmark
        val results = analyses(prg).foldLeft(Map.empty[Analysis,BaseStore]) { (results,analysis) =>
            println(s"... analysing $path using analysis $analysis ...") 
            analysis.analyze()
            val otherResult = extract(analysis)
            results + (analysis -> otherResult) 
        }
        // return a pair of the result of the base analysis and the results of the other analyses
        (baseResult, results)
    }

    /** 
     * Runs all the analyses, then compares precision of each analysis with that of the base analysis
     * @param path the path to the benchmark to compare the analyses on
     * @return a pair of:
     * - a mapping from analysis to the number of values that have been refined in the store
     * - the total number of values in the store of the base analysis
     */ 
    def compareAll(path: Benchmark): (Map[Analysis,Int],Int) = {
        val (baseResult, otherResults) = runBenchmark(path)
        val improvements = otherResults.view.mapValues(compareOrdered(baseResult,_).size).toMap
        val total = baseResult.count(p => baseLattice.cardinality(p._2) != CardinalityNumber(1))
        (improvements,total)
    }
}

object Analyses {
    def contextInsensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                          with BigStepSemantics
                                                                          with NoSensitivity
                                                                          with ConstantPropagationDomain {
        override def toString() = "no-sensitivity"
    }
    def contextSensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                        with BigStepSemantics
                                                                        with FullArgumentSensitivity
                                                                        with ConstantPropagationDomain {
        override def toString() = "full-argument-sensitivity"
    }
    def adaptiveAnalysisPolicy1(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy1
                                                                                        with EagerAdaptiveArgumentSelection
                                                                                        with ConstantPropagationDomain {
        override def toString() = "adaptive-argument-policy1"
        val limit = k
        override def allocCtx(clo: lattice.Closure, args: List[Value]) = super.allocCtx(clo,args)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveAnalysisPolicy2(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy2
                                                                                        with EagerAdaptiveArgumentSelection
                                                                                        with ConstantPropagationDomain {
        override def toString() = "adaptive-argument-policy2"
        val limit = k
        override def allocCtx(clo: lattice.Closure, args: List[Value]) = super.allocCtx(clo,args)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
}

object PrecisionComparison1 extends PrecisionBenchmarks[
    ConstantPropagation.I,
    ConstantPropagation.R,
    Concrete.B,
    ConstantPropagation.C,
    ConstantPropagation.S,
    Concrete.Sym
] {
    def baseAnalysis(prg: SchemeExp): Analysis = Analyses.contextInsensitiveAnalysis(prg)
    def analyses(prg: SchemeExp) = List(
        Analyses.contextSensitiveAnalysis(prg)
        //Analyses.adaptiveAnalysisPolicy1(prg, 10)
        //Analyses.adaptiveAnalysisPolicy2(prg, 10)
    )

    def benchmarks = List(
        "test/blur.scm",
        "test/bound-precision.scm",
        "test/church-2-num.scm",
        "test/church-6.scm",
        "test/church.scm",
        "test/collatz.scm",
        "test/count.scm",
        "test/eta.scm",
        "test/fact.scm",
        "test/fib.scm",
        "test/gcipd.scm",
        //"test/grid.scm",
        "test/inc.scm",
        "test/infinite-1.scm",
        //"test/infinite-2.scm",
        //"test/infinite-3.scm",
        "test/kcfa2.scm",
        "test/kcfa3.scm",
        "test/kernighanvanwyk/ack.scm",
        "test/letrec-begin.scm",
        "test/loop2.scm",
        //"test/mceval.scm",
        "test/mj09.scm",
        "test/mut-rec.scm",
        "test/my-list.scm",
        "test/nested-defines.scm",
        //"test/primtest.scm",
        //"test/quasiquoting-simple.scm",
        //"test/quasiquoting.scm",
        "test/regex.scm",
        "test/rotate.scm",
        "test/rsa.scm",
        "test/sat.scm",
        //"test/scm2c.scm",     // various unsupported primitives
        //"test/scm2java.scm",  // various unsupported primitives
        "test/sq.scm",
        //"test/Streams.scm",   // define-macro
        "test/sym.scm",
        "test/widen.scm",
        "test/work.scm"
    )

    def main(args: Array[String]): Unit = runMainBenchmarks()

    def check(path: Benchmark) = {
        val (base, others) = runBenchmark(path)
        println(s"==> base analysis")
        base.foreach(p => println(s"${p._1} -> ${p._2}"))
        others.foreach { case (analysis, other) =>
            println(s"==> $analysis")
            other.foreach(p => println(s"${p._1} -> ${p._2}"))
        }
    }

    def runMainBenchmarks() = 
        benchmarks.map(b => (b, compareAll(b)))
                  .foreach { case (benchmark, (results, total)) =>
                    println(s"$benchmark: ${results.values.sum}/$total")
                  }
}