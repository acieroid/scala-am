package scalaam.cli.benchmarks.precision

import scalaam.cli._
import scalaam.cli.benchmarks._
import scalaam.util._
import scalaam.core._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
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

    sealed trait BaseAddr extends Address { def printable = true }
    case class VarAddr(vrb: Identifier) extends BaseAddr { override def toString = s"<variable $vrb>" }
    case class PrmAddr(nam: String)     extends BaseAddr { override def toString = s"<primitive $nam>" }
    case class CarAddr(exp: SchemeExp)  extends BaseAddr { override def toString = s"<car ${exp.idn}>" }
    case class CdrAddr(exp: SchemeExp)  extends BaseAddr { override def toString = s"<cdr ${exp.idn}>" }
    case class RetAddr(exp: SchemeExp)  extends BaseAddr { override def toString = s"<return ${exp.idn}>" }
    case class PtrAddr(exp: SchemeExp)  extends BaseAddr { override def toString = s"<pointer ${exp.idn}>" }
    
    private def convertAddr(analysis: Analysis)(addr: analysis.Addr): BaseAddr = addr match {
        case analysis.ComponentAddr(_, analysis.VarAddr(v)) => VarAddr(v)
        case analysis.ComponentAddr(_, analysis.PrmAddr(n)) => PrmAddr(n)
        case analysis.ComponentAddr(_, analysis.PtrAddr(e)) => PtrAddr(e)
        case analysis.ComponentAddr(_, analysis.CarAddr(e)) => CarAddr(e)
        case analysis.ComponentAddr(_, analysis.CdrAddr(e)) => CdrAddr(e)
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
        case analysis.valueLattice.Clo(l,_,_)   => baseDomain.Clo(l,(),None)
        case analysis.valueLattice.Cons(a,d)    => baseDomain.Cons(convertAddr(analysis)(a), convertAddr(analysis)(d))
        case analysis.valueLattice.Pointer(_)   => throw new Exception("Vectors not supported in precision benchmarks") //baseDomain.Pointer(convertAddr(analysis)(a))
        case analysis.valueLattice.Vec(_,_,_)   => throw new Exception("Vectors not supported in precision benchmarks") //baseDomain.Vec(s,e.view.mapValues(convertValue(analysis)).toMap,convertValue(analysis)(i)) 
    }
    private def convertValue(analysis: Analysis)(value: analysis.Value): BaseValue = value match {
        case analysis.valueLattice.Element(v)   => baseDomain.Element(convertV(analysis)(v))
        case analysis.valueLattice.Elements(vs) => vs.map(convertV(analysis)) match {
            case vs if vs.size == 1 => baseDomain.Element(vs.head)
            case vs => baseDomain.Elements(vs)
        }
    }
    private def convertConcreteAddr(addr: SchemeInterpreter.Addr): BaseAddr = addr._2 match {
        case SchemeInterpreter.AddrInfo.VarAddr(v) => VarAddr(v)
        case SchemeInterpreter.AddrInfo.PrmAddr(p) => PrmAddr(p)
        case SchemeInterpreter.AddrInfo.PtrAddr(p) => PtrAddr(p)
        case SchemeInterpreter.AddrInfo.CarAddr(p) => CarAddr(p)
        case SchemeInterpreter.AddrInfo.CdrAddr(p) => CdrAddr(p)
        case SchemeInterpreter.AddrInfo.RetAddr(r) => RetAddr(r)
    }
    private def convertConcreteValue(value: SchemeInterpreter.Value): BaseValue = value match {
        case SchemeInterpreter.Value.Nil                => baseLattice.nil
        case SchemeInterpreter.Value.Clo(lambda, _)     => baseLattice.closure((lambda,()),None)
        case SchemeInterpreter.Value.Primitive(p)       => baseLattice.primitive(StubPrimitive(p.name))
        case SchemeInterpreter.Value.Str(s)             => baseLattice.string(s)
        case SchemeInterpreter.Value.Symbol(s)          => baseLattice.symbol(s)
        case SchemeInterpreter.Value.Integer(i)         => baseLattice.number(i)
        case SchemeInterpreter.Value.Real(r)            => baseLattice.real(r)
        case SchemeInterpreter.Value.Bool(b)            => baseLattice.bool(b)
        case SchemeInterpreter.Value.Character(c)       => baseLattice.char(c)
        case SchemeInterpreter.Value.Cons(a,d)          => baseLattice.cons(convertConcreteAddr(a),convertConcreteAddr(d))
        case SchemeInterpreter.Value.Vector(_)          => throw new Exception("Vectors are not supported in precision benchmarks")
        case _                                          => throw new Exception("Unsupported concrete value for precision benchmarks")
    }
 
    type BaseStore = Map[BaseAddr, BaseValue]

    /** Joining stores */
    private def join(b1: BaseStore, b2: BaseStore): BaseStore = 
        b2.foldLeft(b1) {
            case (acc, (addr2,value2)) => 
                val value1 = acc.getOrElse(addr2, baseLattice.bottom)
                val joined = baseLattice.join(value1,value2)
                acc + (addr2 -> joined)
        }

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
    private def extract(analysis: Analysis): BaseStore =
       analysis.store.groupBy(p => convertAddr(analysis)(p._1)).view
                     .filterKeys(!_.isInstanceOf[PrmAddr])
                     .mapValues(m => analysis.lattice.join(m.values))
                     .mapValues(convertValue(analysis))
                     .toMap 
                
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

    def concreteTimeoutDuration = Duration(2, MINUTES)
    private def extract(interpreter: SchemeInterpreter): BaseStore = 
        interpreter.store.view
                         .mapValues(convertConcreteValue)
                         .groupBy(p => convertConcreteAddr(p._1)).view
                         .filterKeys(!_.isInstanceOf[PrmAddr])
                         .mapValues(m => baseLattice.join(m.map(_._2)))
                         .toMap
                         
    def runConcrete(path: Benchmark, times: Int): BaseStore = {
        val txt = FileUtil.loadFile(path)
        val prg = SchemeUndefiner.undefine(List(SchemeParser.parse(txt)))
        var baseStore: BaseStore = Map.empty
        print("Running concrete interpreter")
        for(_ <- 1 to times) {
            print(".")
            val interpreter = new SchemeInterpreter((i, v) => (), false)
            interpreter.run(prg, Timeout.start(concreteTimeoutDuration))
            baseStore = join(baseStore, extract(interpreter))
        } 
        println()
        return baseStore
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
    def baseAnalysis(prg: SchemeExp): Analysis = 
        SchemeAnalyses.contextInsensitiveAnalysis(prg)
    def analyses(prg: SchemeExp) = List(
        SchemeAnalyses.contextSensitiveAnalysis(prg)
        //Analyses.adaptiveAnalysisPolicy1(prg, 10)
        //Analyses.adaptiveAnalysisPolicy2(prg, 10)
    )

    def main(args: Array[String]): Unit = checkConcrete("test/primtest.scm")

    def checkConcrete(path: Benchmark) = {
        val base = runConcrete(path, 5)
        base.foreach(p => println(s"${p._1} -> ${p._2}"))
    }

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
        SchemeBenchmarks.standard
                        .map(b => (b, compareAll(b)))
                        .foreach { case (benchmark, (results, total)) =>
                            println(s"$benchmark: ${results.values.sum}/$total")
                        }
}