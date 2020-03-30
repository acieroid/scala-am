package scalaam.cli.benchmarks

import scalaam.cli._
import scalaam.core._
import scalaam.util._
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
    case class VarAddr(vrb: Identifier) extends BaseAddr
    case class PrmAddr(nam: String) extends BaseAddr
    case class RetAddr(cmp: SchemeExp) extends BaseAddr
    case class PtrAddr(exp: Expression, ctx: Any) extends BaseAddr
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
        case analysis.valueLattice.Elements(vs) => baseDomain.Elements(vs.map(convertV(analysis)))
    }
 
    case class PrecisionResult(singletons: Int, total: Int)

    // the precision comparison is parameterized by:
    // - the benchmark programs you want to use
    // - the different analyses you want to compare
    def benchmarks: List[Benchmark]
    def analyses(prg: SchemeExp): List[Analysis]

    // and can, optionally, be configured in its timeout (default: 2min.)
    def timeoutDuration = Duration(2, MINUTES)

    // running the benchmarks
    private def runBenchmarks(): List[(Benchmark,Analysis)] =
        benchmarks.flatMap { path =>
            val txt = FileUtil.loadFile(path)
            val prg = SchemeParser.parse(txt)
            analyses(prg).map { analysis =>
                println(s"... analysing $path using $analysis ...")
                analysis.analyze(Timeout.start(timeoutDuration))
                (path, analysis)
            }
        }

    // extract information (= abstract value) per program point (only if the analysis terminated)
    type BaseStore = Map[BaseAddr, BaseValue]
    private def extract(analysis: Analysis): BaseStore =
       analysis.store.groupBy(p => convertAddr(analysis)(p._1))
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
                acc + addr1
            } else {
                acc
            }
        }

    /**
     * A more generic version of 'compareOrdered' to compare any two stores b1 and b2
     *  returns a pair of:
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

    /** Runs all the analyses, then compares precision of each analysis with the previous one */
    def compareAll(): List[(Set[BaseAddr],Set[BaseAddr])] = ???
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
    ConstantPropagation.Sym
] {
    def benchmarks = List(
        "test/my-list.scm"
    )
    def analyses(prg: SchemeExp) = List(
        Analyses.contextInsensitiveAnalysis(prg),
        Analyses.contextSensitiveAnalysis(prg)
        //Analyses.adaptiveAnalysisPolicy1(prg, 10)
        //Analyses.adaptiveAnalysisPolicy2(prg, 10)
    )
}

object PrecisionComparisonMain {
    def main(args: Array[String]) = ???
}