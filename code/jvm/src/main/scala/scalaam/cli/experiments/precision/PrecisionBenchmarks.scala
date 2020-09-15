package scalaam.cli.experiments.precision

import scalaam.core._
import scalaam.language.scheme.SchemeInterpreter.Addr
import scalaam.language.scheme._
import scalaam.language.scheme.lattices.ModularSchemeLattice
import scalaam.language.scheme.primitives._
import scalaam.lattice._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

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
                                           with ModularSchemeDomain {
        val modularLatticeWrapper: ModularSchemeLatticeWrapper {
            val modularLattice: ModularSchemeLattice[Addr,Str,Bln,Num,Rea,Chr,Smb]
        }
    }

    sealed trait BaseAddr extends Address { def printable = true }
    case class VarAddr(vrb: Identifier) extends BaseAddr { def idn = vrb.idn ; override def toString = s"<variable $vrb>" }
    case class PrmAddr(nam: String)     extends BaseAddr { def idn = Identity.none ; override def toString = s"<primitive $nam>" }
    case class RetAddr(idn: Identity)   extends BaseAddr { override def toString = s"<return $idn>" }
    case class PtrAddr(idn: Identity)   extends BaseAddr { override def toString = s"<pointer $idn>" }
    
    private def convertAddr(analysis: Analysis)(addr: analysis.Addr): BaseAddr = addr match {
        case scalaam.modular.scheme.VarAddr(vrb, _) => VarAddr(vrb)
        case scalaam.modular.scheme.PtrAddr(exp, _) => PtrAddr(exp.idn)
        case scalaam.modular.ReturnAddr(_, idn)     => RetAddr(idn)
        case scalaam.modular.scheme.PrmAddr(nam)    => PrmAddr(nam)
        case a                                      => throw new Exception(s"Cannot convert address: $a")
    }

    type BaseValue = baseDomain.L
    val baseDomain = new ModularSchemeLattice[BaseAddr,Str,Bln,Num,Rea,Chr,Smb]
    val baseLattice = baseDomain.schemeLattice
    case class StubPrimitive(name: String) extends SchemePrimitive[BaseValue, BaseAddr] {
        def call(fpos: SchemeExp, args: List[(SchemeExp, BaseValue)], store: Store[BaseAddr,BaseValue], scheme: SchemeInterpreterBridge[BaseValue, BaseAddr]) =
            throw new Exception("Stub primitive: call not supported")
    }
    val emptyEnv = Environment[BaseAddr](Iterable.empty)
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
    private def convertV(analysis: Analysis)(value: analysis.modularLatticeWrapper.modularLattice.Value): baseDomain.Value = value match {
        case analysis.modularLatticeWrapper.modularLattice.Nil => baseDomain.Nil
        case analysis.modularLatticeWrapper.modularLattice.Bool(b)      => baseDomain.Bool(b)
        case analysis.modularLatticeWrapper.modularLattice.Int(i)       => baseDomain.Int(i)
        case analysis.modularLatticeWrapper.modularLattice.Real(r)      => baseDomain.Real(r)
        case analysis.modularLatticeWrapper.modularLattice.Char(c)      => baseDomain.Char(c)
        case analysis.modularLatticeWrapper.modularLattice.Str(s)       => baseDomain.Str(s)
        case analysis.modularLatticeWrapper.modularLattice.Symbol(s)    => baseDomain.Symbol(s)
        case analysis.modularLatticeWrapper.modularLattice.Prim(ps)     => baseDomain.Prim(ps.map(p => StubPrimitive(p.name)))
        case analysis.modularLatticeWrapper.modularLattice.Clo(cs)      => baseDomain.Clo(cs.map(c => ((LambdaIdnEq(c._1._1),emptyEnv),None)))
        case analysis.modularLatticeWrapper.modularLattice.Cons(a,d)    => baseDomain.Cons(convertValue(analysis)(a), convertValue(analysis)(d))
        case analysis.modularLatticeWrapper.modularLattice.Pointer(ps)  => baseDomain.Pointer(ps.map(convertAddr(analysis)(_)))
        case analysis.modularLatticeWrapper.modularLattice.Vec(s,e)     => baseDomain.Vec(s,e.view.mapValues(convertValue(analysis)).toMap)
        case analysis.modularLatticeWrapper.modularLattice.Void         => baseDomain.Void
        case analysis.modularLatticeWrapper.modularLattice.Lock(tids)   => baseDomain.Lock(tids)
        case analysis.modularLatticeWrapper.modularLattice.Thread(tids) => baseDomain.Thread(tids)
        case v                                  => throw new Exception(s"Unsupported value type for conversion: ${v.ord}.")
    }
    private def convertValue(analysis: Analysis)(value: analysis.Value): BaseValue = value match {
        case analysis.modularLatticeWrapper.modularLattice.Elements(vs) => baseDomain.Elements(vs.map(convertV(analysis)))
    }
    private def convertConcreteAddr(addr: SchemeInterpreter.Addr): BaseAddr = addr._2 match {
        case SchemeInterpreter.AddrInfo.VarAddr(v) => VarAddr(v)
        case SchemeInterpreter.AddrInfo.PrmAddr(p) => PrmAddr(p)
        case SchemeInterpreter.AddrInfo.PtrAddr(p) => PtrAddr(p.idn)
        case SchemeInterpreter.AddrInfo.RetAddr(r) => RetAddr(r.idn)
    }
    private def convertConcreteValue(value: SchemeInterpreter.Value): BaseValue = value match {
        case SchemeInterpreter.Value.Nil                => baseLattice.nil
        case SchemeInterpreter.Value.Undefined(_)       => baseLattice.bottom
        case SchemeInterpreter.Value.Clo(l, _, _)       => baseLattice.closure((LambdaIdnEq(l),emptyEnv),None) // TODO: when names are added to the abstract interpreter, preserve that information here
        case SchemeInterpreter.Value.Primitive(p)       => baseLattice.primitive(StubPrimitive(p.name))
        case SchemeInterpreter.Value.Str(s)             => baseLattice.string(s)
        case SchemeInterpreter.Value.Symbol(s)          => baseLattice.symbol(s)
        case SchemeInterpreter.Value.Integer(i)         => baseLattice.number(i)
        case SchemeInterpreter.Value.Real(r)            => baseLattice.real(r)
        case SchemeInterpreter.Value.Bool(b)            => baseLattice.bool(b)
        case SchemeInterpreter.Value.Character(c)       => baseLattice.char(c)
        case SchemeInterpreter.Value.Cons(a,d)          => baseLattice.cons(convertConcreteValue(a),convertConcreteValue(d))
        case SchemeInterpreter.Value.Pointer(a)         => baseLattice.pointer(convertConcreteAddr(a))
        case SchemeInterpreter.Value.Vector(siz,els,_)  =>
            def convertNumber(n: Int): Num = baseLattice.number(n) match {
                case baseDomain.Elements(vs) => vs.head.asInstanceOf[baseDomain.Int].i
            }
            val cSiz = convertNumber(siz)
            val cEls = els.foldLeft(Map[Num,BaseValue]()) { case (acc,(idx,vlu)) =>
                val cIdx = convertNumber(idx)
                val cVlu = convertConcreteValue(vlu)
                val prevVlu = acc.getOrElse(cIdx, baseLattice.bottom)
                val newVlu = baseLattice.join(cVlu, prevVlu)
                acc + (cIdx -> newVlu)
            }
            baseDomain.Element(baseDomain.Vec(cSiz, cEls))
        case v                                          => throw new Exception(s"Unsupported value for concrete conversion: $v")
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
    protected def compareOrdered(b1: BaseStore, b2: BaseStore): Set[BaseAddr] =
        b1.foldLeft(Set.empty[BaseAddr]) { case (acc,(addr1,value1)) =>
            val value2 = b2.getOrElse(addr1,baseLattice.bottom)
            if (value1 != value2) {
                if(!baseLattice.subsumes(value1,value2)) {
                    println(addr1)
                    println(value1)
                    println(value2)
                }
                assert(baseLattice.subsumes(value1,value2))
                //println(s"[$addr1] value $value1 has been refined to $value2")
                acc + addr1
            } else {
                acc
            }
        }

    /**
     * A more generic version of 'compareOrdered' to compare any two stores b1 and b2
     * (i.e., without any assumption on one being more/less precise than the other)
     *  @param b1 a base store
     *  @param b2 a base store
     *  @return a pair of:
     *  - the set of addresses whose abstract values have been refined (= are now more precise)
     *  - the set of addresses whose abstract values have lost precision
     */
    protected def compareAny(b1: BaseStore, b2: BaseStore): (Set[BaseAddr],Set[BaseAddr]) = {
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
      *  Given a concrete interpreter (that terminated), extract its "base store": a mapping from base addresses to base values
      *  That is, convert the resulting store into one within the (context-insensitive) base domain
      *  @param interpreter the concrete interpreter from which the results need to be extracted
      *  @return a store in the base domain
      */ 
    protected def extract(interpreter: SchemeInterpreter): BaseStore =
        interpreter.store.view
                         .mapValues(convertConcreteValue)
                         .groupBy(p => convertConcreteAddr(p._1)).view
                         .filterKeys(!_.isInstanceOf[PrmAddr])
                         .mapValues(m => baseLattice.join(m.map(_._2)))
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

    /**
      * Run the concrete interpreter on a given program
      *
      * @param program the Scheme program to run the interpreter on
      * @param path the name of / path to the benchmark program to run
      * @param timeout (optional) the timeout
      * @param times how many times the interpreter needs to run, results are joined together
      * @return an option value, being:
      *     - the base (joined) store if the interpreter (always) terminated
      *     - `None` otherwise
      */
    protected def runInterpreter(program: SchemeExp, path: Benchmark, timeout: Timeout.T = Timeout.none, times: Int = 1): Option[BaseStore] = {
        print(s"Running concrete interpreter on $path ($times times)")
        val preluded = SchemePrelude.addPrelude(program)
        val undefined = SchemeParser.undefine(List(preluded))
        var baseStore: BaseStore = Map.empty
        try {
            for(_ <- 1 to times) {
                print(".")
                val interpreter = new SchemeInterpreter((i, v) => (), false)
                interpreter.run(undefined, timeout)
                baseStore = join(baseStore,extract(interpreter))
            } 
            println()
            Some(baseStore)
        } catch {
            case e: Exception =>
                println(s"Concrete interpreter failed with $e")
                None
            case e: VirtualMachineError =>
                System.gc()
                println(s"Concrete interpreter failed with $e")
                None
        }
    }

    /**
     * Specify what needs to be done for a given benchmark program
     * @param path the path to the benchmark program
     * @param program the Scheme expression of the entire program
     */
    protected def forBenchmark(path: Benchmark, program: SchemeExp): Unit

    /** Run a benchmark
     *  @param benchmark the benchmark program to run
     */
    def runBenchmark(benchmark: Benchmark) = {
        val txt = Reader.loadFile(benchmark)
        val prg = SchemeParser.parse(txt)
        forBenchmark(benchmark, prg)
    }
}
