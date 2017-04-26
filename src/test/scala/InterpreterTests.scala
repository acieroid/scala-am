import org.scalatest._
import org.scalatest.prop._
import scala.concurrent.duration.Duration

case class Benchmark[Abs : JoinLattice](name: String, file: String, expected: Abs,
  slowconcrete: Boolean = false, slowabstract: Boolean = false, terminates: Boolean = true, works: Boolean = true) {
  def runnable(concrete: Boolean): Boolean =
    works && (if (concrete) { !slowconcrete && terminates } else { !slowabstract })
}
trait BenchmarkFiles {
  val timeout: Duration = Duration(2, "seconds")

  def benchmarks(lattice: SchemeLattice): List[Benchmark[lattice.L]] = {
    implicit val abs = lattice.isSchemeLattice
    List(
      /* Gabriel benchmarks, see http://www.larcenists.org/Twobit/benchmarksAbout.html */
      // Benchmark("boyer", "test/boyer.scm", abs.inject(true), slow = true), // takes too long to finish
      // TODO: browse (do notation)
      Benchmark("cpstak", "test/gabriel/cpstak.scm", abs.inject(6)),
      // TODO: ctak (call/cc)
      Benchmark("dderiv", "test/gabriel/dderiv.scm", abs.inject(true)),
      Benchmark("deriv", "test/gabriel/deriv.scm", abs.inject(true)),
      // TODO: destruc (do notation)
      // TODO: diviter (do notation)
      Benchmark("divrec", "test/gabriel/divrec.scm", abs.inject(true)),
      // TODO: puzzle (do notation)
      Benchmark("takl", "test/gabriel/takl.scm", abs.inject(true)),
      // TODO: puzzle (do notation)

      /* Kernighan and Van Wyk benchmarks, see http://www.larcenists.org/Twobit/benchmarksAbout.html */
      Benchmark("ack", "test/kernighanvanwyk/ack.scm", abs.inject(4)),
      // TODO: array1 (do notation)
      // TODO: cat (input/output)
      // TODO: string (do notation, substring)
      // TODO: sum1: (input/output)
      // TODO: sumloop (do notation)
      // TODO: tail (input/output)
      // TODO: wc (input/output)

      /* Gambit benchmarks, see http://www.larcenists.org/Twobit/benchmarksAbout.html, and http://github.com/gambit/gambit */
      Benchmark("nqueens", "test/gambit/nqueens.scm", abs.inject(92)),
      // Benchmark("array1", "test/gambit/array1.scm", abs.inject(true)), // named let
      // Benchmark("browse", "test/gambit/browse.scm", abs.inject(1101)), // named let
      // Benchmark("cat", "test/gambit/cat.scm", ???) // rely on file io
      // Benchmark("compiler", "test/gambit/compiler.scm", ???)
      // Benchmark("ctak.scm", "test/gambit/ctak.scm", abs.inject(true)) // call/cc
      // Benchmark("destruc", "test/gambit/destruc.scm", abs.inject(true)), // named let
      // Benchmark("diviter", "test/gambit/diviter.scm", abs.inject(true)), // do notation
      // Benchmark("earley", "test/gambit/earley.scm", abs.inject(true)), // named let
      // Benchmark("fibc", "test/gambit/fibc.scm", abs.inject(true)), // call/cc
      // Benchmark("graphs", "test/gambit/graphs.scm", abs.inject(true)), // named let
      // Benchmark("lattice", "test/gambit/lattice.scm", abs.inject(true)), // missing primitives
      // Benchmark("matrix", "test/gambit/matrix.scm", abs.inject(true)), // named let
      // Benchmark("mazefun", "test/gambit/mazefun.scm", abs.inject(true)), // missing primitives
      // Benchmark("nboyer", "test/gambit/nboyer.scm", abs.inject(true)), // dot notation
      // Benchmark("paraffins", "test/gambit/paraffins.scm", abs.inject(true)), // named let
      // Benchmark("perm9", "test/gambit/perm9.scm", abs.inject(true)), // missing primitives
      // Benchmark("peval", "test/gambit/peval.scm", abs.inject(true)), // named let
      Benchmark("primes", "test/gambit/primes.scm", abs.inject(true)),
      // Benchmark("puzzle", "test/gambit/puzzle.scm", abs.inject(true)), // missing primitives
      // Benchmark("sboyer", "test/gambit/sboyer.scm", abs.inject(true)), // dot notation
      // Benchmark("scheme", "test/gambit/scheme.scm", abs.inject(true)), // dot notation
      // Benchmark("slatex", "test/gambit/slatex.scm", ???), // rely on call-with-output-file/truncate (non-R5RS?)
      // Benchmark("string", "test/gambit/string.scm", abs.inject(true)), // missing primitives
      // Benchmark("sum", "test/gambit/sum.scm", abs.inject(true)), // named let
      // Benchmark("sumloop", "test/gambit/sumloop.scm", abs.inject(true)), // missing primitives
      // Benchmark("tail", "test/gambit/tail.scm", ???), // rely on file io
      Benchmark("tak", "test/gambit/tak.scm", abs.inject(true)),
      // Benchmark("trav1", "test/gambit/trav1.scm", abs.inject(true)), // fail to parse
      // Benchmark("triangl", "test/gambit/triangl.scm", abs.inject(true)), // missing primitives
      // Benchmark("wc", "test/gambit/wc.scm", ???), // rely on file io

      /* Benchmarks originating from Rosetta code */
      // Benchmark("easter", "test/rosetta/easter.scm", abs.inject(true)), // TODO: use remainder
      Benchmark("quadratic", "test/rosetta/quadratic.scm", abs.inject(true)),

      /* Benchmarks from https://github.com/uim/sigscheme */
      Benchmark("arithint", "test/sigscheme/arithint.scm", abs.inject(20001)),
      // Benchmark("case", "test/sigscheme/case.scm", abs.inject(20000)), // TODO: case doesn't have full precision in concrete
      Benchmark("let-loop", "test/sigscheme/let-loop.scm", abs.inject(20000)),
      Benchmark("loop", "test/sigscheme/loop.scm", abs.inject(8000)),
      Benchmark("mem", "test/sigscheme/mem.scm", abs.inject(false)), // TODO: #f in guile, #t in racket
      Benchmark("rec", "test/sigscheme/rec.scm", abs.inject(true)),
      Benchmark("takr", "test/sigscheme/takr.scm", abs.inject(7)),

      /* Benchmarks from http://soft.vub.ac.be/SCPI/ */
      Benchmark("2.1", "test/scp1/2.1.scm", abs.inject(true)),
      // Benchmark("2.4", "test/scp1/2.4.scm", abs.inject(true)), // TODO: fail
      // Benchmark("3.1", "test/scp1/3.1.scm", abs.inject(true)), // TODO: fail
      Benchmark("3.2.1", "test/scp1/3.2.1.scm", abs.inject(true)),
      // Benchmark("3.2", "test/scp1/3.2.scm", abs.inject(true)), // TODO: fail
      // Benchmark("3.3", "test/scp1/3.3.scm", abs.inject(true)), // TODO: fail
      Benchmark("3.4", "test/scp1/3.4.scm", abs.inject(true)),
      // Benchmark("3.5", "test/scp1/3.5.scm", abs.inject(true)), // TODO: fail
      // Benchmark("3.6", "test/scp1/3.6.scm", abs.inject(true)), // TODO: fail
      // Benchmark("3.7", "test/scp1/3.7.scm", abs.inject(true)), // TODO: fail
      Benchmark("3.8", "test/scp1/3.8.scm", abs.inject(true)),
      Benchmark("3.9", "test/scp1/3.9.scm", abs.inject(true)),
      Benchmark("4.1", "test/scp1/4.1.scm", abs.inject(true)),
      // Benchmark("4.8", "test/scp1/4.8.scm", abs.inject(true)), // TODO: fail
      // Benchmark("5.14.3", "test/scp1/5.14.3.scm", abs.inject(true)), // TODO: fail
      Benchmark("5.20.4", "test/scp1/5.20.4.scm", abs.inject(true)),
      // Benchmark("5.21", "test/scp1/5.21.scm", abs.inject(true)), // TODO: fail
      // Benchmark("5.22", "test/scp1/5.22.scm", abs.inject(true)), // TODO: fail
      Benchmark("5.6", "test/scp1/5.6.scm", abs.inject(true)),
      // Benchmark("5.7", "test/scp1/5.7.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.11", "test/scp1/7.11.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.12", "test/scp1/7.12.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.13", "test/scp1/7.13.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.14", "test/scp1/7.14.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.15", "test/scp1/7.15.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.16", "test/scp1/7.16.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.17", "test/scp1/7.17.scm", abs.inject(true)), // TODO: fail
      Benchmark("7.2", "test/scp1/7.2.scm", abs.inject(true)),
      // Benchmark("7.3", "test/scp1/7.3.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.4", "test/scp1/7.4.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.5", "test/scp1/7.5.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.7", "test/scp1/7.7.scm", abs.inject(true)), // TODO: fail
      // Benchmark("7.9", "test/scp1/7.9.scm", abs.inject(true)), // TODO: fail
      Benchmark("8.1.1", "test/scp1/8.1.1.scm", abs.inject(true)),
      Benchmark("8.1.3", "test/scp1/8.1.3.scm", abs.inject(true)),
      Benchmark("8.10", "test/scp1/8.10.scm", abs.inject(true)),
      // Benchmark("8.11", "test/scp1/8.11.scm", abs.inject(true)), // TODO: fail
      Benchmark("8.12", "test/scp1/8.12.scm", abs.inject(true)),
      Benchmark("8.13", "test/scp1/8.13.scm", abs.inject(true)),
      // Benchmark("8.15", "test/scp1/8.15.scm", abs.inject(true)), // TODO: fail
      // Benchmark("8.16", "test/scp1/8.16.scm", abs.inject(true)), // TODO: fail
      // Benchmark("8.5", "test/scp1/8.5.scm", abs.inject(true)), // TODO: fail
      Benchmark("8.6", "test/scp1/8.6.scm", abs.inject(true)),
      Benchmark("9.12", "test/scp1/9.12.scm", abs.inject(true)),
      // Benchmark("9.13", "test/scp1/9.13.scm", abs.inject(true)), // TODO: fail
      Benchmark("9.15", "test/scp1/9.15.scm", abs.inject(true)),
      Benchmark("9.16", "test/scp1/9.16.scm", abs.inject(true)),
      Benchmark("9.17", "test/scp1/9.17.scm", abs.inject(true)),
      // Benchmark("9.18", "test/scp1/9.18.scm", abs.inject(true)), // TODO: fail
      Benchmark("9.2", "test/scp1/9.2.scm", abs.inject(true)),
      Benchmark("9.3", "test/scp1/9.3.scm", abs.inject(true)),
      Benchmark("9.5", "test/scp1/9.5.scm", abs.inject(true)),
      // Benchmark("9.6", "test/scp1/9.6.scm", abs.inject(true)), // TODO: fail
      Benchmark("9.7", "test/scp1/9.7.scm", abs.inject(true)),
      Benchmark("9.8", "test/scp1/9.8.scm", abs.inject(true)),
      // Benchmark("9.9", "test/scp1/9.9.scm", abs.inject(true)), // TODO: fail

      /* Benchmarks from ftp://cw.vub.ac.be/pub/courses/curriculum/AlgoDat1/programmacode/ */
      Benchmark("abstrct", "test/ad/abstrct.scm", abs.inject(true)),
      // Benchmark("bfirst", "test/ad/bfirst.scm", abs.inject(true)), // dot notation
      // Benchmark("bst", "test/ad/bst.scm", abs.inject(true)), // dot notation
      // Benchmark("bubsort", "test/ad/bubsort.scm", abs.inject(true)), // fail to parse
      // Benchmark("dict", "test/ad/dict.scm", abs.inject(true)), // dot notation
      // Benchmark("graf", "test/ad/graf.scm", abs.inject(true)), // TODO: no file
      // Benchmark("heap", "test/ad/heap.scm", abs.inject(true)), // fail to parse
      Benchmark("inssort", "test/ad/inssort.scm", abs.inject(true)),
      // Benchmark("linear", "test/ad/linear.scm", abs.inject(true)), // dot notation
      // Benchmark("list", "test/ad/list.scm", abs.inject(true)), // dot notation
      // Benchmark("mesort", "test/ad/mesort.scm", abs.inject(true)), // fail to parse
      Benchmark("prioq", "test/ad/prioq.scm", abs.injectSymbol("Patrick")),
      // Benchmark("qsort", "test/ad/qsort.scm", abs.inject(true)), // fail to parse
      Benchmark("qstand", "test/ad/qstand.scm", abs.inject(true)),
      // Benchmark("queue", "test/ad/queue.scm", abs.inject(true)), // dot notation
      // Benchmark("quick", "test/ad/quick.scm", abs.inject(true)), // fail to parse
      // Benchmark("RBtreeADT", "test/ad/RBtreeADT.scm", abs.inject(true)), // dot notation
      // Benchmark("selsort", "test/ad/selsort.scm", abs.inject(true)), // fail to parse
      Benchmark("stack", "test/ad/stack.scm", abs.inject(true)),
      // Benchmark("stspaceCODE", "test/ad/stspaceCODE.scm", abs.inject(true)), // dot notation

      /* Other benchmarks, handwritten or common */
      Benchmark("fact", "test/fact.scm", abs.inject(120)),
      Benchmark("fib", "test/fib.scm", abs.inject(3)),
      Benchmark("bound-precision", "test/bound-precision.scm", abs.inject(true)),
      Benchmark("church", "test/church.scm", abs.inject(true)),
      // TODO: church-0, church-1 and church-2 should be removed
      Benchmark("church-2", "test/church-2-num.scm", abs.inject(2)),
      Benchmark("church-6", "test/church-6.scm", abs.inject(6)),
      Benchmark("count", "test/count.scm", abs.inject("done")),
      Benchmark("inc", "test/inc.scm", abs.inject(4)),
      Benchmark("infinite-1", "test/infinite-1.scm", abs.bottom),
      Benchmark("infinite-2", "test/infinite-2.scm", abs.bottom),
      Benchmark("infinite-3", "test/infinite-3.scm", abs.bottom),
      Benchmark("letrec-begin", "test/letrec-begin.scm", abs.inject(1)), // TODO: not R5RS due to letrec being used as letrec*
      Benchmark("mceval", "test/mceval.scm", abs.inject(40320)),
      Benchmark("mut-rec", "test/mut-rec.scm", abs.inject(true)),
      Benchmark("sq", "test/sq.scm", abs.inject(9)),
      Benchmark("sym", "test/sym.scm", abs.injectSymbol("foo")),
      Benchmark("widen", "test/widen.scm", abs.inject(10)),
      // Benchmark("looping", "test/looping.scm", abs.inject(true)), // TODO: require named let, do notation
      Benchmark("work", "test/work.scm", abs.inject(362880)),

      /* Benchmarks from unknown sources, used in papers such as Introspective Pushdown Analysis of Higher-Order Programs, Earl et al. (2012) */
      Benchmark("blur", "test/blur.scm", abs.inject(true)),
      Benchmark("collatz", "test/collatz.scm", abs.inject(5)),
      Benchmark("eta", "test/eta.scm", abs.inject(false)),
      Benchmark("gcipd", "test/gcipd.scm", abs.inject(36)),
      Benchmark("grid", "test/grid.scm", abs.inject(true)),
      Benchmark("kcfa2", "test/kcfa2.scm", abs.inject(false)),
      Benchmark("kcfa3", "test/kcfa3.scm", abs.inject(false)),
      Benchmark("loop2", "test/loop2.scm", abs.inject(550)),
      Benchmark("mj09", "test/mj09.scm", abs.inject(2)),
      Benchmark("primtest", "test/primtest.scm", abs.inject(1)),
      Benchmark("regex", "test/regex.scm", abs.inject(false)),
      Benchmark("rsa", "test/rsa.scm", abs.inject(true)),
      Benchmark("sat", "test/sat.scm", abs.inject(true)),
      // Benchmark("scm2c", "test/scm2c", abs.inject("TODO")),
      Benchmark("scm2java", "test/scm2java.scm", abs.inject("public class BOut extends RuntimeEnvironment {\\n public static void main (String[] args) {\\nnew IntValue(3) ;\\n }\\n}\\n"))
    )
  }

    // TODO: import from jevdplas:
    // test/
    //   quasiquoting
    //   quasiquoting-simple
    //   SICP-compiler
    //   Streams
}

abstract class Benchmarks[Exp : Expression, Addr : Address, Time : Timestamp](val lattice: SchemeLattice)
    extends FlatSpec with Matchers with BenchmarkFiles {
  type Abs = lattice.L
  implicit val abs = lattice.isSchemeLattice
  val sem: Semantics[Exp, lattice.L, Addr, Time]
  val machine: AbstractMachine[Exp, lattice.L, Addr, Time]

  def checkResult(file: String, expected: Abs): Unit = {
    val result = machine.eval(sem.parse(Util.fileContent(s"$file").get), sem, false, Timeout.start(timeout))
    if (result.timedOut) {
      println(s"${machine.name}, $file: TIMEOUT, ${result.numberOfStates}, ${result.time}")
      cancel(s"Benchmark $file timed out with ${machine.name}")
    } else {
      if (expected != abs.bottom) { assert(result.containsFinalValue(expected)) }
      println(s"${machine.name}, $file: ${result.numberOfStates}, ${result.time}")
    }
  }
  def check(file: String, expected: Abs): Unit =
    file should s"eval to $expected" in { checkResult(file, expected) }

  val concrete = abs.name.contains("Concrete")

  benchmarks(lattice).foreach(bench =>
    if (bench.runnable(concrete)) {
      check(bench.file, bench.expected)
    })
}

abstract class OneResultTests[Exp : Expression, Addr : Address, Time : Timestamp](val lattice: SchemeLattice)
    extends FlatSpec with Matchers with BenchmarkFiles {
  type Abs = lattice.L
  implicit val abs = lattice.isSchemeLattice
  val sem: Semantics[Exp, lattice.L, Addr, Time]
  val machine: AbstractMachine[Exp, lattice.L, Addr, Time]

  def check(file: String, expected: Abs): Unit =
    file should s"have only one final state in concrete mode and return $expected" in {
      val program = Util.fileContent(s"$file")
      program.isDefined should equal (true)
      val result = machine.eval(sem.parse(program.get), sem, false, Timeout.start(timeout))
      if (result.timedOut) {
        cancel(s"Benchmark $file timed out with ${machine.name}")
      } else {
        result.finalValues.size should equal (1)
        result.finalValues.head should equal (expected)
      }
    }


  benchmarks(lattice).foreach(bench =>
    if (bench.runnable(true)) {
      check(bench.file, bench.expected)
    })
}

abstract class AAMBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAM[SchemeExp, lattice.L, Addr, Time]
}

abstract class AAMGlobalStoreBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, Addr, Time](AAMKAlloc)
}

abstract class AACBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, Addr, Time](AACKAlloc)
}

abstract class FreeBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, Addr, Time](P4FKAlloc)
}

abstract class ConcreteMachineBenchmarks(override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](lattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new ConcreteMachine[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}

class AACConcreteBenchmarks extends AACBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice)
class AACTypeBenchmarks extends AACBenchmarks[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)

class AAMConcreteBenchmarks extends AAMBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice)
class AAMTypeBenchmarks extends AAMBenchmarks[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)

class AAMGlobalStoreConcreteBenchmarks extends AAMGlobalStoreBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice)
class AAMGlobalStoreTypeBenchmarks extends AAMGlobalStoreBenchmarks[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)

class FreeConcreteBenchmarks extends FreeBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice)
class FreeTypeBenchmarks extends FreeBenchmarks[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)

class ConcreteMachineConcreteBenchmarks extends ConcreteMachineBenchmarks(Lattices.ConcreteLattice)

class AACOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](AACKAlloc)
}

class AAMOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new AAM[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}

class AAMGlobalStoreOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](AAMKAlloc)
}

class FreeOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](P4FKAlloc)
}

class ConcreteMachineOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new ConcreteMachine[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}
