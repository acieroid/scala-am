import org.scalatest._
import org.scalatest.prop._

case class Benchmark[Abs : JoinLattice](name: String, file: String, expected: Abs, slow: Boolean = false, dontterminate: Boolean = false)
trait BenchmarkFiles {
  def benchmarks(lattice: SchemeLattice): List[Benchmark[lattice.L]] = {
    implicit val abs = lattice.isSchemeLattice
    List(
      /* Gabriel benchmarks, see http://www.larcenists.org/Twobit/benchmarksAbout.html */
      // Benchmark("boyer", "test/boyer.scm", abs.inject(true), slow = true), // takes too long to finish
      // TODO: browse (do notation)
      Benchmark("cpstak", "test/gabriel/cpstak.scm", abs.inject(6), slow=true),
      // TODO: ctak (call/cc)
      Benchmark("dderiv", "test/gabriel/dderiv.scm", abs.inject(true), slow=true),
      Benchmark("deriv", "test/gabriel/deriv.scm", abs.inject(true), slow=true),
      // TODO: destruc (do notation)
      // TODO: diviter (do notation)
      Benchmark("divrec", "test/gabriel/divrec.scm", abs.inject(true)),
      // TODO: puzzle (do notation)
      Benchmark("takl", "test/gabriel/takl.scm", abs.inject(true), slow=true),
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

      /* Gambit benchmarks, see http://www.larcenists.org/Twobit/benchmarksAbout.html */
      Benchmark("nqueens", "test/gambit/nqueens.scm", abs.inject(92), slow=true),
      // TODO: Benchmark("conform", "test/conform.scm", ???),
      // TODO: Benchmark("conform2", "test/conform2.scm", ???),
      // TODO: most of the benchmarks need to be investigated

      /* Benchmarks originating from Rosetta code */
      // Benchmark("easter", "test/rosetta/easter.scm", abs.inject(true)), // TODO: use remainder
      Benchmark("quadratic", "test/rosetta/quadratic.scm", abs.inject(true)),

      /* Benchmarks from https://github.com/uim/sigscheme */
      Benchmark("arithint", "test/sigscheme/arithint.scm", abs.inject(20001)),
      // Benchmark("case", "test/sigscheme/case.scm", abs.inject(20000)), // TODO: case doesn't have full precision in concrete
      Benchmark("let-loop", "test/sigscheme/let-loop.scm", abs.inject(20000)),
      Benchmark("loop", "test/sigscheme/loop.scm", abs.inject(8000)),
      Benchmark("mem", "test/sigscheme/mem.scm", abs.inject(false), slow=true), // TODO: #f in guile, #t in racket
      Benchmark("rec", "test/sigscheme/rec.scm", abs.inject(true)),
      Benchmark("takr", "test/sigscheme/takr.scm", abs.inject(7), slow=true),

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

      /* Other benchmarks, handwritten or common */
      Benchmark("fact", "test/fact.scm", abs.inject(120)),
      Benchmark("fib", "test/fib.scm", abs.inject(3)),
      Benchmark("bound-precision", "test/bound-precision.scm", abs.inject(true)),
      Benchmark("church", "test/church.scm", abs.inject(true), slow=true),
      // TODO: church-0, church-1 and church-2 should be removed
      Benchmark("church-2", "test/church-2-num.scm", abs.inject(2)),
      Benchmark("church-6", "test/church-6.scm", abs.inject(6)),
      Benchmark("count", "test/count.scm", abs.inject("done")),
      Benchmark("inc", "test/inc.scm", abs.inject(4)),
      Benchmark("infinite-1", "test/infinite-1.scm", abs.bottom, dontterminate=true),
      Benchmark("infinite-2", "test/infinite-2.scm", abs.bottom, dontterminate=true),
      Benchmark("infinite-3", "test/infinite-3.scm", abs.bottom, dontterminate=true),
      Benchmark("letrec-begin", "test/letrec-begin.scm", abs.inject(1)), // TODO: not R5RS due to letrec being used as letrec*
      Benchmark("mceval", "test/mceval.scm", abs.inject(40320), slow=true),
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
      Benchmark("grid", "test/grid.scm", abs.inject(true), slow=true),
      Benchmark("kcfa2", "test/kcfa2.scm", abs.inject(false)),
      Benchmark("kcfa3", "test/kcfa3.scm", abs.inject(false)),
      Benchmark("loop2", "test/loop2.scm", abs.inject(550)),
      Benchmark("mj09", "test/mj09.scm", abs.inject(2)),
      Benchmark("primtest", "test/primtest.scm", abs.inject(1)),
      Benchmark("regex", "test/regex.scm", abs.inject(false), slow=true),
      Benchmark("rsa", "test/rsa.scm", abs.inject(true)),
      Benchmark("sat", "test/sat.scm", abs.inject(true)),
      // Benchmark("scm2c", "test/scm2c", abs.inject("TODO")),
      Benchmark("scm2java", "test/scm2java.scm", abs.inject("public class BOut extends RuntimeEnvironment {\n public static void main (String[] args) {\\nnew IntValue(3) ;\\n }\\n}\\n"), slow=true)
    )
  }

    // TODO: import from jevdplas:
    // test/
    //   quasiquoting
    //   quasiquoting-simple
    //   SICP-compiler
    //   Streams
    // AD/: whole directory
    // Gambit_bench: whole directory
    // SCP1: whole directory
}

abstract class Benchmarks[Exp : Expression, Addr : Address, Time : Timestamp](val lattice: SchemeLattice)
    extends FlatSpec with Matchers with BenchmarkFiles {
  type Abs = lattice.L
  implicit val abs = lattice.isSchemeLattice
  val sem: Semantics[Exp, lattice.L, Addr, Time]
  val machine: AbstractMachine[Exp, lattice.L, Addr, Time]

  def checkResult(file: String, expected: Abs): Unit = {
    val result = machine.eval(sem.parse(Util.fileContent(s"$file").get), sem, false, Timeout.none)
    assert(result.containsFinalValue(expected))
    println(s"${machine.name}, $file: ${result.numberOfStates}, ${result.time}")
  }
  def check(file: String, expected: Abs): Unit =
    file should s"eval to $expected" in { checkResult(file, expected) }

  val concrete = abs.name.contains("Concrete")

  benchmarks(lattice).foreach(bench =>
    if (!bench.slow && !(concrete && bench.dontterminate)) {
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
      val result = machine.eval(sem.parse(program.get), sem, false, Timeout.none)
      result.finalValues.size should equal (1)
      result.finalValues.head should equal (expected)
    }


  benchmarks(lattice).foreach(bench =>
    if (!bench.slow && !bench.dontterminate) {
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
