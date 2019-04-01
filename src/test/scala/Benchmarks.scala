import org.scalatest._

/** The kind of tests that a benchmark can be used for */
object BenchmarkTestKind extends Enumeration {
  type BenchmarkTestKind = Value
  val SExpParse, SchemeParse = Value
  val SchemeRunConcrete, SchemeRunAbstract = Value

  def none: Set[BenchmarkTestKind] = Set.empty
  def all: Set[BenchmarkTestKind] = Set(SExpParse, SchemeParse, SchemeRunConcrete, SchemeRunAbstract)
  def parse: Set[BenchmarkTestKind] = Set(SExpParse, SchemeParse)
  def run: Set[BenchmarkTestKind] = Set(SchemeRunConcrete, SchemeRunAbstract)
  def except(kinds: Set[BenchmarkTestKind]): Set[BenchmarkTestKind] =
    all -- kinds
}


/** A benchmark */
case class Benchmark(
  /** The path to the file */
  file: String,
  /** The expected result for this benchmark as a primitive value */
  result: scalaam.language.sexp.Value,
  /** Supported tests */
  supported: Set[BenchmarkTestKind.BenchmarkTestKind]) {
  override def toString = file
}

object BenchmarksUtil {
  def fileContent(bench: Benchmark): Option[String] = {
    val f = scala.io.Source.fromFile(bench.file)(scala.io.Codec("UTF-8"))
    val content = f.getLines.mkString("\n")
    f.close()
    Option(content)
  }
}

object Benchmarks {
  import BenchmarkTestKind._
  import scalaam.language.sexp._
  def benchmarksFor(kind: BenchmarkTestKind): List[Benchmark] =
    allBenchmarks.filter(b => b.supported.contains(kind))
  def allBenchmarks = List(
    Benchmark("test/ad/abstrct.scm", ValueBoolean(true), all),
    Benchmark("test/ad/bfirst.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/ad/bst.scm", ValueBoolean(true), none), // dot notation
    // TODO: ad/btree.scm only contains definition, add a body
    Benchmark("test/ad/bubsort.scm", ValueBoolean(true), none), // fails to parse
    Benchmark("test/ad/dict.scm", ValueBoolean(true), all),
    Benchmark("test/ad/heap.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/ad/inssort.scm", ValueBoolean(true), all),
    Benchmark("test/ad/linear.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/ad/list.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/ad/mesort.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/ad/prioq.scm", ValueBoolean(true), all),
    Benchmark("test/ad/qsort.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/ad/qstand.scm", ValueBoolean(true), all),
    Benchmark("test/ad/queue.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/ad/quick.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/ad/RBtreeADT.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/ad/selsort.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/ad/stack.scm", ValueBoolean(true), all),
    Benchmark("test/ad/stspaceCODE.scm", ValueBoolean(true), none), // dot notation

    Benchmark("test/blur.scm", ValueBoolean(true), all),
    Benchmark("test/bound-precision.scm", ValueBoolean(true), all),
    Benchmark("test/church-2-num.scm", ValueInteger(2), all),
    Benchmark("test/church-6.scm", ValueInteger(6), all),
    Benchmark("test/church.scm", ValueBoolean(true), all),
    Benchmark("test/collatz.scm", ValueInteger(5), all),
    Benchmark("test/count.scm", ValueString("done"), all),
    Benchmark("test/eta.scm", ValueBoolean(false), all),
    Benchmark("test/fact.scm", ValueInteger(120), all),
    Benchmark("test/fib.scm", ValueInteger(3), all),

Benchmark("test/gabriel/boyer.scm", ValueBoolean(true), all),
    Benchmark("test/gabriel/cpstak.scm", ValueInteger(6), all),
    Benchmark("test/gabriel/dderiv.scm", ValueBoolean(true), all),
    Benchmark("test/gabriel/deriv.scm", ValueBoolean(true), all),
    Benchmark("test/gabriel/divrec.scm", ValueBoolean(true), all),
    Benchmark("test/gabriel/takl.scm", ValueBoolean(true), all),

    Benchmark("test/gambit/array1.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/browse.scm", ValueInteger(1101), parse /* missing string-ref */),
    Benchmark("test/gambit/cat.scm", ValueBoolean(true), parse /* rely on IO */),
    Benchmark("test/gambit/compiler.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/gambit/ctak.scm", ValueBoolean(true), parse /* call/cc */),
    Benchmark("test/gambit/deriv.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/destruc.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/diviter.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/earley.scm", ValueBoolean(true), parse /* list->vector */),
    Benchmark("test/gambit/fibc.scm", ValueBoolean(true), parse /* call/cc */),
    Benchmark("test/gambit/graphs.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/lattice.scm", ValueBoolean(true), parse /* apply */),
    Benchmark("test/gambit/matrix.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/mazefun.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/nboyer.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/gambit/nqueens.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/paraffins.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/perm9.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/peval.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/primes.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/puzzle.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/sboyer.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/gambit/scheme.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/gambit/slatex.scm", ValueBoolean(true), none), // dot notation
    Benchmark("test/gambit/string.scm", ValueBoolean(true), parse /* missing substring */),
    Benchmark("test/gambit/sum.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/sumloop.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/tail.scm", ValueBoolean(true), parse /* rely on IO */),
    Benchmark("test/gambit/tak.scm", ValueBoolean(true), all),
    Benchmark("test/gambit/trav1.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/gambit/triangl.scm", ValueBoolean(true), parse /* list->vector */),
    Benchmark("test/gambit/wc.scm", ValueBoolean(true), parse /* rely on IO */),

    Benchmark("test/gcipd.scm", ValueInteger(36), all),
    Benchmark("test/grid.scm", ValueBoolean(true), all),
    Benchmark("test/inc.scm", ValueInteger(4), all),
/* TODO[easy] these should return bottom
    Benchmark("test/infinite-1.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
    Benchmark("test/infinite-2.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
    Benchmark("test/infinite-3.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
 */
    Benchmark("test/kcfa2.scm", ValueBoolean(false), all),
    Benchmark("test/kcfa3.scm", ValueBoolean(false), all),
    Benchmark("test/kernighanvanwyk/ack.scm", ValueInteger(4), all),
    Benchmark("test/letrec-begin.scm", ValueInteger(1), all),
    Benchmark("test/loop2.scm", ValueInteger(550), all),
    Benchmark("test/mceval.scm", ValueInteger(40320), all),
    Benchmark("test/mj09.scm", ValueInteger(2), all),
    Benchmark("test/mut-rec.scm", ValueBoolean(true), all),
    Benchmark("test/nested-defines.scm", ValueBoolean(true), all),
    Benchmark("test/primtest.scm", ValueInteger(1), all),
    Benchmark("test/quasiquoting-simple.scm", ValueBoolean(true), none), // quasiquoting
    Benchmark("test/quasiquoting.scm", ValueBoolean(true), none), // quasiquoting
    Benchmark("test/regex.scm", ValueBoolean(false), all),
    Benchmark("test/rosetta/easter.scm", ValueBoolean(true), all),
    Benchmark("test/rosetta/quadratic.scm", ValueBoolean(true), all),
    Benchmark("test/rotate.scm", ValueString("hallo"), all),
    Benchmark("test/rsa.scm", ValueBoolean(true), all),
    Benchmark("test/sat.scm", ValueBoolean(true), all),
    Benchmark("test/scm2c.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/scm2java.scm", ValueString("public class BOut extends RuntimeEnvironment {\\n public static void main (String[] args) {\\nnew IntValue(3) ;\\n }\\n}\\n"), all),
    Benchmark("test/scp1/2.1.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/2.4.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/3.1.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/3.2.1.scm", ValueBoolean(true), all),
    Benchmark("test/scp1/3.2.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/3.3.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/3.4.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/3.6.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/3.8.scm",   ValueBoolean(true), parse /* incorrect output?! */),
    Benchmark("test/scp1/3.9.scm",   ValueBoolean(true), parse /* takes too long? */),
    Benchmark("test/scp1/4.1.scm",   ValueBoolean(true), parse /* takes too long? */),
    Benchmark("test/scp1/4.8.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/5.14.3.scm",ValueBoolean(true), all),
    Benchmark("test/scp1/5.19.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/5.20.4.scm",ValueBoolean(true), all),
    Benchmark("test/scp1/5.21.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/5.22.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/5.6.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/5.7.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/7.11.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/7.12.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/7.13.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/7.14.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/7.15.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/7.16.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/7.17.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/7.18.scm",  ValueBoolean(true), parse /* unknown */),
    Benchmark("test/scp1/7.2.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/7.3.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/7.4.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/7.5.scm",   ValueBoolean(true), none), // dot notation
    Benchmark("test/scp1/7.6.scm",   ValueBoolean(true), none), // dot notation
    Benchmark("test/scp1/7.9.scm",   ValueBoolean(true), none), // dot notation
    Benchmark("test/scp1/8.1.1.scm", ValueBoolean(true), all),
    Benchmark("test/scp1/8.1.3.scm", ValueBoolean(true), all),
    Benchmark("test/scp1/8.10.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/8.11.scm",  ValueBoolean(true), none), // dot notation
    Benchmark("test/scp1/8.12.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/8.13.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/8.14.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/8.15.scm",  ValueBoolean(true), none), // dot notation
    Benchmark("test/scp1/8.16.scm",  ValueBoolean(true), none), // dot notation
    Benchmark("test/scp1/8.5.scm",   ValueBoolean(true), none), // dot notation
    Benchmark("test/scp1/8.6.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/9.12.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/9.13.scm",  ValueBoolean(true), parse /* unknown */),
    Benchmark("test/scp1/9.14.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/9.15.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/9.16.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/9.17.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/9.18.scm",  ValueBoolean(true), all),
    Benchmark("test/scp1/9.2.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/9.3.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/9.5.scm",   ValueBoolean(true), parse /* unknown */),
    Benchmark("test/scp1/9.6.scm",   ValueBoolean(true), all),
    Benchmark("test/scp1/9.7.scm",   ValueBoolean(true), parse /* unknown */),
    Benchmark("test/scp1/9.8.scm",   ValueBoolean(true), parse /* unknown */),
    Benchmark("test/scp1/9.9.scm",   ValueBoolean(true), all),

    Benchmark("test/SICP-compiler.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/sigscheme/arithint.scm", ValueInteger(20001), all),
    Benchmark("test/sigscheme/case.scm", ValueInteger(20000), all),
    Benchmark("test/sigscheme/let-loop.scm", ValueInteger(20000), all),
    Benchmark("test/sigscheme/loop.scm", ValueInteger(8000), all),
    Benchmark("test/sigscheme/mem.scm", ValueBoolean(false), all),
    Benchmark("test/sigscheme/rec.scm", ValueBoolean(true), all),
    Benchmark("test/sigscheme/takr.scm", ValueInteger(7), all),
    Benchmark("test/sq.scm", ValueInteger(9), all),
    Benchmark("test/Streams.scm", ValueBoolean(true), none), // unknown reason
    Benchmark("test/sym.scm", ValueSymbol("foo"), all),
    Benchmark("test/widen.scm", ValueInteger(10), all),
    Benchmark("test/work.scm", ValueInteger(362880), all)
   )

  val unused: List[Benchmark] = allBenchmarks.filter(b => b.supported.isEmpty)
}

class UnusedBenchmarksTests extends FlatSpec with Matchers {
  Benchmarks.unused.foreach(bench => bench.file should "be used" in { cancel("unused") })
}
