import org.scalatest._

/** The kind of tests that a benchmark can be used for */
object BenchmarkTestKind extends Enumeration {
  type BenchmarkTestKind = Value
  val SExpParse, SchemeParse = Value
  val SchemeRunConcrete, SchemeRunAbstract = Value
}

/** A benchmark */
case class Benchmark(
  /* The path to the file */
  file: String,
  /* Supported tests */
  supported: List[BenchmarkTestKind.BenchmarkTestKind]) {
  override def toString = file
}

object BenchmarksUtil {
  def fileContent(bench: Benchmark): Option[String] = {
    val f = scala.io.Source.fromFile(bench.file)
    val content = f.getLines.mkString("\n")
    f.close()
    Option(content)
  }
}

object Benchmarks {
  import BenchmarkTestKind._
  def benchmarksFor(kind: BenchmarkTestKind): List[Benchmark] =
    all.filter(b => b.supported.contains(kind))
  val all = List(
    Benchmark("test/ad/abstrct.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/ad/bfirst.scm", List()), // dot notation
    Benchmark("test/ad/bst.scm", List()), // dot notation
    Benchmark("test/ad/btree.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/ad/bubsort.scm", List()), // unknown reason
    Benchmark("test/ad/dict.scm", List()), // unknown reason
    Benchmark("test/ad/dictExamples.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/ad/heap.scm", List()), // unknown reason
    Benchmark("test/ad/inssort.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/ad/linear.scm", List()), // dot notation
    Benchmark("test/ad/list.scm", List()), // dot notation
    Benchmark("test/ad/mesort.scm", List()), // unknown reason
    Benchmark("test/ad/prioq.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/ad/qsort.scm", List()), // unknown reason
    Benchmark("test/ad/qstand.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/ad/queue.scm", List()), // dot notation
    Benchmark("test/ad/quick.scm", List()), // unknown reason
    Benchmark("test/ad/RBtreeADT.scm", List()), // dot notation
    Benchmark("test/ad/selsort.scm", List()), // unknown reason
    Benchmark("test/ad/stack.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/ad/stspaceCODE.scm", List()), // dot notation
    Benchmark("test/blur.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/bound-precision.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/church-2-num.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/church-6.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/church.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/collatz.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/count.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/eta.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/fact.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/fib.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gabriel/boyer.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gabriel/cpstak.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gabriel/dderiv.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gabriel/deriv.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gabriel/divrec.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gabriel/takl.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/array1.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/browse.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/cat.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/compiler.scm", List()), // unknown reason
    Benchmark("test/gambit/ctak.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/deriv.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/destruc.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/diviter.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/earley.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/fibc.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/graphs.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/lattice.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/matrix.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/mazefun.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/nboyer.scm", List()), // dot notation
    Benchmark("test/gambit/nqueens.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/paraffins.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/perm9.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/peval.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/primes.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/puzzle.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/sboyer.scm", List()), // dot notation
    Benchmark("test/gambit/scheme.scm", List()), // dot notation
    Benchmark("test/gambit/slatex.scm", List()), // dot notation
    Benchmark("test/gambit/string.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/sum.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/sumloop.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/tail.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/tak.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/trav1.scm", List()), // unknown reason
    Benchmark("test/gambit/triangl.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gambit/wc.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/gcipd.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/grid.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/inc.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/infinite-1.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/infinite-2.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/infinite-3.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/kcfa2.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/kcfa3.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/kernighanvanwyk/ack.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/letrec-begin.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/loop2.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/mceval.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/mj09.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/mut-rec.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/nested-defines.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/primtest.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/quasiquoting-simple.scm", List()), // quasiquoting
    Benchmark("test/quasiquoting.scm", List()), // quasiquoting
    Benchmark("test/regex.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/rosetta/easter.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/rosetta/quadratic.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/rotate.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/rsa.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sat.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scm2c.scm", List()), // unknown reason
    Benchmark("test/scm2java.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/2.1.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/2.4.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.1.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.2.1.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.2.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.3.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.4.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.6.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.8.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/3.9.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/4.1.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/4.8.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/5.14.3.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/5.19.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/5.20.4.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/5.21.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/5.22.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/5.6.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/5.7.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.11.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.12.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.13.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.14.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.15.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.16.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.17.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.18.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.2.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.3.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.4.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/7.5.scm", List()), // dot notation
    Benchmark("test/scp1/7.6.scm", List()), // dot notation
    Benchmark("test/scp1/7.9.scm", List()), // dot notation
    Benchmark("test/scp1/8.1.1.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/8.1.3.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/8.10.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/8.11.scm", List()), // dot notation
    Benchmark("test/scp1/8.12.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/8.13.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/8.14.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/8.15.scm", List()), // dot notation
    Benchmark("test/scp1/8.16.scm", List()), // dot notation
    Benchmark("test/scp1/8.5.scm", List()), // dot notation
    Benchmark("test/scp1/8.6.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.12.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.13.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.14.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.15.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.16.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.17.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.18.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.2.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.3.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.5.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.6.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.7.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.8.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/scp1/9.9.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/SICP-compiler.scm", List()), // unknown reason
    Benchmark("test/sigscheme/arithint.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sigscheme/case.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sigscheme/let-loop.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sigscheme/loop.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sigscheme/mem.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sigscheme/rec.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sigscheme/takr.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/sq.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/Streams.scm", List()), // unknown reason
    Benchmark("test/sym.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/widen.scm", List(SExpParse, SchemeParse)),
    Benchmark("test/work.scm", List(SExpParse, SchemeParse)))

  val unused: List[Benchmark] = all.filter(b => b.supported.isEmpty)
}

class UnusedBenchmarksTests extends FlatSpec with Matchers {
  Benchmarks.unused.foreach(bench => bench.file should "be used" in { cancel("unused") })
}
