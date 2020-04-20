package scalaam.test

import org.scalatest.propspec.AnyPropSpec

trait SchemeBenchmarkTests extends AnyPropSpec {
  // a benchmark is just a file name
  type Benchmark = String
  // the benchmarks involved in the tests
  def benchmarks: Set[Benchmark] = Set.empty

  // needs to be implemented to specify the testing behaviour per benchmark
  protected def onBenchmark(b: Benchmark): Unit
  // run the benchmarks
  benchmarks.foreach(onBenchmark)
}

trait SimpleBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks = SchemeBenchmarks.other
}

trait AllBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks = SchemeBenchmarks.allBenchmarks
}

object SchemeBenchmarks {

  val ad: Set[String] = Set(
    "test/ad/abstrct.scm",
    "test/ad/bfirst.scm", // VARARG
    "test/ad/bst.scm", // VARARG
    //"test/ad/btree.scm", // TODO add a body
    "test/ad/bubsort.scm",
    "test/ad/dict.scm",
    // "test/ad/dictExamples.scm", // EMPTY
    //"test/ad/heap.scm", // PARSER ERROR TODO
    "test/ad/inssort.scm",
    "test/ad/linear.scm", // VARARG
    "test/ad/list.scm", // VARARG
    "test/ad/mesort.scm",
    "test/ad/prioq.scm",
    "test/ad/qsort.scm",
    "test/ad/qstand.scm",
    "test/ad/queue.scm", // VARARG
    "test/ad/quick.scm",
    "test/ad/RBtreeADT.scm", // VARARG
    "test/ad/selsort.scm", // PARSER ERROR TODO
    "test/ad/stack.scm",
    "test/ad/stspaceCODE.scm", // VARARG
  )

  val gabriel: Set[String] = Set(
    "test/gabriel/boyer.scm",
    "test/gabriel/browse.scm",
    "test/gabriel/cpstak.scm",
    "test/gabriel/dderiv.scm",
    "test/gabriel/deriv.scm",
    "test/gabriel/destruc.scm",
    "test/gabriel/diviter.scm",
    "test/gabriel/divrec.scm",
    "test/gabriel/puzzle.scm",
    "test/gabriel/takl.scm",
    "test/gabriel/triangl.scm",
  )

  val gambit: Set[String] = Set(
    "test/gambit/array1.scm",
    "test/gambit/browse.scm",
    "test/gambit/cat.scm",
    //"test/gambit/compiler.scm", // PARSER ERROR TODO
    "test/gambit/ctak.scm",
    "test/gambit/deriv.scm",
    "test/gambit/destruc.scm",
    "test/gambit/diviter.scm",
    "test/gambit/earley.scm",
    "test/gambit/fibc.scm",
    "test/gambit/graphs.scm",
    "test/gambit/lattice.scm",
    "test/gambit/matrix.scm",
    "test/gambit/mazefun.scm",
    "test/gambit/nboyer.scm", // VARARG
    "test/gambit/nqueens.scm",
    "test/gambit/paraffins.scm",
    "test/gambit/perm9.scm",
    "test/gambit/peval.scm", // VARARG
    "test/gambit/primes.scm",
    "test/gambit/puzzle.scm",
    "test/gambit/sboyer.scm", // VARARG
    "test/gambit/scheme.scm", // VARARG
    //"test/gambit/slatex.scm", // PARSER LIMITATION TODO
    "test/gambit/string.scm",
    "test/gambit/sum.scm",
    "test/gambit/sumloop.scm",
    "test/gambit/tail.scm",
    "test/gambit/tak.scm",
    //"test/gambit/trav1.scm", // PARSER ERROR TODO
    "test/gambit/triangl.scm",
    "test/gambit/wc.scm",
  )

  val rosetta: Set[String] = Set(
    "test/rosetta/easter.scm",
    "test/rosetta/quadratic.scm",
  )

  val scp1: Set[String] = Set(
    "test/scp1/2.1.scm",
    "test/scp1/2.4.scm",

    "test/scp1/3.1.scm",
    "test/scp1/3.2.1.scm",
    "test/scp1/3.2.scm",
    "test/scp1/3.3.scm",
    "test/scp1/3.4.scm",
    "test/scp1/3.6.scm",
    "test/scp1/3.8.scm",
    //"test/scp1/3.9.scm", // LOOPS, EVEN WITH FINER TIMEOUT TODO

    "test/scp1/4.1.scm", // LOOPS, EVEN WITH FINER TIMEOUT TODO
    "test/scp1/4.8.scm",

    "test/scp1/5.6.scm",
    "test/scp1/5.7.scm",
    "test/scp1/5.14.3.scm",
    "test/scp1/5.19.scm",
    "test/scp1/5.20.4.scm",
    "test/scp1/5.21.scm",
    "test/scp1/5.22.scm",

    "test/scp1/7.2.scm",
    "test/scp1/7.3.scm",
    "test/scp1/7.4.scm",
    // "test/scp1/7.5.scm", // DOT NOTATION
    // "test/scp1/7.6.scm", // DOT NOTATION
    //"test/scp1/7.9.scm",
    "test/scp1/7.11.scm",
    "test/scp1/7.12.scm",
    //"test/scp1/7.13.scm", // SOMETIMES LOOPS TODO
    "test/scp1/7.14.scm",
    "test/scp1/7.15.scm",
    "test/scp1/7.16.scm",
    "test/scp1/7.17.scm",
    "test/scp1/7.18.scm",

    "test/scp1/8.1.1.scm",
    "test/scp1/8.1.3.scm",
    "test/scp1/8.5.scm", // VARARG
    "test/scp1/8.6.scm",
    //"test/scp1/8.10.scm", // SMALLSTEP LOOPS, EVEN WITH FINER TIMEOUT TODO
    "test/scp1/8.11.scm", // VARARG
    "test/scp1/8.12.scm",
    "test/scp1/8.13.scm",
    "test/scp1/8.14.scm",
    "test/scp1/8.15.scm",
    "test/scp1/8.16.scm", // VARARG

    "test/scp1/9.2.scm",
    "test/scp1/9.3.scm",
    "test/scp1/9.5.scm",
    "test/scp1/9.6.scm",
    "test/scp1/9.7.scm",
    "test/scp1/9.8.scm",
    "test/scp1/9.9.scm",
    "test/scp1/9.12.scm",
    "test/scp1/9.13.scm",
    "test/scp1/9.14.scm",
    "test/scp1/9.15.scm",
    "test/scp1/9.16.scm",
    "test/scp1/9.17.scm",
    "test/scp1/9.18.scm",
  )

  val scp1_compressed: Set[String] = Set(
    // TODO Uncomment commented out files.
    "test/scp1-compressed/2.scm",
    "test/scp1-compressed/3.scm",
    "test/scp1-compressed/4.scm",
    "test/scp1-compressed/5.scm",
    "test/scp1-compressed/7.scm",
    "test/scp1-compressed/8.scm",
    "test/scp1-compressed/9.scm",
  )

  val scp1_singleFile: Set[String] = Set("test/scp1-compressed/all.scm")

  val sigscheme: Set[String] = Set(
    "test/sigscheme/arithint.scm",
    "test/sigscheme/case.scm",
    "test/sigscheme/let-loop.scm",
    "test/sigscheme/loop.scm",
    "test/sigscheme/mem.scm",
    "test/sigscheme/rec.scm",
    "test/sigscheme/takr.scm",
  )

  val theLittleSchemer: Set[String] = Set(
    "test/WeiChenRompf2019/the-little-schemer/ch1.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch2.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch3.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch4.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch5.scm",
    //"test/WeiChenRompf2019/the-little-schemer/ch6.scm", // PARSER LIMITATION TODO check whether needed
    "test/WeiChenRompf2019/the-little-schemer/ch7.scm",
    //"test/WeiChenRompf2019/the-little-schemer/ch8.scm", // PARSER LIMITATION TODO check whether needed
    //"test/WeiChenRompf2019/the-little-schemer/ch9.scm", // UNSUPPORTED FEATURE? (lambda args body)
    "test/WeiChenRompf2019/the-little-schemer/ch10.scm",
  )

  val toplas98: Set[String] = Set(
    //"test/WeiChenRompf2019/toplas98/boyer.sch", // USES SQUARE BRACKETS
    //"test/WeiChenRompf2019/toplas98/dynamic.sch", // PARSER LIMITATION TODO
    //"test/WeiChenRompf2019/toplas98/graphs.sch", // MISSING PRIMITIVE open-input-file
    //"test/WeiChenRompf2019/toplas98/handle.scm", // MAYBE INVALID SCHEME PROGRAM TODO check
    //"test/WeiChenRompf2019/toplas98/lattice.scm", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/toplas98/lattice-processed.scm", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/toplas98/maze.sch", // PARSER ERROR: #\space is interpreted as #\s pace
    //"test/WeiChenRompf2019/toplas98/nbody.sch", // PARSER LIMITATION TODO
    //"test/WeiChenRompf2019/toplas98/nbody-processed.sch", // PARSER LIMITATION TODO
    //"test/WeiChenRompf2019/toplas98/nucleic.sch", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/toplas98/nucleic2.sch", // USES MACROS: define-syntax
    //"test/WeiChenRompf2019/toplas98/splay.scm", // PARSER ERROR
  )

  val WCR2019: Set[String] = Set(
    //"test/WeiChenRompf2019/church_exp.sch", // PARSER LIMITATION TODO // Unknown (void) function
    "test/WeiChenRompf2019/church_simple.sch",
    //"test/WeiChenRompf2019/earley.sch", // MISSING PRIMITIVE read TODO
    "test/WeiChenRompf2019/fermat.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-16.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-32.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-64.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-256.scm",
    "test/WeiChenRompf2019/kcfa3.scm",
    //"test/WeiChenRompf2019/mbrotZ.sch", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/meta-circ.scm", // UNSUPPORTED FEATURE? (lambda args body)
    //"test/WeiChenRompf2019/omega.scm", // STACKOVERFLOW CONCRETE MACHINE
    "test/WeiChenRompf2019/regex-derivative.scm",
    "test/WeiChenRompf2019/rsa.scm",
    "test/WeiChenRompf2019/scheme2java.scm",
    "test/WeiChenRompf2019/solovay-strassen.scm",
  )

  val other: Set[String] = Set(
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
    "test/grid.scm",
    "test/inc.scm",
    "test/infinite-1.scm",
    "test/infinite-2.scm",
    "test/infinite-3.scm",
    "test/kcfa2.scm",
    "test/kcfa3.scm",
    "test/kernighanvanwyk/ack.scm",
    "test/letrec-begin.scm",
    "test/loop2.scm",
    "test/mceval.scm",
    "test/mj09.scm",
    "test/mut-rec.scm",
    "test/my-list.scm",
    "test/nested-defines.scm",
    "test/primtest.scm",
    "test/quasiquoting-simple.scm",
    //"test/quasiquoting.scm",  // unquote-splicing
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
    "test/work.scm",
  )

  val WeiChenRompf2019: Set[String] = WCR2019 ++ theLittleSchemer ++ toplas98
  val    allBenchmarks: Set[String] = ad ++ gabriel ++ gambit ++ rosetta ++ scp1 ++ sigscheme ++ WeiChenRompf2019 ++ other
}
