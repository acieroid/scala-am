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
    //"test/ad/bfirst.scm", // Unbound identifier: create-graph
    "test/ad/bst.scm",
    //"test/ad/btree.scm", // TODO add a body
    "test/ad/bubsort.scm",
    "test/ad/dict.scm",
    "test/ad/heap.scm",
    "test/ad/inssort.scm",
    "test/ad/linear.scm",
    "test/ad/list.scm",
    "test/ad/mesort.scm",
    "test/ad/prioq.scm",
    "test/ad/qsort.scm",
    "test/ad/qstand.scm",
    "test/ad/queue.scm",
    "test/ad/quick.scm",
    "test/ad/RBtreeADT.scm",
    "test/ad/selsort.scm",
    "test/ad/stack.scm",
    "test/ad/stspaceCODE.scm",
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
    //"test/gambit/cat.scm", // Needs open-input-file.
    //"test/gambit/compiler.scm", // Parser error (see issue on Github).
    "test/gambit/ctak.scm",
    "test/gambit/deriv.scm",
    "test/gambit/destruc.scm",
    "test/gambit/diviter.scm",
    "test/gambit/earley.scm",
    //"test/gambit/fibc.scm", // Needs call-cc.
    "test/gambit/graphs.scm",
    "test/gambit/lattice.scm",
    "test/gambit/matrix.scm",
    "test/gambit/mazefun.scm",
    "test/gambit/nboyer.scm",
    "test/gambit/nqueens.scm",
    "test/gambit/paraffins.scm",
    "test/gambit/perm9.scm",
    //"test/gambit/peval.scm", // Error in program.
    "test/gambit/primes.scm",
    "test/gambit/puzzle.scm",
    "test/gambit/sboyer.scm",
    //"test/gambit/scheme.scm", // Error in program.
    //"test/gambit/slatex.scm", // Needs integer->char.
    //"test/gambit/string.scm", // Needs susbtring.
    "test/gambit/sum.scm",
    "test/gambit/sumloop.scm",
    "test/gambit/tail.scm",
    "test/gambit/tak.scm",
    "test/gambit/trav1.scm",
    "test/gambit/triangl.scm",
    "test/gambit/wc.scm",
  )

  val rosetta: Set[String] = Set(
    "test/rosetta/easter.scm",
    "test/rosetta/quadratic.scm",
  )

  val scp1: Set[String] = Set(
    // Procedures, blocks, conditions.
    "test/scp1/leap-year.scm",
    "test/scp1/third-root.scm",

    // Recursion vs. iteration.
    "test/scp1/addition.scm",
    "test/scp1/fast-multiply.scm",
    "test/scp1/multiply.scm",
    "test/scp1/calc-e-and-cos.scm",
    "test/scp1/counter.scm",
    "test/scp1/weird.scm",
    "test/scp1/sim-fast-multiply.scm",
    "test/scp1/draw-umbrella.scm",

    // Higher-Order Procedures
    "test/scp1/print-abc.scm",
    "test/scp1/simpson-integral.scm",

    // Lists.
    "test/scp1/add-to-end.scm",
    "test/scp1/append.scm",
    "test/scp1/super-list-merge-n.scm",
    "test/scp1/list-compare-n.scm",
    "test/scp1/grades.scm",
    "test/scp1/compress-measurements.scm",
    "test/scp1/sales-period.scm",

    // Trees.
    "test/scp1/count-tree.scm",
    "test/scp1/fringe.scm",
    "test/scp1/unfringe.scm",
    "test/scp1/same-structure.scm",
    "test/scp1/deep-map-combine.scm",
    "test/scp1/apple-tree.scm",
    "test/scp1/organigram.scm",
    "test/scp1/fireworks.scm",
    "test/scp1/university.scm",
    "test/scp1/animal-classification.scm",
    "test/scp1/tree-with-branches.scm",
    "test/scp1/coca-cola.scm",
    "test/scp1/family-budget.scm",
    //"test/scp1/circus.scm", // Vararg append nog supported by concrete interpreter.

    // Objects.
    "test/scp1/flip.scm",
    "test/scp1/flip2.scm",
    "test/scp1/polynome.scm",
    "test/scp1/haha.scm",
    "test/scp1/scoreboard.scm",
    "test/scp1/parking-counter.scm",
    "test/scp1/square-and-rectangle.scm",
    "test/scp1/lightbulb.scm",
    "test/scp1/cashdesk-counter.scm",
    "test/scp1/car-counter.scm",
    "test/scp1/twitter.scm",

    // Destructive operations
    "test/scp1/count-pairs.scm",
    "test/scp1/ring.scm",
    "test/scp1/ring-rotate.scm",
    "test/scp1/find-cycles.scm",
    "test/scp1/ring-copy.scm",
    "test/scp1/josephus-problem.scm",
    "test/scp1/count-pairs2.scm",
    "test/scp1/flatten.scm",
    "test/scp1/ring-squares.scm",
    "test/scp1/slide-in.scm",
    "test/scp1/dedouble.scm",
    "test/scp1/insert.scm",
    "test/scp1/all-but-interval.scm",
    "test/scp1/merge.scm",
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

  val SICP: Set[String] = Set(
    "test/icp/icp_1c_multiple-dwelling.scm",
    "test/icp/icp_1c_ontleed.scm",
    "test/icp/icp_1c_prime-sum-pair.scm",
    "test/icp/icp_2_aeval.scm",
    "test/icp/icp_3_leval.scm",
    //"test/icp/icp_4_qeval.scm", // Needs define-syntax and delay.
    "test/icp/icp_5_regsim.scm",
    "test/icp/icp_6_stopandcopy_scheme.scm",
    "test/icp/icp_7_eceval.scm",
    "test/icp/icp_8_compiler.scm",
  )

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
    //"test/WeiChenRompf2019/the-little-schemer/ch4.scm", // No main code (only definitions).
    //"test/WeiChenRompf2019/the-little-schemer/ch5.scm", // No main code (only definitions).
    //"test/WeiChenRompf2019/the-little-schemer/ch6.scm", // Commented out half of the file. Now does not parse anymore.
    "test/WeiChenRompf2019/the-little-schemer/ch7.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch8.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch9.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch10.scm",
  )

  val toplas98: Set[String] = Set(
    //"test/WeiChenRompf2019/toplas98/boyer.sch", // Uses square brackets.
    //"test/WeiChenRompf2019/toplas98/dynamic.scm", // Uses call-with-input-file
    //"test/WeiChenRompf2019/toplas98/graphs.scm", // Uses open-input-file.
    //"test/WeiChenRompf2019/toplas98/handle.scm", // Uses defmacro (not standard r5rs).
    //"test/WeiChenRompf2019/toplas98/lattice.scm", // Parser error. Uses undefined (void) function.
    //"test/WeiChenRompf2019/toplas98/lattice-processed.scm", // Parser error. Uses undefined (void) function.
    //"test/WeiChenRompf2019/toplas98/maze.scm", // Uses open-input-file.
    //"test/WeiChenRompf2019/toplas98/nbody.scm", // Parser error.
    //"test/WeiChenRompf2019/toplas98/nbody-processed.scm", // Parser error.
    //"test/WeiChenRompf2019/toplas98/nucleic.sch", // Uses square brackets.
    //"test/WeiChenRompf2019/toplas98/nucleic2.sch", // USES macros (define-syntax).
    //"test/WeiChenRompf2019/toplas98/splay.scm", // Uses () instead of '(), but has other issues.
  )

  val WCR2019: Set[String] = Set(
    //"test/WeiChenRompf2019/church_exp.sch", // Uses non-standard (void) function.
    "test/WeiChenRompf2019/church_simple.sch",
    //"test/WeiChenRompf2019/earley.sch", // Uses read.
    "test/WeiChenRompf2019/fermat.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-16.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-32.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-64.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-256.scm",
    "test/WeiChenRompf2019/kcfa3.scm",
    //"test/WeiChenRompf2019/mbrotZ.sch", // Parser error.
    //"test/WeiChenRompf2019/meta-circ.scm", // UNSUPPORTED FEATURE? (lambda args body)
    //"test/WeiChenRompf2019/omega.scm", // STACKOVERFLOW CONCRETE MACHINE
    //"test/WeiChenRompf2019/regex-derivative.scm", // PARSER ERROR
    "test/WeiChenRompf2019/rsa.scm",
    //"test/WeiChenRompf2019/scheme2java.scm", // Needs primitive char-alphabetic?
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
  val    allBenchmarks: Set[String] = ad ++ gabriel ++ gambit ++ rosetta ++ scp1 ++ SICP ++ sigscheme ++ WeiChenRompf2019 ++ other
}
