package scalaam.test

import org.scalatest.propspec.AnyPropSpec

import scala.util.Random

trait SchemeBenchmarkTests extends AnyPropSpec {
  // a benchmark is just a file name
  type Benchmark = String
  // the benchmarks involved in the tests
  def benchmarks(): Set[Benchmark] = Set.empty

  // needs to be implemented to specify the testing behaviour per benchmark
  protected def onBenchmark(b: Benchmark): Unit
  // run the benchmarks
  benchmarks().foreach(onBenchmark)
}

trait SimpleBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SchemeBenchmarks.other
}

trait RandomBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SchemeBenchmarks.selectRandom(40)
}

trait AllBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SchemeBenchmarks.allBenchmarks
}

object SchemeBenchmarks {

  val ad: Set[String] = Set(
    "test/R5RS/ad/abstrct.scm",
    //"test/R5RS/ad/bfirst.scm", // Unbound identifier: create-graph
    "test/R5RS/ad/bst.scm",
    //"test/R5RS/ad/btree.scm", // Lacks a body.
    "test/R5RS/ad/bubsort.scm",
    "test/R5RS/ad/dict.scm",
    "test/R5RS/ad/heap.scm",
    "test/R5RS/ad/inssort.scm",
    "test/R5RS/ad/linear.scm",
    "test/R5RS/ad/list.scm",
    "test/R5RS/ad/mesort.scm",
    "test/R5RS/ad/prioq.scm",
    "test/R5RS/ad/qsort.scm",
    "test/R5RS/ad/qstand.scm",
    "test/R5RS/ad/queue.scm",
    "test/R5RS/ad/quick.scm",
    "test/R5RS/ad/RBtreeADT.scm",
    "test/R5RS/ad/selsort.scm",
    "test/R5RS/ad/stack.scm",
    "test/R5RS/ad/stspaceCODE.scm",
  )

  val gabriel: Set[String] = Set(
    "test/R5RS/gabriel/boyer.scm",
    "test/R5RS/gabriel/browse.scm",
    "test/R5RS/gabriel/cpstak.scm",
    "test/R5RS/gabriel/dderiv.scm",
    "test/R5RS/gabriel/deriv.scm",
    "test/R5RS/gabriel/destruc.scm",
    "test/R5RS/gabriel/diviter.scm",
    "test/R5RS/gabriel/divrec.scm",
    "test/R5RS/gabriel/puzzle.scm",
    "test/R5RS/gabriel/takl.scm",
    "test/R5RS/gabriel/triangl.scm",
  )

  val gambit: Set[String] = Set(
    "test/R5RS/gambit/array1.scm",
    "test/R5RS/gambit/browse.scm",
    //"test/R5RS/gambit/cat.scm", // Needs open-input-file.
    //"test/R5RS/gambit/compiler.scm", // Parser error (see issue on Github).
    //"test/R5RS/gambit/ctak.scm", // Needs call-with-current-continuation.
    "test/R5RS/gambit/deriv.scm",
    "test/R5RS/gambit/destruc.scm",
    "test/R5RS/gambit/diviter.scm",
    "test/R5RS/gambit/earley.scm",
    //"test/R5RS/gambit/fibc.scm", // Needs call-cc.
    "test/R5RS/gambit/graphs.scm",
    "test/R5RS/gambit/lattice.scm",
    "test/R5RS/gambit/matrix.scm",
    "test/R5RS/gambit/mazefun.scm",
    "test/R5RS/gambit/nboyer.scm",
    "test/R5RS/gambit/nqueens.scm",
    "test/R5RS/gambit/paraffins.scm",
    "test/R5RS/gambit/perm9.scm",
    //"test/R5RS/gambit/peval.scm", // Error in program.
    "test/R5RS/gambit/primes.scm",
    //"test/R5RS/gambit/puzzle.scm",  // Needs call-with-current-continuation.
    "test/R5RS/gambit/sboyer.scm",
    //"test/R5RS/gambit/scheme.scm", // Error in program.
    //"test/R5RS/gambit/slatex.scm", // Needs integer->char.
    //"test/R5RS/gambit/string.scm", // Needs susbtring.
    "test/R5RS/gambit/sum.scm",
    "test/R5RS/gambit/sumloop.scm",
    //"test/R5RS/gambit/tail.scm", // Needs file manipulation primitives (open-input-port, close-input-port, read-char).
    "test/R5RS/gambit/tak.scm",
    //"test/R5RS/gambit/trav1.scm", // Needs append.
    "test/R5RS/gambit/triangl.scm",
    //"test/R5RS/gambit/wc.scm", // Needs file manipulation primitives (open-input-port, close-input-port, read-char).
  )

  val rosetta: Set[String] = Set(
    "test/R5RS/rosetta/easter.scm",
    "test/R5RS/rosetta/quadratic.scm",
  )

  val scp1: Set[String] = Set(
    // Procedures, blocks, conditions.
    "test/R5RS/scp1/leap-year.scm",
    "test/R5RS/scp1/third-root.scm",

    // Recursion vs. iteration.
    "test/R5RS/scp1/addition.scm",
    "test/R5RS/scp1/fast-multiply.scm",
    "test/R5RS/scp1/multiply.scm",
    "test/R5RS/scp1/calc-e-and-cos.scm",
    "test/R5RS/scp1/counter.scm",
    "test/R5RS/scp1/weird.scm",
    "test/R5RS/scp1/sim-fast-multiply.scm",
    "test/R5RS/scp1/draw-umbrella.scm",

    // Higher-Order Procedures
    "test/R5RS/scp1/print-abc.scm",
    "test/R5RS/scp1/simpson-integral.scm",

    // Lists.
    "test/R5RS/scp1/add-to-end.scm",
    "test/R5RS/scp1/append.scm",
    "test/R5RS/scp1/super-list-merge-n.scm",
    "test/R5RS/scp1/list-compare-n.scm",
    "test/R5RS/scp1/grades.scm",
    "test/R5RS/scp1/compress-measurements.scm",
    "test/R5RS/scp1/sales-period.scm",

    // Trees.
    "test/R5RS/scp1/count-tree.scm",
    "test/R5RS/scp1/fringe.scm",
    "test/R5RS/scp1/unfringe.scm",
    "test/R5RS/scp1/same-structure.scm",
    "test/R5RS/scp1/deep-map-combine.scm",
    "test/R5RS/scp1/apple-tree.scm",
    "test/R5RS/scp1/organigram.scm",
    "test/R5RS/scp1/fireworks.scm",
    "test/R5RS/scp1/university.scm",
    "test/R5RS/scp1/animal-classification.scm",
    "test/R5RS/scp1/tree-with-branches.scm",
    "test/R5RS/scp1/coca-cola.scm",
    "test/R5RS/scp1/family-budget.scm",
    //"test/R5RS/scp1/circus.scm", // Vararg append nog supported by concrete interpreter.

    // Objects.
    "test/R5RS/scp1/flip.scm",
    "test/R5RS/scp1/flip2.scm",
    "test/R5RS/scp1/polynome.scm",
    "test/R5RS/scp1/haha.scm",
    "test/R5RS/scp1/scoreboard.scm",
    "test/R5RS/scp1/parking-counter.scm",
    "test/R5RS/scp1/square-and-rectangle.scm",
    "test/R5RS/scp1/lightbulb.scm",
    "test/R5RS/scp1/cashdesk-counter.scm",
    "test/R5RS/scp1/car-counter.scm",
    "test/R5RS/scp1/twitter.scm",

    // Destructive operations
    "test/R5RS/scp1/count-pairs.scm",
    "test/R5RS/scp1/ring.scm",
    "test/R5RS/scp1/ring-rotate.scm",
    "test/R5RS/scp1/find-cycles.scm",
    "test/R5RS/scp1/ring-copy.scm",
    "test/R5RS/scp1/josephus-problem.scm",
    "test/R5RS/scp1/count-pairs2.scm",
    "test/R5RS/scp1/flatten.scm",
    "test/R5RS/scp1/ring-squares.scm",
    "test/R5RS/scp1/slide-in.scm",
    "test/R5RS/scp1/dedouble.scm",
    "test/R5RS/scp1/insert.scm",
    "test/R5RS/scp1/all-but-interval.scm",
    "test/R5RS/scp1/merge.scm",
  )

  val scp1_compressed: Set[String] = Set(
    "test/R5RS/scp1-compressed/2.scm",
    "test/R5RS/scp1-compressed/3.scm",
    "test/R5RS/scp1-compressed/4.scm",
    "test/R5RS/scp1-compressed/5.scm",
    "test/R5RS/scp1-compressed/7.scm",
    "test/R5RS/scp1-compressed/8.scm",
    "test/R5RS/scp1-compressed/9.scm",
  )

  val scp1_singleFile: Set[String] = Set("test/R5RS/scp1-compressed/all.scm")

  val SICP: Set[String] = Set(
    "test/R5RS/icp/icp_1c_multiple-dwelling.scm",
    "test/R5RS/icp/icp_1c_ontleed.scm",
    "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
    "test/R5RS/icp/icp_2_aeval.scm",
    "test/R5RS/icp/icp_3_leval.scm",
    //"test/R5RS/icp/icp_4_qeval.scm", // Needs define-syntax and delay.
    "test/R5RS/icp/icp_5_regsim.scm",
    "test/R5RS/icp/icp_6_stopandcopy_scheme.scm",
    "test/R5RS/icp/icp_7_eceval.scm",
    "test/R5RS/icp/icp_8_compiler.scm",
  )

  val sigscheme: Set[String] = Set(
    "test/R5RS/sigscheme/arithint.scm",
    "test/R5RS/sigscheme/case.scm",
    "test/R5RS/sigscheme/let-loop.scm",
    "test/R5RS/sigscheme/loop.scm",
    "test/R5RS/sigscheme/mem.scm",
    "test/R5RS/sigscheme/rec.scm",
    "test/R5RS/sigscheme/takr.scm",
  )

  val theLittleSchemer: Set[String] = Set(
    "test/R5RS/WeiChenRompf2019/the-little-schemer/ch1.scm",
    "test/R5RS/WeiChenRompf2019/the-little-schemer/ch2.scm",
    "test/R5RS/WeiChenRompf2019/the-little-schemer/ch3.scm",
    //"test/R5RS/WeiChenRompf2019/the-little-schemer/ch4.scm", // No main code (only definitions).
    //"test/R5RS/WeiChenRompf2019/the-little-schemer/ch5.scm", // No main code (only definitions).
    //"test/R5RS/WeiChenRompf2019/the-little-schemer/ch6.scm", // Commented out half of the file. Now does not parse anymore.
    //"test/R5RS/WeiChenRompf2019/the-little-schemer/ch7.scm", // No main code (only definitions).
    "test/R5RS/WeiChenRompf2019/the-little-schemer/ch8.scm",
    //"test/R5RS/WeiChenRompf2019/the-little-schemer/ch9.scm", // Unbound identifier: will-stop?
    "test/R5RS/WeiChenRompf2019/the-little-schemer/ch10.scm",
  )

  val toplas98: Set[String] = Set(
    //"test/R5RS/WeiChenRompf2019/toplas98/boyer.sch", // Uses square brackets.
    //"test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm", // Uses call-with-input-file
    //"test/R5RS/WeiChenRompf2019/toplas98/graphs.scm", // Uses open-input-file.
    //"test/R5RS/WeiChenRompf2019/toplas98/handle.scm", // Uses defmacro (not standard r5rs).
    //"test/R5RS/WeiChenRompf2019/toplas98/lattice.scm", // Parser error. Uses undefined (void) function.
    //"test/R5RS/WeiChenRompf2019/toplas98/lattice-processed.scm", // Parser error. Uses undefined (void) function.
    //"test/R5RS/WeiChenRompf2019/toplas98/maze.scm", // Uses open-input-file.
    //"test/R5RS/WeiChenRompf2019/toplas98/nbody.scm", // Parser error.
    //"test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm", // Parser error.
    //"test/R5RS/WeiChenRompf2019/toplas98/nucleic.sch", // Uses square brackets.
    //"test/R5RS/WeiChenRompf2019/toplas98/nucleic2.sch", // USES macros (define-syntax).
    //"test/R5RS/WeiChenRompf2019/toplas98/splay.scm", // Uses () instead of '(), but has other issues.
  )

  val WCR2019: Set[String] = Set(
    //"test/R5RS/WeiChenRompf2019/church_exp.sch", // Uses non-standard (void) function.
    "test/R5RS/WeiChenRompf2019/church_simple.sch",
    //"test/R5RS/WeiChenRompf2019/earley.sch", // Uses read.
    "test/R5RS/WeiChenRompf2019/fermat.scm",
    "test/R5RS/WeiChenRompf2019/kcfa-worst-case-16.scm",
    "test/R5RS/WeiChenRompf2019/kcfa-worst-case-32.scm",
    "test/R5RS/WeiChenRompf2019/kcfa-worst-case-64.scm",
    "test/R5RS/WeiChenRompf2019/kcfa-worst-case-256.scm",
    "test/R5RS/WeiChenRompf2019/kcfa3.scm",
    //"test/R5RS/WeiChenRompf2019/mbrotZ.sch", // Parser error.
    //"test/R5RS/WeiChenRompf2019/meta-circ.scm", // Uses procedure?
    "test/R5RS/WeiChenRompf2019/omega.scm",
    //"test/R5RS/WeiChenRompf2019/regex-derivative.scm", // Parser error.
    "test/R5RS/WeiChenRompf2019/rsa.scm",
    //"test/R5RS/WeiChenRompf2019/scheme2java.scm", // Uses char-alphabetic?
    //"test/R5RS/WeiChenRompf2019/solovay-strassen.scm", // Program seems erroneous.
  )

  val other: Set[String] = Set(
    "test/R5RS/blur.scm",
    "test/R5RS/bound-precision.scm",
    "test/R5RS/church-2-num.scm",
    "test/R5RS/church-6.scm",
    "test/R5RS/church.scm",
    "test/R5RS/collatz.scm",
    "test/R5RS/count.scm",
    "test/R5RS/eta.scm",
    "test/R5RS/fact.scm",
    "test/R5RS/fib.scm",
    "test/R5RS/gcipd.scm",
    "test/R5RS/grid.scm",
    "test/R5RS/inc.scm",
    "test/R5RS/infinite-1.scm",
    "test/R5RS/infinite-2.scm",
    "test/R5RS/infinite-3.scm",
    "test/R5RS/kcfa2.scm",
    "test/R5RS/kcfa3.scm",
    "test/R5RS/kernighanvanwyk/ack.scm",
    "test/R5RS/letrec-begin.scm",
    "test/R5RS/loop2.scm",
    "test/R5RS/mceval.scm",
    "test/R5RS/mj09.scm",
    "test/R5RS/mut-rec.scm",
    "test/R5RS/my-list.scm",
    "test/R5RS/nested-defines.scm",
    "test/R5RS/primtest.scm",
    "test/R5RS/quasiquoting-simple.scm",
    //"test/R5RS/quasiquoting.scm", // Uses unquote-splicing.
    "test/R5RS/regex.scm",
    "test/R5RS/rotate.scm",
    "test/R5RS/rsa.scm",
    "test/R5RS/sat.scm",
    //"test/R5RS/scm2c.scm", // Uses string->list.
    //"test/R5RS/scm2java.scm", // Uses list->string.
    "test/R5RS/sq.scm",
    //"test/R5RS/Streams.scm", // Uses define-macro.
    "test/R5RS/sym.scm",
    "test/R5RS/widen.scm",
    "test/R5RS/work.scm",
  )

  val WeiChenRompf2019: Set[String] = WCR2019 ++ theLittleSchemer ++ toplas98
  val    allBenchmarks: Set[String] = ad ++ gabriel ++ gambit ++ rosetta ++ scp1 ++ SICP ++ sigscheme ++ WeiChenRompf2019 ++ other

  def selectRandom(n: Int): Set[String] = Random.shuffle(allBenchmarks).take(n)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val savina: Set[String] = Set(
    "test/concurrentScheme/actors/savina/big.scm",
    "test/concurrentScheme/actors/savina/cham.scm",
    "test/concurrentScheme/actors/savina/count-seq.scm",
    "test/concurrentScheme/actors/savina/count.scm",
    "test/concurrentScheme/actors/savina/fjc-seq.scm",
    "test/concurrentScheme/actors/savina/fjc.scm",
    "test/concurrentScheme/actors/savina/fjt-seq.scm",
    "test/concurrentScheme/actors/savina/fjt-seq2.scm",
    "test/concurrentScheme/actors/savina/fjt.scm",
    "test/concurrentScheme/actors/savina/phil.scm",
    "test/concurrentScheme/actors/savina/pp.scm",
    "test/concurrentScheme/actors/savina/thr-seq.scm",
    "test/concurrentScheme/actors/savina/thr.scm",
  )

  val soter: Set[String] = Set(
    "test/concurrentScheme/actors/soter/concdb.scm",
    "test/concurrentScheme/actors/soter/parikh.scm",
    "test/concurrentScheme/actors/soter/pipe-seq.scm",
    "test/concurrentScheme/actors/soter/pipe.scm",
    "test/concurrentScheme/actors/soter/safe_send.scm",
    "test/concurrentScheme/actors/soter/state_factory.scm",
    "test/concurrentScheme/actors/soter/stutter.scm",
    "test/concurrentScheme/actors/soter/unsafe_send.scm",
  )
  
  val actors: Set[String] = savina ++ soter ++ Set(
    "test/concurrentScheme/actors/cell.scm",
    "test/concurrentScheme/actors/check.scm",
    "test/concurrentScheme/actors/factorial.scm",
    "test/concurrentScheme/actors/stack.scm",
  )

  val futures: Set[String] = Set(
    "test/concurrentScheme/futures/abp.scm",
    "test/concurrentScheme/futures/actors.scm",
    "test/concurrentScheme/futures/atoms.scm",
    "test/concurrentScheme/futures/bchain.scm",
    "test/concurrentScheme/futures/count.scm",
    "test/concurrentScheme/futures/crypt.scm",
    "test/concurrentScheme/futures/dekker.scm",
    "test/concurrentScheme/futures/fact-indep.scm",
    "test/concurrentScheme/futures/fact.scm",
    "test/concurrentScheme/futures/fact2.scm",
    "test/concurrentScheme/futures/lastzero2.scm",
    "test/concurrentScheme/futures/life.scm",
    "test/concurrentScheme/futures/list-with-length.scm",
    "test/concurrentScheme/futures/matmul.scm",
    "test/concurrentScheme/futures/mcarlo.scm",
    "test/concurrentScheme/futures/mceval.scm",
    "test/concurrentScheme/futures/minimax.scm",
    "test/concurrentScheme/futures/msort.scm",
    "test/concurrentScheme/futures/nbody.scm",
    "test/concurrentScheme/futures/pc.scm",
    "test/concurrentScheme/futures/phil.scm",
    "test/concurrentScheme/futures/phild.scm",
    "test/concurrentScheme/futures/pp.scm",
    "test/concurrentScheme/futures/pps.scm",
    "test/concurrentScheme/futures/qsort.scm",
    "test/concurrentScheme/futures/readers2.scm",
    "test/concurrentScheme/futures/ringbuf.scm",
    "test/concurrentScheme/futures/rng.scm",
    "test/concurrentScheme/futures/sieve.scm",
    "test/concurrentScheme/futures/simple.scm",
    "test/concurrentScheme/futures/stm.scm",
    "test/concurrentScheme/futures/sudoku.scm",
    "test/concurrentScheme/futures/trapr.scm",
    "test/concurrentScheme/futures/treiber-stack.scm",
    "test/concurrentScheme/futures/tsp.scm",
  )

  val futuresVariations: Set[String] = Set(
    "test/concurrentScheme/futures/variations/count10.scm",
    "test/concurrentScheme/futures/variations/count11.scm",
    "test/concurrentScheme/futures/variations/count12.scm",
    "test/concurrentScheme/futures/variations/count13.scm",
    "test/concurrentScheme/futures/variations/count14.scm",
    "test/concurrentScheme/futures/variations/count15.scm",
    "test/concurrentScheme/futures/variations/count2.scm",
    "test/concurrentScheme/futures/variations/count3.scm",
    "test/concurrentScheme/futures/variations/count4.scm",
    "test/concurrentScheme/futures/variations/count5.scm",
    "test/concurrentScheme/futures/variations/count6.scm",
    "test/concurrentScheme/futures/variations/count7.scm",
    "test/concurrentScheme/futures/variations/count8.scm",
    "test/concurrentScheme/futures/variations/count9.scm",
    "test/concurrentScheme/futures/variations/indexer10.scm",
    "test/concurrentScheme/futures/variations/indexer11.scm",
    "test/concurrentScheme/futures/variations/indexer12.scm",
    "test/concurrentScheme/futures/variations/indexer13.scm",
    "test/concurrentScheme/futures/variations/indexer14.scm",
    "test/concurrentScheme/futures/variations/indexer15.scm",
    "test/concurrentScheme/futures/variations/indexer2.scm",
    "test/concurrentScheme/futures/variations/indexer3.scm",
    "test/concurrentScheme/futures/variations/indexer4.scm",
    "test/concurrentScheme/futures/variations/indexer5.scm",
    "test/concurrentScheme/futures/variations/indexer6.scm",
    "test/concurrentScheme/futures/variations/indexer7.scm",
    "test/concurrentScheme/futures/variations/indexer8.scm",
    "test/concurrentScheme/futures/variations/indexer9.scm",
    "test/concurrentScheme/futures/variations/mutex2.scm",
    "test/concurrentScheme/futures/variations/mutex3.scm",
    "test/concurrentScheme/futures/variations/mutex4.scm",
    "test/concurrentScheme/futures/variations/mutex5.scm",
    "test/concurrentScheme/futures/variations/mutex6.scm",
    "test/concurrentScheme/futures/variations/pcounter10.scm",
    "test/concurrentScheme/futures/variations/pcounter11.scm",
    "test/concurrentScheme/futures/variations/pcounter12.scm",
    "test/concurrentScheme/futures/variations/pcounter13.scm",
    "test/concurrentScheme/futures/variations/pcounter14.scm",
    "test/concurrentScheme/futures/variations/pcounter15.scm",
    "test/concurrentScheme/futures/variations/pcounter2.scm",
    "test/concurrentScheme/futures/variations/pcounter3.scm",
    "test/concurrentScheme/futures/variations/pcounter4.scm",
    "test/concurrentScheme/futures/variations/pcounter5.scm",
    "test/concurrentScheme/futures/variations/pcounter6.scm",
    "test/concurrentScheme/futures/variations/pcounter7.scm",
    "test/concurrentScheme/futures/variations/pcounter8.scm",
    "test/concurrentScheme/futures/variations/pcounter9.scm",
    "test/concurrentScheme/futures/variations/race2.scm",
    "test/concurrentScheme/futures/variations/race3.scm",
    "test/concurrentScheme/futures/variations/race4.scm",
    "test/concurrentScheme/futures/variations/race5.scm",
    "test/concurrentScheme/futures/variations/race6.scm",
  )

  val threads: Set[String] = Set(
    "test/concurrentScheme/threads/abp.scm",
    "test/concurrentScheme/threads/actors.scm",
    "test/concurrentScheme/threads/atoms.scm",
    "test/concurrentScheme/threads/bchain.scm",
    "test/concurrentScheme/threads/count.scm",
    "test/concurrentScheme/threads/crypt.scm",
    "test/concurrentScheme/threads/dekker.scm",
    "test/concurrentScheme/threads/fact-indep.scm",
    "test/concurrentScheme/threads/fact.scm",
    "test/concurrentScheme/threads/fact2.scm",
    "test/concurrentScheme/threads/lastzero2.scm",
    "test/concurrentScheme/threads/life.scm",
    "test/concurrentScheme/threads/matmul.scm",
    "test/concurrentScheme/threads/mcarlo.scm",
    "test/concurrentScheme/threads/mceval.scm",
    "test/concurrentScheme/threads/minimax.scm",
    "test/concurrentScheme/threads/msort.scm",
    "test/concurrentScheme/threads/nbody.scm",
    "test/concurrentScheme/threads/pc.scm",
    "test/concurrentScheme/threads/peterson.scm",
    "test/concurrentScheme/threads/phil.scm",
    "test/concurrentScheme/threads/phild.scm",
    "test/concurrentScheme/threads/philosophers2.scm",
    "test/concurrentScheme/threads/pp.scm",
    "test/concurrentScheme/threads/pps.scm",
    "test/concurrentScheme/threads/producer.scm",
    "test/concurrentScheme/threads/qsort.scm",
    "test/concurrentScheme/threads/randomness.scm",
    "test/concurrentScheme/threads/readers2.scm",
    "test/concurrentScheme/threads/ringbuf.scm",
    "test/concurrentScheme/threads/rng.scm",
    "test/concurrentScheme/threads/sieve.scm",
    "test/concurrentScheme/threads/simple.scm",
    "test/concurrentScheme/threads/stm.scm",
    "test/concurrentScheme/threads/sudoku.scm",
    "test/concurrentScheme/threads/trapr.scm",
    "test/concurrentScheme/threads/tsp.scm",
  )

  val threadsVariations: Set[String] = Set(
    "test/concurrentScheme/threads/variations/count10.scm",
    "test/concurrentScheme/threads/variations/count11.scm",
    "test/concurrentScheme/threads/variations/count12.scm",
    "test/concurrentScheme/threads/variations/count13.scm",
    "test/concurrentScheme/threads/variations/count14.scm",
    "test/concurrentScheme/threads/variations/count15.scm",
    "test/concurrentScheme/threads/variations/count2.scm",
    "test/concurrentScheme/threads/variations/count3.scm",
    "test/concurrentScheme/threads/variations/count4.scm",
    "test/concurrentScheme/threads/variations/count5.scm",
    "test/concurrentScheme/threads/variations/count6.scm",
    "test/concurrentScheme/threads/variations/count7.scm",
    "test/concurrentScheme/threads/variations/count8.scm",
    "test/concurrentScheme/threads/variations/count9.scm",
    "test/concurrentScheme/threads/variations/fs10.scm",
    "test/concurrentScheme/threads/variations/fs11.scm",
    "test/concurrentScheme/threads/variations/fs12.scm",
    "test/concurrentScheme/threads/variations/fs13.scm",
    "test/concurrentScheme/threads/variations/fs14.scm",
    "test/concurrentScheme/threads/variations/fs15.scm",
    "test/concurrentScheme/threads/variations/fs2.scm",
    "test/concurrentScheme/threads/variations/fs3.scm",
    "test/concurrentScheme/threads/variations/fs4.scm",
    "test/concurrentScheme/threads/variations/fs5.scm",
    "test/concurrentScheme/threads/variations/fs6.scm",
    "test/concurrentScheme/threads/variations/fs7.scm",
    "test/concurrentScheme/threads/variations/fs8.scm",
    "test/concurrentScheme/threads/variations/fs9.scm",
    "test/concurrentScheme/threads/variations/incdec2.scm",
    "test/concurrentScheme/threads/variations/incdec3.scm",
    "test/concurrentScheme/threads/variations/incdec4.scm",
    "test/concurrentScheme/threads/variations/incdec5.scm",
    "test/concurrentScheme/threads/variations/incdec6.scm",
    "test/concurrentScheme/threads/variations/indexer10.scm",
    "test/concurrentScheme/threads/variations/indexer11.scm",
    "test/concurrentScheme/threads/variations/indexer12.scm",
    "test/concurrentScheme/threads/variations/indexer13.scm",
    "test/concurrentScheme/threads/variations/indexer14.scm",
    "test/concurrentScheme/threads/variations/indexer15.scm",
    "test/concurrentScheme/threads/variations/indexer2.scm",
    "test/concurrentScheme/threads/variations/indexer3.scm",
    "test/concurrentScheme/threads/variations/indexer4.scm",
    "test/concurrentScheme/threads/variations/indexer5.scm",
    "test/concurrentScheme/threads/variations/indexer6.scm",
    "test/concurrentScheme/threads/variations/indexer7.scm",
    "test/concurrentScheme/threads/variations/indexer8.scm",
    "test/concurrentScheme/threads/variations/indexer9.scm",
    "test/concurrentScheme/threads/variations/mutex2.scm",
    "test/concurrentScheme/threads/variations/mutex3.scm",
    "test/concurrentScheme/threads/variations/mutex4.scm",
    "test/concurrentScheme/threads/variations/mutex5.scm",
    "test/concurrentScheme/threads/variations/mutex6.scm",
    "test/concurrentScheme/threads/variations/pcounter10.scm",
    "test/concurrentScheme/threads/variations/pcounter11.scm",
    "test/concurrentScheme/threads/variations/pcounter12.scm",
    "test/concurrentScheme/threads/variations/pcounter13.scm",
    "test/concurrentScheme/threads/variations/pcounter14.scm",
    "test/concurrentScheme/threads/variations/pcounter15.scm",
    "test/concurrentScheme/threads/variations/pcounter2.scm",
    "test/concurrentScheme/threads/variations/pcounter3.scm",
    "test/concurrentScheme/threads/variations/pcounter4.scm",
    "test/concurrentScheme/threads/variations/pcounter5.scm",
    "test/concurrentScheme/threads/variations/pcounter6.scm",
    "test/concurrentScheme/threads/variations/pcounter7.scm",
    "test/concurrentScheme/threads/variations/pcounter8.scm",
    "test/concurrentScheme/threads/variations/pcounter9.scm",
    "test/concurrentScheme/threads/variations/philosophers3.scm",
    "test/concurrentScheme/threads/variations/philosophers4.scm",
    "test/concurrentScheme/threads/variations/philosophers5.scm",
    "test/concurrentScheme/threads/variations/philosophers6.scm",
    "test/concurrentScheme/threads/variations/race2.scm",
    "test/concurrentScheme/threads/variations/race3.scm",
    "test/concurrentScheme/threads/variations/race4.scm",
    "test/concurrentScheme/threads/variations/race5.scm",
    "test/concurrentScheme/threads/variations/race6.scm",
  )
}