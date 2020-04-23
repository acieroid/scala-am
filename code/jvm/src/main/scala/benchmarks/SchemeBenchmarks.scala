package scalaam.cli.benchmarks

object SchemeBenchmarks {

    val standard = Set(
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
        //"test/grid.scm",
        "test/inc.scm",
        "test/infinite-1.scm",
        //"test/infinite-2.scm",
        //"test/infinite-3.scm",
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
        //"test/primtest.scm",
        //"test/quasiquoting-simple.scm",
        //"test/quasiquoting.scm",
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
        "test/work.scm"
    )

    val scp1_compressed: List[String] = List(
      "test/scp1-compressed/2.scm",
      "test/scp1-compressed/3.scm",
      "test/scp1-compressed/4.scm",
      "test/scp1-compressed/5.scm",
      "test/scp1-compressed/7.scm",
      "test/scp1-compressed/8.scm",
      "test/scp1-compressed/9.scm",
    )

    val gabriel = List(
      "test/gabriel/browse.scm",
      "test/gabriel/puzzle.scm",
      "test/gabriel/dderiv.scm",
      "test/gabriel/destruc.scm",
      "test/gabriel/deriv.scm",
      "test/gabriel/diviter.scm",
      "test/gabriel/divrec.scm",
    )

    val gambit = List(
      //"test/gambit/scheme.scm",
      //"test/gambit/sboyer.scm",
      //"test/gambit/nboyer.scm",
      "test/gambit/matrix.scm",
      //"test/gambit/peval.scm",
      //"test/gambit/earley.scm", // list->vector
      "test/gambit/graphs.scm",
      "test/gambit/lattice.scm",
      "test/gambit/mazefun.scm",
      // "test/gambit/browse.scm", // part of gabriel
      "test/gambit/paraffins.scm",
      // "test/gambit/puzzle.scm",
      // "test/gambit/trav1.scm", // append? or dot notation
      "test/gambit/perm9.scm",
      // "test/gambit/destruc.scm", // part of gabriel
      // "test/gambit/triangl.scm", // list->vector
      "test/gambit/nqueens.scm",
      "test/gambit/primes.scm",
      "test/gambit/sumloop.scm",
    )


  val ad = List(
    // "test/ad/bfirst.scm", // incorrect benchmark
    // "test/ad/bst.scm", // only definitions
    // "test/ad/btree.scm", // only definitions
    "test/ad/dict.scm",
    "test/ad/prioq.scm",
    // "test/ad/queue.scm", // only definitions
    "test/ad/quick.scm",
    // "test/ad/RBtreeADT.scm", // dot notation?
    "test/ad/stack.scm",
    // "test/stspaceCODE.scm" // only definitions
    "test/ad/abstrct.scm",
    "test/ad/bubsort.scm",
    "test/ad/heap.scm",
    "test/ad/inssort.scm",
    "test/ad/linear.scm",
    "test/ad/list.scm",
    "test/ad/mesort.scm",
    "test/ad/qsort.scm",
    "test/ad/qstand.scm",
    "test/ad/selsort.scm",
  )


  val icp = List(
    // "test/icp/icp_1c_ontleed.scm", // too slow
    // "test/icp/icp_1c_multiple-dwelling.scm", // stack overflows the concrete interpreter
    // "test/icp/icp_1c_prime-sum-pair.scm", // too slow
    "test/icp/icp_2_aeval.scm",
    "test/icp/icp_3_leval.scm",
    // "test/icp/icp_4_qeval.scm", // define-syntax, apply, eval
    "test/icp/icp_5_regsim.scm",
    // "test/icp/icp_6_stopandcopy_scheme", // vectors
    // "test/icp/icp_7_eceval.scm", // too slow
    "test/icp/icp_8_compiler.scm"
  )

}
