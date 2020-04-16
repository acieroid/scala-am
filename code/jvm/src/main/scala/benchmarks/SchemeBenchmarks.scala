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

  val forPrimitives = List(
    //"test/SICP-compiler.scm", // A bit too slow to enable by default
    "test/mceval.scm",
    "test/scp1/9.12.scm",
    "test/gabriel/browse.scm",
    "test/scp1/8.15.scm",
    "test/gambit/mazefun.scm",
    "test/gabriel/diviter.scm",
    "test/gabriel/divrec.scm",
    // "test/gambit/matrix.scm", // disabled because of vectors (vectors are fine, but equal? is not defined on them)
    "test/scp1/9.18.scm",
    "test/scp1/5.14.3.scm",
    "test/scp1/7.16.scm",
    "test/scp1/9.16.scm",
    "test/gabriel/destruc.scm",
    "test/gabriel/dderiv.scm",
    "test/scp1/7.11.scm",
    "test/scp1/7.13.scm",
    "test/scp1/5.22.scm",
    "test/scp1/7.14.scm",
    "test/scp1/7.4.scm", // named let
    "test/scp1/7.17.scm",
    "test/scp1/9.14.scm",
    "test/scp1/7.9.scm",
    //    "test/sigscheme/mem.scm",
    "test/scp1/7.15.scm",
    "test/sat.scm",
    "test/gabriel/deriv.scm",
    "test/sigscheme/takr.scm",
    "test/scp1/7.12.scm",
    "test/regex.scm",
    // "test/grid.scm", // vectors
    // "test/gabriel/puzzle.scm", // vectors
    "test/scp1/5.20.4.scm",
    "test/scp1/5.19.scm",
    "test/scp1/9.15.scm"
  )
}
