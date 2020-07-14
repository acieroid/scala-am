package scalaam.cli.experiments

// TODO: this is duplicated code from the test suite -> centralize all benchmarks listings in a shared package

import java.io.File
import scala.util.Random

object SchemeBenchmarks {

  def files(dir: File): Array[File] = {
    val lst = dir.listFiles()
    if (lst == null) Array()
    else lst
  }

  // Just include an entire folder of programs, except some that are explicitly specified. The directory name should not end with a "/".
  def fromFolderR(directory: String, exclude: String*): Set[String] = {
    def recur(dir: File): Array[File] = {
      val here = files(dir)
      val (dr, noDr) = here.partition(_.isDirectory)
      noDr ++ dr.flatMap(recur)
    }
    val root = new File(directory)
    val base = root.getAbsolutePath.length - directory.length
    recur(root).map(_.getAbsolutePath.substring(base)).toSet -- exclude.map(file => s"$directory/$file")
  }

  def fromFolder(directory: String, exclude: String*): Set[String] = {
    val root = new File(directory)
    val base = root.getAbsolutePath.length - directory.length
    files(root).filter(!_.isDirectory).map(_.getAbsolutePath.substring(base)).toSet -- exclude.map(file => s"$directory/$file")
  }

    // A full up-to-date list of benchmarks is present in scalaam.test.SchemeBenchmarks.

    val standard = Set(
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
        //"test/R5RS/grid.scm",
        "test/R5RS/inc.scm",
        "test/R5RS/infinite-1.scm",
        //"test/R5RS/infinite-2.scm",
        //"test/R5RS/infinite-3.scm",
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
        //"test/R5RS/primtest.scm",
        //"test/R5RS/quasiquoting-simple.scm",
        //"test/R5RS/quasiquoting.scm",
        "test/R5RS/regex.scm", 
        "test/R5RS/rotate.scm",
        "test/R5RS/rsa.scm",
        "test/R5RS/sat.scm",
        //"test/R5RS/scm2c.scm",     // various unsupported primitives
        //"test/R5RS/scm2java.scm",  // various unsupported primitives
        "test/R5RS/sq.scm",
        //"test/R5RS/Streams.scm",   // define-macro
        "test/R5RS/sym.scm",
        "test/R5RS/widen.scm",
        "test/R5RS/work.scm"
    )

    val scp1_compressed: List[String] = List(
      "test/R5RS/scp1-compressed/2.scm",
      "test/R5RS/scp1-compressed/3.scm",
      "test/R5RS/scp1-compressed/4.scm",
      "test/R5RS/scp1-compressed/5.scm",
      "test/R5RS/scp1-compressed/7.scm",
      "test/R5RS/scp1-compressed/8.scm",
      "test/R5RS/scp1-compressed/9.scm",
    )

    val gabriel = List(
      "test/R5RS/gabriel/browse.scm",
      "test/R5RS/gabriel/puzzle.scm",
      "test/R5RS/gabriel/dderiv.scm",
      "test/R5RS/gabriel/destruc.scm",
      "test/R5RS/gabriel/deriv.scm",
      "test/R5RS/gabriel/diviter.scm",
      "test/R5RS/gabriel/divrec.scm",
    )

    val gambit = List(
      "test/R5RS/gambit/scheme.scm",
      "test/R5RS/gambit/sboyer.scm",
      "test/R5RS/gambit/nboyer.scm",
      "test/R5RS/gambit/matrix.scm",
      "test/R5RS/gambit/peval.scm",
      "test/R5RS/gambit/earley.scm", // list->vector
      "test/R5RS/gambit/graphs.scm",
      "test/R5RS/gambit/lattice.scm",
      "test/R5RS/gambit/mazefun.scm",
      // "test/R5RS/gambit/browse.scm", // part of gabriel
      "test/R5RS/gambit/paraffins.scm",
      // "test/R5RS/gambit/puzzle.scm",
      // "test/R5RS/gambit/trav1.scm", // append? or dot notation
      "test/R5RS/gambit/perm9.scm",
      // "test/R5RS/gambit/destruc.scm", // part of gabriel
      "test/R5RS/gambit/triangl.scm", // list->vector
      "test/R5RS/gambit/nqueens.scm",
      "test/R5RS/gambit/primes.scm",
      "test/R5RS/gambit/sumloop.scm",
    )


  val ad = List(
    // "test/R5RS/ad/bfirst.scm", // incorrect benchmark
    // "test/R5RS/ad/bst.scm", // only definitions
    // "test/R5RS/ad/btree.scm", // only definitions
    "test/R5RS/ad/dict.scm",
    "test/R5RS/ad/prioq.scm",
    // "test/R5RS/ad/queue.scm", // only definitions
    "test/R5RS/ad/quick.scm",
    // "test/R5RS/ad/RBtreeADT.scm", // dot notation?
    "test/R5RS/ad/stack.scm",
    // "test/R5RS/stspaceCODE.scm" // only definitions
    "test/R5RS/ad/abstrct.scm",
    "test/R5RS/ad/bubsort.scm",
    "test/R5RS/ad/heap.scm",
    "test/R5RS/ad/inssort.scm",
    "test/R5RS/ad/linear.scm",
    "test/R5RS/ad/list.scm",
    "test/R5RS/ad/mesort.scm",
    "test/R5RS/ad/qsort.scm",
    "test/R5RS/ad/qstand.scm",
    "test/R5RS/ad/selsort.scm",
  )


  val icp = List(
    "test/R5RS/icp/icp_1c_ontleed.scm", // too slow
    // "test/R5RS/icp/icp_1c_multiple-dwelling.scm", // stack overflows the concrete interpreter
    "test/R5RS/icp/icp_1c_prime-sum-pair.scm", // too slow
    "test/R5RS/icp/icp_2_aeval.scm",
    "test/R5RS/icp/icp_3_leval.scm",
    // "test/R5RS/icp/icp_4_qeval.scm", // define-syntax, apply, eval
    "test/R5RS/icp/icp_5_regsim.scm",
    "test/R5RS/icp/icp_6_stopandcopy_scheme", // vectors
    "test/R5RS/icp/icp_7_eceval.scm", // too slow
    "test/R5RS/icp/icp_8_compiler.scm"
  )

}
