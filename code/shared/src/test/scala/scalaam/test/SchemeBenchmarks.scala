package scalaam.test

import java.io.File

import scalaam.util.SmartUnion
import scala.util.Random

trait SimpleBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarks.other)
}

trait RandomBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarks.selectRandomSeq(40))
}

trait SequentialBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarks.sequentialBenchmarks)
}


trait ThreadBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarks.threads)
}

trait ConcurrentBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarks.concurrentBenchmarks)
}

trait AllBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarks.allBenchmarks)
}

object SchemeBenchmarks {

  // Just include an entire folder of programs, except some that are explicitly specified. The directory name should not end with a "/".
  def fromFolderR(directory: String, exclude: String*): Set[String] = {
    def recur(dir: File): Array[File] = {
      val here = dir.listFiles()
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
    root.listFiles().filter(!_.isDirectory).map(_.getAbsolutePath.substring(base)).toSet -- exclude.map(file => s"$directory/$file")
  }

  lazy val ad: Set[String] = fromFolderR("test/R5RS/ad",
    "bfirst.scm", // Unbound identifier: create-graph
    "btree.scm", // Lacks a body.
    "README"
  )

  lazy val gabriel: Set[String] = fromFolderR("test/R5RS/gabriel", "README")

  lazy val gambit: Set[String] = fromFolderR("test/R5RS/gambit",
    "README",
    "cat.scm", // Needs open-input-file.
    "compiler.scm", // Parser error (see issue on Github).
    "ctak.scm", // Needs call-with-current-continuation.
    "fibc.scm", // Needs call-cc.
    "peval.scm", // Error in program.
    "puzzle.scm",  // Needs call-with-current-continuation.
    "scheme.scm", // Error in program.
    "string.scm", // Needs susbtring.
    "tail.scm", // Needs file manipulation primitives (open-input-port, close-input-port, read-char).
    "trav1.scm", // Needs append.
    "wc.scm", // Needs file manipulation primitives (open-input-port, close-input-port, read-char).
  )

  lazy val rosetta: Set[String] = fromFolderR("test/R5RS/rosetta")

  lazy val scp1: Set[String] = fromFolderR("test/R5RS/scp1",
    "circus.scm", // Vararg append nog supported by concrete interpreter.
    "README"
  )

  lazy val scp1_compressed: Set[String] = fromFolderR("test/R5RS/scp1-compressed",
    "README"
  )

  lazy val scp1_singleFile: Set[String] = Set("test/R5RS/scp1-compressed/all.scm")

  lazy val SICP: Set[String] = fromFolder("test/R5RS/icp",
    "icp_4_qeval.scm", // Needs define-syntax and delay.
  )

  lazy val sigscheme: Set[String] = fromFolderR("test/R5RS/sigscheme", "README")

  lazy val theLittleSchemer: Set[String] = fromFolderR("test/R5RS/WeiChenRompf2019/the-little-schemer",
    "ch4.scm", // No main code (only definitions).
    "ch5.scm", // No main code (only definitions).
    "ch6.scm", // Commented out half of the file. Now does not parse anymore.
    "ch7.scm", // No main code (only definitions).
    "ch9.scm", // Unbound identifier: will-stop?
    "README.md"
  )

  lazy val toplas98: Set[String] = fromFolder("test/R5RS/WeiChenRompf2019/toplas98",
    "boyer.sch", // Uses square brackets.
    "dynamic.scm", // Uses call-with-input-file
    "graphs.scm", // Uses open-input-file.
    "handle.scm", // Uses defmacro (not standard r5rs).
    "lattice.scm", // Parser error. Uses undefined (void) function.
    "lattice-processed.scm", // Parser error. Uses undefined (void) function.
    "maze.scm", // Uses open-input-file.
    "nbody.scm", // Parser error.
    "nbody-processed.scm", // Parser error.
    "nucleic.sch", // Uses square brackets.
    "nucleic2.sch", // USES macros (define-syntax).
    "splay.scm", // Uses () instead of '(), but has other issues.
    "README.md"
  )

  lazy val WCR2019: Set[String] = fromFolder("test/R5RS/WeiChenRompf2019",
    "church_exp.sch", // Uses non-standard (void) function.
    "earley.sch", // Uses read.
    "mbrotZ.sch", // Parser error.
    "meta-circ.scm", // Uses procedure?
    "regex-derivative.scm", // Parser error.
    "scheme2java.scm", // Uses char-alphabetic?
    "solovay-strassen.scm", // Program seems erroneous.
    "README.md",
    ".DS_Store"
  )

  lazy val other: Set[String] = fromFolder("test/R5RS",
    "test/R5RS/quasiquoting.scm", // Uses unquote-splicing.
    "test/R5RS/scm2c.scm", // Uses string->list.
    "test/R5RS/scm2java.scm", // Uses list->string.
    "test/R5RS/Streams.scm", // Uses define-macro.
  )

  lazy val     WeiChenRompf2019: Set[String] = SmartUnion.sunionList(List(theLittleSchemer, toplas98, WCR2019))
  lazy val sequentialBenchmarks: Set[String] = SmartUnion.sunionList(List(ad, gabriel, gambit, rosetta, scp1, SICP, sigscheme, WeiChenRompf2019, other))

  def selectRandomSeq(n: Int): Set[String] = Random.shuffle(sequentialBenchmarks).take(n)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val savina: Set[String] = fromFolderR(    "test/concurrentScheme/actors/savina")

  lazy val soter: Set[String] = fromFolderR("test/concurrentScheme/actors/soter")

  lazy val actors: Set[String] = savina ++ soter ++ fromFolder("test/concurrentScheme/actors", "README")

  lazy val futures: Set[String] = fromFolderR("test/concurrentScheme/futures", "README.org")

  lazy val futuresVariations: Set[String] = fromFolderR(    "test/concurrentScheme/futures/variations")

  lazy val threads: Set[String] = fromFolderR("test/concurrentScheme/threads",
    "abp.scm", // Unbound reference: display-recorded.
    "lastzero2.scm", // Uses let*, but should use something like letrec*?
    "phild.scm", // Unbound reference: bool-top
  )

  lazy val threadsVariations: Set[String] = fromFolderR("test/concurrentScheme/threads/variations")

  lazy val concurrentBenchmarks: Set[String] = SmartUnion.sunionList(List(actors, futures, futuresVariations, savina, soter, threads, threadsVariations))

  def selectRandomPar(n: Int): Set[String] = Random.shuffle(concurrentBenchmarks).take(n)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val allBenchmarks: Set[String] = SmartUnion.sunion(concurrentBenchmarks, sequentialBenchmarks)
}

/*

  ***** SCP1 programs per topic. *****

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

*/