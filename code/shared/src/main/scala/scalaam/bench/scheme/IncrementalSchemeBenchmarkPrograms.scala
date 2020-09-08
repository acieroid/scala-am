package scalaam.bench.scheme

object IncrementalSchemeBenchmarkPrograms {
  lazy val threads: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/cscheme/threads",
    "puzzle.scm",  // Needs call-with-current-continuation.
    ".DS_Store",
  )
  lazy val concurrent: Set[String] = threads
  lazy val sequential: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme",
    "icp_4_qeval_nodup.scm", // define-syntax, force, delay
    "scheme.scm", // error in program
    ".DS_Store",
  )
}