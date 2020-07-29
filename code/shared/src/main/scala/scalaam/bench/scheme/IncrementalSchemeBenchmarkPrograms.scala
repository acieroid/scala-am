package scalaam.bench.scheme

object IncrementalSchemeBenchmarkPrograms {
  lazy val threads: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/cscheme/threads",
    "puzzle.scm",  // Needs call-with-current-continuation.
  )
  lazy val concurrent: Set[String] = threads
  lazy val sequential: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme",
    "icp_4_qeval_nodup.scm", // define-syntax, force, delay
    "scheme.scm" // error in program
    // TODO "icp_7_8_open_coded_incorrect.scm" contains "errors" within the program.
  )
}