package scalaam.bench.scheme

object IncrementalSchemeBenchmarkPrograms {
  lazy val threads: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/cscheme/threads",
    "puzzle.scm",  // Needs call-with-current-continuation.
  )
  lazy val concurrent: Set[String] = threads
  lazy val sequential: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme",
    "icp_4_qeval_nodup.scm", // define-syntax
    "icp_7_eceval_compile_open_coded.scm" // must still fit this into the compiler... :(
  )
}