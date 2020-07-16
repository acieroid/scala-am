package scalaam.bench.scheme

object IncrementalSchemeBenchmarkPrograms {
  lazy val threads: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/cscheme/threads")
  lazy val concurrent: Set[String] = threads
  lazy val sequential: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme")
}
