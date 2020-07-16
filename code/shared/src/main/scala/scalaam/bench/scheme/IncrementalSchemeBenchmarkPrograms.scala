package scalaam.bench.scheme

object IncrementalSchemeBenchmarkPrograms {
  lazy val threads: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/cscheme/threads",
    "mcarlo.scm")
  lazy val sequential: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme")
}
