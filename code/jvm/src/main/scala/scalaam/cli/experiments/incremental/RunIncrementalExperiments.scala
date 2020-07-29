package scalaam.cli.experiments.incremental

object RunIncrementalExperiments {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModXPerformance.main(args)
    IncrementalSchemeModXPrecision.main(args)
    IncrementalSchemeModConcProperties.main(args)
  }
}

object RunIncrementalExperimentsChooseBenchmarks {
  val benchmarks: Array[String] = Array("test/changes/scheme/icp_1c_multiple-dwelling.scm")
  def main(args: Array[String]): Unit = {
    RunIncrementalExperiments.main(benchmarks)
  }
}