package scalaam.cli.experiments.incremental

object RunIncrementalExperiments {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModXPerformance.main(args)
    IncrementalSchemeModXPrecision.main(args)
    IncrementalSchemeModXProperties.main(args)
  }
}