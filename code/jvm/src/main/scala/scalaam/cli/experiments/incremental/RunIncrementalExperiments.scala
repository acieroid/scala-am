package scalaam.cli.experiments.incremental

object RunIncrementalExperiments {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModXPerformance.main(Array())
    IncrementalSchemeModXPrecision.main(Array())
    IncrementalSchemeModConcProperties.main(Array())
  }
}
