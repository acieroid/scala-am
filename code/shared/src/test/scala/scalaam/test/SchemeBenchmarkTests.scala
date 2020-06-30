package scalaam.test

import org.scalatest.propspec.AnyPropSpec

trait SchemeBenchmarkTests extends AnyPropSpec {
  // A benchmark is just a file name.
  type Benchmark = String
  // The benchmarks involved in the tests. Needs to be overridden later.
  def benchmarks(): Set[Benchmark] = Set.empty

  // Needs to be implemented to specify the testing behaviour per benchmark.
  protected def onBenchmark(b: Benchmark): Unit
  // Run the benchmarks.
  benchmarks().foreach(onBenchmark)
}
