package scalaam.test

import scalaam.bench.scheme.SchemeBenchmarkPrograms
import scalaam.util.SmartUnion

trait SimpleBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarkPrograms.other)
}

trait RandomBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarkPrograms.selectRandomSeq(40))
}

trait SequentialBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarkPrograms.sequentialBenchmarks)
}

trait ThreadBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarkPrograms.threads)
}

trait ConcurrentBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarkPrograms.concurrentBenchmarks)
}

trait AllBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), SchemeBenchmarkPrograms.allBenchmarks)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

trait ConcurrentIncrementalBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), IncrementalBenchmarks.concurrent)
}

trait SequentialIncrementalBenchmarks extends SchemeBenchmarkTests {
  override def benchmarks(): Set[Benchmark] = SmartUnion.sunion(super.benchmarks(), IncrementalBenchmarks.sequential)
}

object IncrementalBenchmarks {
  lazy val concurrent: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/cscheme/threads",
    "mcarlo.scm")
  lazy val sequential: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme")
}