package scalaam.test.modular.scheme.incremental

import org.scalatest.Tag
import scalaam.language.CScheme.CSchemeParser
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.modular.incremental.scheme.AnalysisBuilder._
import scalaam.modular.scheme._
import scalaam.test._
import scalaam.test.modular.scheme.SchemeSoundnessTests
import scalaam.util.Reader
import scalaam.util.benchmarks.Timeout

trait IncrementalModXSoundnessTests extends SchemeSoundnessTests {

  type IncrementalAnalysis = ModAnalysis[SchemeExp] with GlobalStore[SchemeExp]
                                                    with ReturnValue[SchemeExp]
                                                    with SchemeDomain
                                                    with IncrementalModAnalysis[SchemeExp]
                                                    
  override def analysis(b: SchemeExp): IncrementalAnalysis

  private var version: Version = Old

  override def runInterpreter(i: SchemeInterpreter, p: SchemeExp, t: Timeout.T): SchemeInterpreter.Value = i.run(p, t, version)

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Incremental (re-)analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = CSchemeParser.parse(content)

      // Check soundness on the original version of the program.
      version = Old
      val (cResultOld, cPosResultsOld) = evalConcrete(program,benchmark)
      val anlOld = runAnalysis(program,benchmark)
      compareResult(anlOld, cResultOld)
      compareIdentities(anlOld, cPosResultsOld)

      // Check soundness on the updated version of the program.
      version = New
      val (cResultNew, cPosResultsNew) = evalConcrete(program,benchmark)
      val anlNew = updateAnalysis(program,benchmark)
      compareResult(anlNew, cResultNew)
      compareIdentities(anlNew, cPosResultsNew)
    }

   private def updateAnalysis(program: SchemeExp, benchmark: Benchmark): IncrementalAnalysis =
    try {
      val anl: IncrementalAnalysis = analysis(program)
      val timeout = analysisTimeout(benchmark)
      anl.updateAnalysis(timeout)
      assume(anl.finished(), "Reanalysis timed out.")
      anl
    } catch {
      case e: VirtualMachineError =>
        System.gc()
        cancel(s"Reanalysis of $benchmark encountered an error: $e.")
    }

  override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ IncrementalTest
}

class IncrementalSmallStepModConc extends IncrementalModXSoundnessTests with ConcurrentIncrementalBenchmarks {
  def name = "Incremental ModConc soundness test"
  override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalModConcAnalysis(b)
  override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ SchemeModConcTest :+ SmallStepTest
  override def isSlow(b: Benchmark): Boolean =
    Set(
      "test/changes/cscheme/threads/actors.scm",
      "test/changes/cscheme/threads/crypt.scm",
      "test/changes/cscheme/threads/crypt2.scm",
      "test/changes/cscheme/threads/stm.scm",
    )(b)
}

class IncrementalModF extends IncrementalModXSoundnessTests with SequentialIncrementalBenchmarks {
  def name = "Incremental ModF soundness test"
  override def analysis(b: SchemeExp): IncrementalAnalysis = new IncrementalSchemeModFAnalysis(b)
  override def testTags(b: Benchmark): Seq[Tag] = super.testTags(b) :+ SchemeModFTest :+ BigStepTest
  override def isSlow(b: Benchmark): Boolean =
    Set(
      "test/changes/scheme/icp_1c_multiple-dwelling-coarse.scm",
      "test/changes/scheme/icp_1c_multiple-dwelling-fine.scm",
      "test/changes/scheme/icp_3_leval_ex_5.scm",
      "test/changes/scheme/icp_7_8_open_coded.scm",
      "test/changes/scheme/mceval-dynamic.scm",
      "test/changes/scheme/nboyer.scm",
      "test/changes/scheme/peval.scm",
    )(b)
}