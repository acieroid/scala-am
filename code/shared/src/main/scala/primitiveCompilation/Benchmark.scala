package scalaam.primitiveCompilation

import java.io.{BufferedWriter, File, FileWriter}

import scalaam.io.Writer
import scalaam.io.Writer.{closeDefaultWriter, setDefaultWriter, write, writeln}
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme._
import scalaam.primitiveCompilation.{Definitions, SchemeBenchmarks}

object Benchmark extends App {

  trait Strategy
  case object Prelude extends Strategy
  case object Compile extends Strategy

  val warmup = 3
  val actual = 15

  setDefaultWriter(Writer.openTimeStamped("benchOutput/", "results.txt"))

  def run(file: String, s: Strategy = Prelude, timing: Boolean = true): Unit = {
    System.gc()
    writeln(s"[$file] ")
    val program = if (s == Prelude) Definitions.parseWithPrelude(file) else Definitions.parseWithoutPrelude(file)
    val suffix = file.replaceAll("/", "_").replaceAll(".scm", ".txt")

    if (timing) {
      // Warmup.
      write("* Warmup - ")
      for (i <- 0 until warmup) {
        write(i + " ")
        val analysis = new ModAnalysis(program) with BigStepSemantics with ConstantPropagationDomain with PrimitiveSensitivity with StandardSchemeModFSemantics {

          import scalaam.language.scheme.primitives._

          val primitives = if (s == Prelude) new SchemeLatticePrimitives[Value, Addr] else new CompiledSchemePrimitives[Value, Addr]
        }
        analysis.analyze()
      }

    }

    var time = 0L

    // Get results for each call (but timing results are kept for next iteration).
    writeln("\n* Calls + Time 0")
    val analysis = new ModAnalysis(program) with BigStepSemantics with ConstantPropagationDomain with PrimitiveSensitivity with StandardSchemeModFSemantics {
      import scalaam.language.scheme.primitives._
      val primitives = if (s == Prelude) new SchemeLatticePrimitives[Value, Addr] else new CompiledSchemePrimitives[Value, Addr]

      def dump(suffix: String): Unit = {
        val file = new BufferedWriter(new FileWriter(new File(s"benchOutput/call/${s}_nonprims_$suffix")))
        file.write(allComponents.filter(cmp => componentName(cmp) match {
          case Some(name) => !Definitions.primitiveDefinitions.keySet.contains(name) // ignore primitives
          case _ => true
        })
        .map(cmp => s"$cmp: ${try{store(ReturnAddr(cmp))}catch {case e: Throwable => "_?_"}}").toList.sorted.mkString("\n"))
        file.flush()
        file.close()
      }
    }

    analysis.initPrimitiveBenchmarks()
    System.gc()
    val t0 = System.nanoTime()
    analysis.analyze()
    val t1 = System.nanoTime()
    time += (t1 - t0)
    //analysis.callToFile(s + "_" + suffix)

    if (!timing) analysis.dump(suffix)

    if (timing) {
      // Time measurements.
      write("* Time - ")
      for (i <- 1 until actual) {
        write(i + " ")
        val analysis = new ModAnalysis(program) with BigStepSemantics with ConstantPropagationDomain with PrimitiveSensitivity with StandardSchemeModFSemantics {

          import scalaam.language.scheme.primitives._

          val primitives = if (s == Prelude) new SchemeLatticePrimitives[Value, Addr] else new CompiledSchemePrimitives[Value, Addr]
        }
        System.gc()
        val t0 = System.nanoTime()
        analysis.analyze()
        val t1 = System.nanoTime()
        time += (t1 - t0)
      }
      writeln(s"\n   Average time: ${(time / 1000000) / actual}ms")
      writeln(s"   Primitive time: ${(InterceptCall.primTime / 1000000) / actual}ms")
    }

    //analysis.timeToFile(s + "_" + suffix)
  }

  def gabriel(): Unit = {
    SchemeBenchmarks.gabriel.foreach(b => run(b))
  }

  val allbench: List[String] = List(
    "test/mceval.scm",
    "test/scp1/9.12.scm",
    "test/gabriel/browse.scm",
    "test/scp1/8.15.scm",
    "test/gambit/mazefun.scm",
    "test/gabriel/diviter.scm",
    "test/gabriel/divrec.scm",
    // "test/gambit/matrix.scm", // disabled because of vectors
    "test/scp1/9.18.scm",
    "test/scp1/5.14.3.scm",
    "test/scp1/7.16.scm",
    "test/scp1/9.16.scm",
    "test/gambit/destruc.scm",
    "test/gabriel/destruc.scm",
    "test/gabriel/dderiv.scm",
    "test/scp1/7.11.scm",
    "test/scp1/7.13.scm",
    "test/scp1/5.22.scm",
    "test/scp1/7.14.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-256.scm",
    "test/scp1/7.4.scm",
    "test/scp1/7.17.scm",
    "test/scp1/9.14.scm",
    "test/scp1/7.9.scm",
    //    "test/sigscheme/mem.scm",
    "test/scp1/7.15.scm",
    "test/sat.scm",
    "test/gabriel/deriv.scm",
    "test/sigscheme/takr.scm",
    "test/scp1/7.12.scm",
    "test/regex.scm",
    "test/grid.scm",
    "test/gabriel/puzzle.scm",
    "test/scp1/5.20.4.scm",
    "test/scp1/5.19.scm",
    "test/scp1/9.15.scm")

  def timeAll(s: Strategy = Prelude): Unit = {
    allbench.reverse.foreach(b => {
      writeln("***** Prelude *****")
      run(b, Prelude)
      writeln("***** Compile *****")
      run(b, Compile)
    })
  }

  def precAll(s: Strategy = Prelude): Unit = {
    allbench.reverse.foreach(run(_, s, false))
  }

  def time(): Unit = {
    timeAll()
  }

  def precision(bench: List[String] = allbench): Unit = {
    bench.foreach({b =>
      run(b, Prelude, false)
      run(b, Compile, false)
    })
  }

  time()

  closeDefaultWriter()
}
