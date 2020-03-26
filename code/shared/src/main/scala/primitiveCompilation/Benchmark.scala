package scalaam.primitiveCompilation

import java.io._

import scalaam.io.Writer
import scalaam.io.Writer._
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme.CompoundSensitivities.S._
import scalaam.modular.scheme._
import scalaam.util.Metrics

object Benchmark extends App {

  trait Strategy
  case object Prelude extends Strategy
  case object Compile extends Strategy

  val strategies = List(Prelude, Compile)

  val warmup = 3
  val actual = 15

  setDefaultWriter(Writer.openTimeStamped("benchOutput/", "results.txt"))

  abstract class MainAnalysis(val pgm: SchemeExp, val strategy: Strategy) extends ModAnalysis(pgm) with BigStepSemantics with ConstantPropagationDomain with StandardSchemeModFSemantics {
    import scalaam.language.scheme.primitives._
    val primitives = if (strategy == Prelude) new SchemeLatticePrimitives[Value, Addr] else new CompiledSchemePrimitives[Value, Addr]

      def dump(suffix: String): Unit = {
        val file = new BufferedWriter(new FileWriter(new File(s"benchOutput/call/${strategy}_nonprims_$suffix")))
        file.write(allComponents.filter(cmp => componentName(cmp) match {
          case Some(name) => !PrimitiveDefinitions.definitions.keySet.contains(name) // ignore primitives
          case _ => true
        })
        .map(cmp => s"$cmp: ${try{store(ReturnAddr(cmp))}catch {case e: Throwable => "_?_"}}").toList.sorted.mkString("\n"))
        file.flush()
        file.close()
      }
  }

  def newAnalysis(pgm: SchemeExp, strategy: Strategy, s: S): MainAnalysis = s match {
    case S_0_0     => new MainAnalysis(pgm, strategy) with CompoundSensitivities.S_0_0
    case S_CS_0    => new MainAnalysis(pgm, strategy) with CompoundSensitivities.S_CS_0
    case S_CS_CS   => new MainAnalysis(pgm, strategy) with CompoundSensitivities.S_CS_CS
    case S_FA_0    => new MainAnalysis(pgm, strategy) with CompoundSensitivities.S_FA_0
    case S_FA_CS   => new MainAnalysis(pgm, strategy) with CompoundSensitivities.S_FA_CS
    case S_CSFA_0  => new MainAnalysis(pgm, strategy) with CompoundSensitivities.S_CSFA_0
    case S_CSFA_CS => new MainAnalysis(pgm, strategy) with CompoundSensitivities.S_CSFA_CS
  }

  def run(file: String, se: S, s: Strategy = Prelude, timing: Boolean = true): (Long, Double) = {
    System.gc()
    writeln(s"[$file] ")
    val program = if (s == Prelude) PrimitiveDefinitions.parseWithPrelude(file) else PrimitiveDefinitions.parseWithoutPrelude(file)
    val suffix = file.replaceAll("/", "_").replaceAll(".scm", ".txt")

    var time: Long = 0
    var card: Double = 0.0

    if (!timing) {
      val analysis = newAnalysis(program, s, se)
      analysis.initPrimitiveBenchmarks()
      System.gc() // Can be removed.
        analysis.analyze()
      def lower(addr: analysis.Addr): Option[String] = addr match {
        case cmpAddr: analysis.ComponentAddr =>
          cmpAddr.addr match {
            case _: analysis.PrmAddr => None
            case p: analysis.PtrAddr[_] => Some(p.toString)
            case v: analysis.VarAddr => Some(v.id.fullString)
          }
        case retAddr: analysis.ReturnAddr =>
          retAddr.cmp match {
            case _: analysis.MainComponent => Some("cmp<main>")
            case c: analysis.CallComponent => Some(s"cmp<${c.clo._1.idn.pos}>")
          }
        case _ => ???
      }
      val finalStore: Map[String, analysis.Value] = analysis.store.toList.foldLeft(Map[String, analysis.Value]())((acc, av) => av match {
        case (addr, v) => lower(addr) match {
          case Some(k) => if (acc.contains(k)) {
            acc + (k -> analysis.lattice.join(acc(k), v))
          } else {
            acc + (k -> v)
          }
          case None => acc
        }
      })
      for ((k, v) <- finalStore.toList.sortBy(_._1)) {
        if (analysis.lattice.cardinality(v) == scalaam.core.CardinalityNumber(1) || analysis.lattice.cardinality(v) == scalaam.core.CardinalityNumber(3)) {
          println(s"$k: ${analysis.lattice.cardinality(v)} -> $v")
        }
      }
      val cardinalities: List[(Int, Int)] = finalStore.values.map(analysis.lattice.cardinality).groupBy(_.n).view.mapValues(_.size).toList.sortBy(_._1) // TODO: this might not be the most efficient line of code.
      writeln("* Cardinalities:")
      cardinalities.foreach({case (c, n) => writeln(s"  * $c: $n")})
      val filtered = (if (cardinalities.head._1 == -1) cardinalities.tail else cardinalities).map(t => (t._1.toLong, t._2.toLong))
      writeln(s"  -> Counted ${cardinalities.foldLeft(0)((acc, kv) => acc + kv._2)} values.")
      card = Metrics.weightedAverage(filtered.map(_.swap))
      writeln(s"  -> Weighted avg. fin. members: $card")
    }

    if (timing) {
      var times: List[Long] = List()

      // Warmup.
      write("* Warmup - ")
      for (i <- 0 until warmup) {
        write(i + " ")
        // TODO: Add System.gc() here?
        newAnalysis(program, s, se).analyze()
      }

      InterceptCall.init() // Can also use analysis.initPrimitiveBenchmarks.

      // Time measurements.
      write("* Time - ")
      for (i <- 1 to actual) {
        write(i + " ")
        val analysis = newAnalysis(program, s, se)
        System.gc()
        val t0 = System.nanoTime()
        analysis.analyze()
        val t1 = System.nanoTime()
        times = (t1 - t0) :: times
      }

      val m = Metrics.all(times)
      writeln(s"\n      Mean time: ${m.mea / 1000000}ms")
      writeln(s"      Min  time: ${m.min / 1000000}ms")
      writeln(s"      Max  time: ${m.max / 1000000}ms")
      writeln(s"      Med  time: ${m.med / 1000000}ms")
      writeln(s"         Stddev: ${m.std / 1000000}ms")
      time = (InterceptCall.primTime / 1000000) / actual
      writeln(s"   Avg primtime: $time ms")
    }
    (time, card)
  }

  val allbench: List[String] = List(
//    "test/mceval.scm",
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
    //"test/WeiChenRompf2019/kcfa-worst-case-256.scm", // Doesn't use primitives but can give an insight on timing variations (e.g. 71ms vs 68ms).
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
    "test/scp1/9.15.scm"
  )

  def measure(time: Boolean, bench: List[String] = allbench.reverse, s: List[S] = CompoundSensitivities.sensitivities, st: List[Strategy] = strategies): Unit = {
    bench.foreach{ b =>
      s.foreach{ s =>
        st.foreach { st =>
          writeln(s"***** $st / $s *****")
          run(b, s, st, time)
        }
      }
    }
  }

  def time(bench: List[String] = allbench.reverse, s: List[S] = CompoundSensitivities.sensitivities, st: List[Strategy] = strategies): Unit = measure(true, bench, s ,st)
  def precision(bench: List[String] = allbench.reverse, s: List[S] = CompoundSensitivities.sensitivities, st: List[Strategy] = strategies): Unit = measure(false, bench, s ,st)

  /*
  def testConsistency(): Unit = {
    def testOne(file: String): Unit = {
      write(s"Checking consistency for $file: ")
      val card = run(file, S_0_0, Prelude, false)._2
      for (_ <- 1 to 9) {
        write("* ")
        val c = run(file, S_0_0, Prelude, false)._2
        if (c != card) {
          writeln("Test failed.")
          return
        }
      }
      writeln("ok.")
    }
    allbench.reverse.foreach(testOne)
  }
  */

  //time()
  List("test/scp1/5.19.scm").foreach(b => {
    precision(List(b), s = List(S_0_0, S_CS_CS, S_CSFA_CS), st = List(Prelude))
  })

  closeDefaultWriter()
}
