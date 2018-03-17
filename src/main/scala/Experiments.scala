object ThreadExperiments {
  val benchFiles: List[(String, String)] = List(
    ("ABP", "threads/suite/abp.scm"),
    ("COUNT", "threads/suite/count.scm"),
    ("DEKKER", "threads/suite/dekker.scm"),
    ("FACT", "threads/suite/fact.scm"),
    ("MATMUL", "threads/suite/matmul.scm"),
    ("MCARLO", "threads/suite/mcarlo.scm"),
    ("MSORT", "threads/suite/msort.scm"),
    ("PC", "threads/suite/pc.scm"),
    ("PHIL", "threads/suite/phil.scm"),
    ("PHILD", "threads/suite/phild.scm"),
    ("PP", "threads/suite/pp.scm"),
    ("RINGBUF", "threads/suite/ringbuf.scm"),
    ("RNG", "threads/suite/rng.scm"),
    ("SUDOKU", "threads/suite/sudoku.scm"),
    ("TRAPR", "threads/suite/trapr.scm"),
    ("ATOMS", "threads/suite/atoms.scm"),
    ("STM", "threads/suite/stm.scm"),
    ("NBODY", "threads/suite/nbody.scm"),
    ("SIEVE", "threads/suite/sieve.scm"),
    ("CRYPT", "threads/suite/crypt.scm"),
    ("MCEVAL", "threads/suite/mceval.scm"),
    ("QSORT", "threads/suite/qsort.scm"),
    ("TSP", "threads/suite/tsp.scm"),
    ("BCHAIN", "threads/suite/bchain.scm"),
    ("LIFE", "threads/suite/life.scm"),
    ("PPS", "threads/suite/pps.scm"),
    ("MINIMAX", "threads/suite/minimax.scm"),
    ("ACTORS", "threads/suite/actors.scm")
  )
  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](args.size > 0)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, false, timeout))
    }

    if (args.size == 0) {
      val N = 20
      val warmup = 10
      for ((name, file) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")
        val times = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            run(file).time
          })
        val time = (times.drop(warmup).sum / N * 1000).toInt
        println(s"$time")
      }
    } else {
      val result = run(args(0))
      val graph = args(1)
      if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
      println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
      result.toFile(graph)(GraphDOTOutput)
    }
  }
}

object ThreadExperimentsMacrostepping {
  val benchFiles: List[(String, String)] = List(
    ("ABP", "threads/suite/abp.scm"),
    ("COUNT", "threads/suite/count.scm"),
    ("DEKKER", "threads/suite/dekker.scm"),
    ("FACT", "threads/suite/fact.scm"),
    ("MATMUL", "threads/suite/matmul.scm"),
    ("MCARLO", "threads/suite/mcarlo.scm"),
    ("MSORT", "threads/suite/msort.scm"),
    ("PC", "threads/suite/pc.scm"),
    ("PHIL", "threads/suite/phil.scm"),
    ("PHILD", "threads/suite/phild.scm"),
    ("PP", "threads/suite/pp.scm"),
    ("RINGBUF", "threads/suite/ringbuf.scm"),
    ("RNG", "threads/suite/rng.scm"),
    ("SUDOKU", "threads/suite/sudoku.scm"),
    ("TRAPR", "threads/suite/trapr.scm"),
    ("ATOMS", "threads/suite/atoms.scm"),
    ("STM", "threads/suite/stm.scm"),
    ("NBODY", "threads/suite/nbody.scm"),
    ("SIEVE", "threads/suite/sieve.scm"),
    ("CRYPT", "threads/suite/crypt.scm"),
    ("MCEVAL", "threads/suite/mceval.scm"),
    ("QSORT", "threads/suite/qsort.scm"),
    ("TSP", "threads/suite/tsp.scm"),
    ("BCHAIN", "threads/suite/bchain.scm"),
    ("LIFE", "threads/suite/life.scm"),
    ("PPS", "threads/suite/pps.scm"),
    ("MINIMAX", "threads/suite/minimax.scm"),
    ("ACTORS", "threads/suite/actors.scm")
  )
  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentAAMGS[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](Macrostepping, args.size > 0)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, timeout))
    }

    if (args.size == 0) {
      val N = 20
      val warmup = 2
      for ((name, file) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")
        val times = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            run(file).time
          })
        val time = (times.drop(warmup).sum / N * 1000).toInt
        println(s"$time")
      }
    } else {
      val result = run(args(0))
      val graph = args(1)
      if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
      println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
      result.toFile(graph)(GraphDOTOutput)
    }
  }
}

object ThreadExperimentsNaive {
  val benchFiles: List[(String, String)] = List(
    ("ABP", "threads/suite/abp.scm"),
    ("COUNT", "threads/suite/count.scm"),
    ("DEKKER", "threads/suite/dekker.scm"),
    ("FACT", "threads/suite/fact.scm"),
    ("MATMUL", "threads/suite/matmul.scm"),
    ("MCARLO", "threads/suite/mcarlo.scm"),
    ("MSORT", "threads/suite/msort.scm"),
    ("PC", "threads/suite/pc.scm"),
    ("PHIL", "threads/suite/phil.scm"),
    ("PHILD", "threads/suite/phild.scm"),
    ("PP", "threads/suite/pp.scm"),
    ("RINGBUF", "threads/suite/ringbuf.scm"),
    ("RNG", "threads/suite/rng.scm"),
    ("SUDOKU", "threads/suite/sudoku.scm"),
    ("TRAPR", "threads/suite/trapr.scm"),
    ("ATOMS", "threads/suite/atoms.scm"),
    ("STM", "threads/suite/stm.scm"),
    ("NBODY", "threads/suite/nbody.scm"),
    ("SIEVE", "threads/suite/sieve.scm"),
    ("CRYPT", "threads/suite/crypt.scm"),
    ("MCEVAL", "threads/suite/mceval.scm"),
    ("QSORT", "threads/suite/qsort.scm"),
    ("TSP", "threads/suite/tsp.scm"),
    ("BCHAIN", "threads/suite/bchain.scm"),
    ("LIFE", "threads/suite/life.scm"),
    ("PPS", "threads/suite/pps.scm"),
    ("MINIMAX", "threads/suite/minimax.scm"),
    ("ACTORS", "threads/suite/actors.scm")
  )
  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(150 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentAAMGS[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](AllInterleavings, args.size > 0)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, timeout))
    }

    if (args.size == 0) {
      val N = 1
      val warmup = 0
      for ((name, file) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")
        val times = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            run(file).time
          })
        val time = (times.drop(warmup).sum / N * 1000).toInt
        println(s"$time")
      }
    } else {
      val result = run(args(0))
      val graph = args(1)
      if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
      println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
      result.toFile(graph)(GraphDOTOutput)
    }
  }
}

object ThreadExperimentsScalabilityProcesses {
  import java.io._
  import java.nio.file.Files
  val MAX = 150
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(max: Int, processes: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    val name = "\"foo\""
    writer.write("(letrec ((f (lambda (n) n))\n")
    for (i <- 0 until max) {
      if (i <= processes) {
        writer.write(s"       (t$i (t/spawn $i))\n")
      } else {
        writer.write(s"       (t$i (+ $i))\n")
      }
    }
    writer.write(")\n")
    writer.write("(f 1))\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](false)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _

    val writer = new PrintWriter(new File("modular-scalability-t.dat"))
    val N = 10
    val warmup = 1
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"/tmp/p${i}.scm"
      gen(MAX, i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          (1 to N+warmup).foreach(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              run(program, false, timeout).time
            }
            if (i <= warmup) {} else {
              val itime = (time * 1000).toInt
              println(s"$sname | $itime")
              writer.write(s"$sname $itime\n")
            }
          })
        case None => ()
      }
      writer.flush
      // rm(file)
      System.gc()
    }
    writer.close
  }
}

object ThreadExperimentsScalabilityJoins {
  import java.io._
  import java.nio.file.Files
  val MAX = 150
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(max: Int, joins: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    val name = "\"foo\""
    writer.write("(letrec ((f (lambda (n) n))\n")
    for (i <- 0 until max+1) {
      if (i < joins) {
        writer.write(s"(t$i (t/spawn (t/join t${i+1})))")
      } else {
        writer.write(s"(t$i (t/spawn (+ ${i+1})))")
      }
    }
    writer.write(")\n")
    writer.write("1)\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](false)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _

    val writer = new PrintWriter(new File("modular-scalability-j.dat"))
    val N = 20
    val warmup = 10
    for (i <- 1 to MAX by 10) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"/tmp/p${i}.scm"
      gen(MAX, i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          val times: List[Double] = (1 to N+warmup).map(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              run(program, false, timeout).time
            }
            if (i <= warmup) {} else {
              val itime = (time * 1000).toInt
            println(s"$sname | $itime")
            writer.write(s"$sname $itime\n")
          }
            time
          }).toList
          val mean = (times.drop(warmup).sum / N * 1000).toInt
          println(s"MEAN: $mean")
        case None => ()
      }
      writer.flush
      // rm(file)
      System.gc()
    }
    writer.close
  }
}

object ThreadExperimentsScalabilityConflicts {
  import java.io._
  import java.nio.file.Files
  val MAX = 150
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(max: Int, joins: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    val name = "\"foo\""
    writer.write("(letrec ((x (t/ref 0))\n")
    for (i <- 0 until max) {
      if (i < joins) {
        writer.write(s"       (t$i (t/spawn (t/ref-set x (+ (t/deref x) 1))))\n")
      } else {
        writer.write(s"       (t$i (t/spawn (+ (+ (+ $i) 1))))\n")
      }
    }
    writer.write(")\n")
    for (i <- 0 until max) {
      writer.write(s"(t/join t$i)")
    }
    writer.write(")\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](false)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _

    val writer = new PrintWriter(new File("modular-scalability-c.dat"))
    val N = 20
    val warmup = 10
    for (i <- 1 to MAX by 10) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"/tmp/p${i}.scm"
      gen(MAX, i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          val times: List[Double] = (1 to N+warmup).map(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              run(program, false, timeout).time
            }
            if (i <= warmup) {} else {
              val itime = (time * 1000).toInt
              println(s"$sname | $itime")
              writer.write(s"$sname $itime\n")
            }
            time
          }).toList
          val mean = (times.drop(warmup).sum / N * 1000).toInt
          println(s"MEAN: $mean")
        case None => ()
      }
      writer.flush
      // rm(file)
      System.gc()
    }
    writer.close
  }
}

object ThreadExperimentsScalabilityProcessesMacrostepping {
  import java.io._
  import java.nio.file.Files
  val MAX = 10

  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentAAMGS[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](Macrostepping, false)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _

    val writer = new PrintWriter(new File("macrostep-scalability-t.dat"))
    val N = 20
    val warmup = 10
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"/tmp/p${i}.scm"
      ThreadExperimentsScalabilityProcesses.gen(MAX, i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          (1 to N+warmup).foreach(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              val res = run(program, false, timeout)
              if (res.timedOut) { println(s"TIMED OUT") }
              res.time
            }
            if (i <= warmup) {} else {
              val itime = (time * 1000).toInt
              println(s"$sname | $itime")
              writer.write(s"$sname $itime\n")
            }
          })
        case None => ()
      }
      writer.flush
      System.gc()
    }
    writer.close
  }
}

object ThreadExperimentsScalabilityJoinsMacrostepping {
  import java.io._
  import java.nio.file.Files
  val MAX = 10

  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentAAMGS[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](Macrostepping, false)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _

    val writer = new PrintWriter(new File("macrostep-scalability-j.dat"))
    val N = 3
    val warmup = 0
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"/tmp/p${i}.scm"
      ThreadExperimentsScalabilityJoins.gen(MAX, i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          val times: List[Double] = (1 to N+warmup).map(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              val res = run(program, false, timeout)
              if (res.timedOut) { println(s"TIMED OUT") }
              res.time
            }
            if (i <= warmup) {} else {
              val itime = (time * 1000).toInt
            println(s"$sname | $itime")
            writer.write(s"$sname $itime\n")
          }
            time
          }).toList
          val mean = (times.drop(warmup).sum / N * 1000).toInt
          println(s"MEAN: $mean")
        case None => ()
      }
      writer.flush
      // rm(file)
      System.gc()
    }
    writer.close
  }
}

object ThreadExperimentsScalabilityConflictsMacrostepping {
  import java.io._
  import java.nio.file.Files
  val MAX = 10

  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentAAMGS[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](Macrostepping, false)
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new CSchemePrimitives[ClassicalAddress.A, lat.L])

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _

    val writer = new PrintWriter(new File("macrostep-scalability-c.dat"))
    val N = 3
    val warmup = 0
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"/tmp/p${i}.scm"
      ThreadExperimentsScalabilityConflicts.gen(MAX, i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          val times: List[Double] = (1 to N+warmup).map(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              val res = run(program, false, timeout)
              if (res.timedOut) { println(s"TIMED OUT") }
              res.time
            }
            if (i <= warmup) {} else {
              val itime = (time * 1000).toInt
              println(s"$sname | $itime")
              writer.write(s"$sname $itime\n")
            }
            time
          }).toList
          val mean = (times.drop(warmup).sum / N * 1000).toInt
          println(s"MEAN: $mean")
        case None => ()
      }
      writer.flush
      // rm(file)
      System.gc()
    }
    writer.close
  }
}
