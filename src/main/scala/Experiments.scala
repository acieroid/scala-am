object ExperimentsConfig {
  val timeout: Option[Long] = Some(1800 * 1e9.toLong)
  val warmupRuns: Int = 0
  val nruns: Int = 1
}

object ActorExperimentsModular {
  val benchFiles: List[(String, String)] = List(
    ("PP", "actors/savina/pp.scm"),
    ("COUNT", "actors/savina/count.scm"),
    ("FJT", "actors/savina/fjt.scm"),
    ("FJC", "actors/savina/fjc.scm"),
    ("THR", "actors/savina/thr.scm"),
    ("CHAM", "actors/savina/cham.scm"),
    ("BIG", "actors/savina/big.scm"),
    ("CDICT", "actors/savina/cdict.scm"),
    ("CSLL", "actors/savina/csll.scm"),
    ("PCBB", "actors/savina/pcbb.scm"),
    ("PHIL", "actors/savina/phil.scm"),
    ("SBAR", "actors/savina/sbar.scm"),
    ("CIG", "actors/savina/cig.scm"),
    ("LOGM", "actors/savina/logm.scm"),
    ("BTX", "actors/savina/btx.scm"),
    ("RSORT", "actors/savina/rsort.scm"),
    ("FBANK", "actors/savina/fbank.scm"),
    ("SIEVE", "actors/savina/sieve.scm"),
    ("UCT", "actors/savina/uct.scm"),
    ("OFL", "actors/savina/ofl.scm"),
    ("TRAPR", "actors/savina/trapr.scm"),
    ("PIPREC", "actors/savina/piprec.scm"),
    ("RMM", "actors/savina/rmm.scm"),
    ("QSORT", "actors/savina/qsort.scm"),
    ("APSP", "actors/savina/apsp.scm"),
    ("SOR", "actors/savina/sor.scm"),
    ("ASTAR", "actors/savina/astar.scm"),
    ("NQN", "actors/savina/nqn.scm"))
  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    // val mbox = new BoundedListMboxImpl[ContextSensitiveTID, lat.L](1)
    //val mbox = new GraphMboxImpl[ContextSensitiveTID, lat.L]
    // val machine = new ActorsAAMGlobalStoreUnboundedActors[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox)
    // val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox)
    val machine = new ActorsModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](false) // args.size > 0)
    // val sem = new ASchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L])
    // val visitor = new RecordActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, ExperimentsConfig.timeout))
    }

    if (args.size == 0) {
      val N = ExperimentsConfig.nruns
      val warmup = ExperimentsConfig.warmupRuns
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

object ActorExperimentsMacrostepping {
  val benchFiles: List[(String, String)] = List(
    ("PP", "actors/savina/pp.scm"),
    ("COUNT", "actors/savina/count.scm"),
    ("FJT", "actors/savina/fjt.scm"),
    ("FJC", "actors/savina/fjc.scm"),
    ("THR", "actors/savina/thr.scm"),
    ("CHAM", "actors/savina/cham.scm"),
    ("BIG", "actors/savina/big.scm"),
    ("CDICT", "actors/savina/cdict.scm"),
    ("CSLL", "actors/savina/csll.scm"),
    ("PCBB", "actors/savina/pcbb.scm"),
    ("PHIL", "actors/savina/phil.scm"),
    ("SBAR", "actors/savina/sbar.scm"),
    ("CIG", "actors/savina/cig.scm"),
    ("LOGM", "actors/savina/logm.scm"),
    ("BTX", "actors/savina/btx.scm"),
    ("RSORT", "actors/savina/rsort.scm"),
    ("FBANK", "actors/savina/fbank.scm"),
    ("SIEVE", "actors/savina/sieve.scm"),
    ("UCT", "actors/savina/uct.scm"),
    ("OFL", "actors/savina/ofl.scm"),
    ("TRAPR", "actors/savina/trapr.scm"),
    ("PIPREC", "actors/savina/piprec.scm"),
    ("RMM", "actors/savina/rmm.scm"),
    ("QSORT", "actors/savina/qsort.scm"),
    ("APSP", "actors/savina/apsp.scm"),
    ("SOR", "actors/savina/sor.scm"),
    ("ASTAR", "actors/savina/astar.scm"),
    ("NQN", "actors/savina/nqn.scm")
  )
  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox, args.size > 0, ActorMacrostepping)
    val visitor = new RecordActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, ExperimentsConfig.timeout))
    }

    if (args.size == 0) {
      val N = ExperimentsConfig.nruns
      val warmup = ExperimentsConfig.warmupRuns
      for ((name, file) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")
        val times = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            run(file).time
          }
        )
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

object ActorExperimentsAllInterleavings {
  val benchFiles: List[(String, String)] = List(
    ("PP", "actors/savina/pp.scm"),
    ("COUNT", "actors/savina/count.scm"),
    ("FJT", "actors/savina/fjt.scm"),
    ("FJC", "actors/savina/fjc.scm"),
    ("THR", "actors/savina/thr.scm"),
    ("CHAM", "actors/savina/cham.scm"),
    ("BIG", "actors/savina/big.scm"),
    ("CDICT", "actors/savina/cdict.scm"),
    ("CSLL", "actors/savina/csll.scm"),
    ("PCBB", "actors/savina/pcbb.scm"),
    ("PHIL", "actors/savina/phil.scm"),
    ("SBAR", "actors/savina/sbar.scm"),
    ("CIG", "actors/savina/cig.scm"),
    ("LOGM", "actors/savina/logm.scm"),
    ("BTX", "actors/savina/btx.scm"),
    ("RSORT", "actors/savina/rsort.scm"),
    ("FBANK", "actors/savina/fbank.scm"),
    ("SIEVE", "actors/savina/sieve.scm"),
    ("UCT", "actors/savina/uct.scm"),
    ("OFL", "actors/savina/ofl.scm"),
    ("TRAPR", "actors/savina/trapr.scm"),
    ("PIPREC", "actors/savina/piprec.scm"),
    ("RMM", "actors/savina/rmm.scm"),
    ("QSORT", "actors/savina/qsort.scm"),
    ("APSP", "actors/savina/apsp.scm"),
    ("SOR", "actors/savina/sor.scm"),
    ("ASTAR", "actors/savina/astar.scm"),
    ("NQN", "actors/savina/nqn.scm")
  )
  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox, args.size > 0, ActorAllInterleavings)
    val visitor = new RecordActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, ExperimentsConfig.timeout))
    }

    if (args.size == 0) {
      val N = ExperimentsConfig.nruns
      val warmup = ExperimentsConfig.warmupRuns
      for ((name, file) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")
        val times = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            run(file).time
          }
        )
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

object ActorExperimentsScalability1 {
  import java.io._
  import java.nio.file.Files
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(behaviors: Int, selectorsPerBeh: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    writer.write("(letrec (\n")
    for (i <- 0 until behaviors) {
      val name = "\"b" ++ i.toString ++ "\""
      val next = (i + 1) % behaviors
      writer.write(s"  (b$i (a/actor $name ()\n")
      for (j <- 0 until selectorsPerBeh) {
        writer.write(s"      (m$j () (let ((p$next (a/create b$next)))\n")
        for (k <- 0 until selectorsPerBeh) {
          writer.write(s"        (a/send p$next m$k)\n")
        }
        writer.write(s"      (a/become b$i)))\n")
      }
      writer.write("     ))\n")
    }
    writer.write("  )\n")
    for (j <- 0 until selectorsPerBeh) {
      writer.write(s"(a/send (a/create b0) m$j)\n")
    }
    writer.write(")\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](false) // args.size > 0)
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, timeout))
    }

    val N = 20
    val warmup = 10
    for (i <- 1 to 10) {
      for (j <- 1 to 10) {
        val sname = (i.toString ++ " " ++ j.toString).padTo(10, " ").mkString
        val file = s"actors/scalability/b${i}m${j}.scm"
        gen(i, j, file)

        print(s"$i $j | ")
        val times = (1 to N+warmup).map(i => {
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            run(file).time
          }
        })
        val time = (times.drop(warmup).sum / N * 1000).toInt
        println(time)
        rm(file)
      }
    }
  }
}

object ActorExperimentsScalabilityMessages {
  import java.io._
  import java.nio.file.Files
  val MAX = 150
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(selectors: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    val name = "\"foo\""
    writer.write(s"(letrec ((b (a/actor $name ()\n")
    for (i <- 0 until MAX) {
      writer.write(s"              (m$i ()(a/become b))\n")
    }
    writer.write("  )) (p (a/create b)))\n")
    for (i <- 0 until MAX) {
      writer.write(s"  (a/send p m${i % selectors})")
    }
    writer.write("  )\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](false) // args.size > 0)
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _


    val writer = new PrintWriter(new File("scalability.dat"))
    val N = 20
    val warmup = 10
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"actors/scalability/b1m${i}.scm"
      gen(i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          (1 to N+warmup).foreach(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              run(program, true, timeout).time
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
      rm(file)
      System.gc()
    }
    writer.close
  }
}

object ActorExperimentsScalabilityBehaviors {
  import java.io._
  import java.nio.file.Files
  val MAX = 150
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(behaviors: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    writer.write(s"(letrec (")
    for (i <- 0 until MAX) {
      val name = "\"foo\""
      writer.write(s"  (b$i (a/actor $name ()\n")
      writer.write(s"         (m () (a/become b))))\n")
    }
    writer.write("  )")
    for (i <- 0 until MAX) {
      writer.write(s"(a/send (a/create b${i % behaviors}) m)")
    }
    writer.write("  )\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](false) // args.size > 0)
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _


    val writer = new PrintWriter(new File("scalability.dat"))
    val N = 20
    val warmup = 10
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"actors/scalability/b${i}m${i}.scm"
      gen(i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          (1 to N+warmup).foreach(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              run(program, true, timeout).time
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
      rm(file)
      System.gc()
    }
    writer.close
  }
}

object ActorExperimentsScalabilityMessagesMacrostepping {
  import java.io._
  import java.nio.file.Files
  val MAX = 20
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(selectors: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    val name = "\"foo\""
    writer.write(s"(letrec ((b (a/actor $name ()\n")
    for (i <- 0 until MAX) {
      writer.write(s"              (m$i ()(a/become b))\n")
    }
    writer.write("  )) (p (a/create b)))\n")
    for (i <- 0 until MAX) {
      writer.write(s"  (a/send p m${i % selectors})")
    }
    writer.write("  )\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox, false, ActorMacrostepping)
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _

    val writer = new PrintWriter(new File("scalability.dat"))
    val N = 20
    val warmup = 10
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"actors/scalability/b1m${i}.scm"
      gen(i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          (1 to N+warmup).foreach(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              run(program, true, timeout).time
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
      rm(file)
      System.gc()
    }
    writer.close
  }
}

object ActorExperimentsScalabilityBehaviorsMacrostepping {
  import java.io._
  import java.nio.file.Files
  val MAX = 20
  def rm(file: String) = {
    val f = new File(file)
    f.delete()
  }
  def gen(behaviors: Int, file: String) = {
    val writer = new PrintWriter(new File(file))
    writer.write(s"(letrec (")
    for (i <- 0 until MAX) {
      val name = "\"foo\""
      writer.write(s"  (b$i (a/actor $name ()\n")
      writer.write(s"         (m () (a/become b))))\n")
    }
    writer.write("  )")
    for (i <- 0 until MAX) {
      writer.write(s"(a/send (a/create b${i % behaviors}) m)")
    }
    writer.write("  )\n")
    writer.close
  }

  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox, false, ActorMacrostepping)
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    val run = ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem) _


    val writer = new PrintWriter(new File("scalability.dat"))
    val N = 20
    val warmup = 10
    for (i <- 1 to MAX) {
      val sname = (i.toString).padTo(10, " ").mkString
      val file = s"actors/scalability/b${i}m${i}.scm"
      gen(i, file)

      Util.fileContent(file) match {
        case Some(program) =>
          (1 to N+warmup).foreach(i => {
            val time = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
              run(program, true, timeout).time
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
      rm(file)
      System.gc()
    }
    writer.close
  }
}
