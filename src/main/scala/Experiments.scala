object ActorExperiments {
  val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
  val timeout: Option[Long] = None // Some(1000000000)
  implicit val isASchemeLattice = lat.isASchemeLattice
  val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
  implicit val isActorTimestamp = time.isActorTimestamp
  //val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
  val mbox = new BoundedListMboxImpl[ContextSensitiveTID, lat.L](1)
  //val mbox = new GraphMboxImpl[ContextSensitiveTID, lat.L]
  // val machine = new ActorsAAMGlobalStoreUnboundedActors[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox)
  // val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox)
  val machine = new ActorsModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID]
  // val sem = new ASchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L])
  val visitor = new RecordActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
  val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

  def run(file: String) = {
    Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, timeout))
  }

  val benchFiles: Map[String, String] = Map(
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
