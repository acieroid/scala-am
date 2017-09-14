object ThreadExperiments {
  val benchFiles: List[(String, String)] = List()
  def main(args: Array[String]): Unit = {
    val lat = new MakeCSchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isCSchemeLattice = lat.isCSchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val machine = new ConcurrentModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID]
    // val sem = new ASchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L])
    val sem = new CSchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L])

    def run(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, timeout))
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
