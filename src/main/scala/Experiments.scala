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
  def main(args: Array[String]): Unit = {
    val result = run(args(0))
    val graph = args(1)
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
    result.toFile(graph)(GraphDOTOutput)
  }
  // TODO: instead of using mailbox as actor context, use set of sent messages, or k last messages
  // why? Because, with powerset we seem to maintain a low state space overhead of using an actor context, but the others (L1, G) have a much bigger overhead
}
