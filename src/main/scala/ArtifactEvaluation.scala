import Util._
import ScalaAM._
import scala.concurrent.duration.Duration

object ArtifactEvaluation {
  /* The benchmark names and their corresponding file */
  val benchFiles: Map[String, String] = Map(
    ("pp", "actors/savina/pp.scm"),
    ("count", "actors/savina/count.scm"),
    ("count-seq", "actors/savina/count-seq.scm"),
    ("fjt-seq", "actors/savina/fjt-seq.scm"),
    ("fjc-seq", "actors/savina/fjc-seq.scm"),
    ("factorial", "actors/factorial.scm"),
    ("stack", "actors/stack.scm"),
    ("cell", "actors/cell.scm"),
    ("parikh", "actors/soter/parikh.scm"),
    ("pipe-seq", "actors/soter/pipe-seq.scm"),
    ("unsafe-send", "actors/soter/unsafe_send.scm"),
    ("safe-send", "actors/soter/safe_send.scm"),
    ("state-factory", "actors/soter/state_factory.scm"),
    ("stutter", "actors/soter/stutter.scm"))

  /* Timeout of 60 seconds (in nanoseconds) */
  val timeout: Duration = Duration(60, "seconds")

  def timesize(bench: String, mbox: String, bound: Int): (Int, Int) = {
    val counting = false /* no abstract counting */
    val lattice = new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](counting) /* constant propagation lattice */
    implicit val isSchemeLattice = lattice.isSchemeLattice
    val alattice = new MakeASchemeLattice[lattice.L]
    implicit val isASchemeLattice = alattice.isASchemeLattice

    import ActorTimestamp.fromTime
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isTimestamp = time.isActorTimestamp

    val address: AddressWrapper = ClassicalAddress
    implicit val isAddress = address.isAddress

    val mboxImpl = mbox match {
      case "PS" => new PowersetMboxImpl[ContextSensitiveTID, alattice.L]
      case "MS" => new BoundedMultisetMboxImpl[ContextSensitiveTID, alattice.L](bound)
      case "L" => new BoundedListMboxImpl[ContextSensitiveTID, alattice.L](bound)
      case "G" => new GraphMboxImpl[ContextSensitiveTID, alattice.L]
    }
    val machine = new ActorsAAMGlobalStore[SchemeExp, alattice.L, address.A, time.T, ContextSensitiveTID](mboxImpl)

    val visitor = new EmptyActorVisitor[SchemeExp, alattice.L, address.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[alattice.L, address.A, time.T, ContextSensitiveTID](new SchemePrimitives[address.A, alattice.L], visitor)
    val N = 10
    val warmup = 2 // 2 runs that are ignored to warm up
    val (states, times) = (1 to N+warmup).map(i =>
      runOnFile(bench, program => {
        val res = scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
          machine.eval(sem.parse(program), sem, false, Timeout.start(Some(timeout.toNanos)))
        }
        (res.numberOfStates, if (res.timedOut) { -0.001 /* negative number indicates a timeout */ } else { res.time })
      })).unzip
    (states.head, ((times.drop(warmup).sum * 1000) / N).toInt) /* back to ms for the paper */
  }

  def timesize(): Unit = {
    val benchs = List(
      ("pp", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("count", List(("PS", 0), ("MS", 1), ("G", 0))),
      ("count-seq", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0))),
      ("fjt-seq", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("fjc-seq", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("factorial", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("stack", List(("PS", 0), ("MS", 1), ("L", 4), ("G", 0))),
      ("cell", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0))),
      ("parikh", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0))),
      ("pipe-seq", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("unsafe-send", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("safe-send", List(("PS", 0), ("MS", 1), ("L", 4), ("G", 0))),
      ("state-factory", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("stutter", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0)))
    )
    for ((name, cfgs) <- benchs) {
      val sname = name.padTo(15, " ").mkString
      print(s"$sname | ")
      for ((mb, b) <- cfgs) {
        val (s, t) = timesize(benchFiles(name), mb, b)
        print(s"$s, $t | ")
      }
      println("")
    }
  }

  def errors(bench: String, mbox: String, bound: Int): Int = {
    val counting = false /* no abstract counting */
    val lattice = new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](counting) /* constant propagation lattice */
    implicit val isSchemeLattice = lattice.isSchemeLattice
    val alattice = new MakeASchemeLattice[lattice.L]
    implicit val isASchemeLattice = alattice.isASchemeLattice

    import ActorTimestamp.fromTime
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isTimestamp = time.isActorTimestamp

    val address: AddressWrapper = ClassicalAddress
    implicit val isAddress = address.isAddress

    val mboxImpl = mbox match {
      case "PS" => new PowersetMboxImpl[ContextSensitiveTID, alattice.L]
      case "MS" => new BoundedMultisetMboxImpl[ContextSensitiveTID, alattice.L](bound)
      case "L" => new BoundedListMboxImpl[ContextSensitiveTID, alattice.L](bound)
      case "G" => new GraphMboxImpl[ContextSensitiveTID, alattice.L]
    }
    val machine = new ActorsAAMGlobalStore[SchemeExp, alattice.L, address.A, time.T, ContextSensitiveTID](mboxImpl)

    val visitor = new RecordActorVisitor[SchemeExp, alattice.L, address.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[alattice.L, address.A, time.T, ContextSensitiveTID](new SchemePrimitives[address.A, alattice.L], visitor)
    runOnFile(bench, program => {
      scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
        machine.eval(sem.parse(program), sem, false, Timeout.start(Some(timeout.toNanos)))
      }
    })
    visitor.errors.size
  }
  def errors(): Unit = {
    val benchs = List(
      ("parikh", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0))),
      ("unsafe-send", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("safe-send", List(("PS", 0), ("MS", 1), ("L", 4), ("G", 0))),
      ("stutter", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0))),
      ("stack", List(("PS", 0), ("MS", 1), ("L", 4), ("G", 0))),
      ("count-seq", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0))),
      ("cell", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0)))
    )
    for ((name, cfgs) <- benchs) {
      val sname = name.padTo(15, " ").mkString
      print(s"$sname | ")
      for ((mb, b) <- cfgs) {
        val (res) = errors(benchFiles(name), mb, b)
        print(s"$res | ")
      }
      println("")
    }
  }

  def bounds(bench: String, mbox: String, bound: Int): List[(String, MboxSize)] = {
    val counting = false /* no abstract counting */
    val lattice = new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](counting) /* constant propagation lattice */
    implicit val isSchemeLattice = lattice.isSchemeLattice
    val alattice = new MakeASchemeLattice[lattice.L]
    implicit val isASchemeLattice = alattice.isASchemeLattice

    import ActorTimestamp.fromTime
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isTimestamp = time.isActorTimestamp

    val address: AddressWrapper = ClassicalAddress
    implicit val isAddress = address.isAddress

    val mboxImpl = mbox match {
      case "PS" => new PowersetMboxImpl[ContextSensitiveTID, alattice.L]
      case "MS" => new BoundedMultisetMboxImpl[ContextSensitiveTID, alattice.L](bound)
      case "L" => new BoundedListMboxImpl[ContextSensitiveTID, alattice.L](bound)
      case "G" => new GraphMboxImpl[ContextSensitiveTID, alattice.L]
    }
    val machine = new ActorsAAMGlobalStore[SchemeExp, alattice.L, address.A, time.T, ContextSensitiveTID](mboxImpl)

    val visitor = new RecordActorVisitor[SchemeExp, alattice.L, address.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[alattice.L, address.A, time.T, ContextSensitiveTID](new SchemePrimitives[address.A, alattice.L], visitor)
    runOnFile(bench, program => {
      scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
        machine.eval(sem.parse(program), sem, false, Timeout.start(Some(timeout.toNanos))).asInstanceOf[machine.ActorsAAMOutput].bounds.toList.map({
          case (k, v) => (k.toString, v)
        })
      }
    })
  }

  def bounds(): Unit = {
    val benchs = List(
      ("pipe-seq", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0)), List(("pipe-node", 1))),
      ("state-factory", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0)), List(("state-actor", 1))),
      ("pp", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0)), List(("ping-actor", 1), ("pong-actor", 1))),
      ("count-seq", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0)), List(("producer-actor", 1), ("counting-actor", 2))),
      ("cell", List(("PS", 0), ("MS", 1), ("L", 2), ("G", 0)), List(("cell", 2), ("display-actor", 1))),
      ("fjc-seq", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0)), List(("forkjoin-actor", 1))),
      ("fjt-seq", List(("PS", 0), ("MS", 1), ("L", 1), ("G", 0)), List(("throughput-actor", 1)))
    )
    for ((name, cfgs, boundsToCheck) <- benchs) {
      val sname = name.padTo(15, " ").mkString
      print(s"$sname | ")
      for ((mb, b) <- cfgs) {
        val bs = bounds(benchFiles(name), mb, b)
        var verified = true
        for ((actorToCheck, boundToCheck) <- boundsToCheck) {
          bs.foreach({ case (actor, bound) =>
            if (actor.startsWith(actorToCheck) && bound > MboxSizeN(boundToCheck) ) {
              verified = false
            }
          })
        }
        val res = if (verified) "v" else "x"
        print(s"$res | ")
      }
      println("")
    }
  }
  def main(args: Array[String]) {
    if (args.size < 1) {
      println("Please provide one of the following command: timesize, bounds")
    } else {
      args(0) match {
        case "timesize" => timesize()
        case "errors" => errors()
        case "bounds" => bounds()
      }
    }
  }
}
