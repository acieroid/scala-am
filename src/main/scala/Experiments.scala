object ExperimentsConfig {
  val timeout: Option[Long] = Some(1800 * 1e9.toLong)
  val warmupRuns: Int = 0
  val nruns: Int = 1
}

object Recorder {
  import scala.collection.mutable.Map

  object Typ extends Enumeration {
    type Typ = Value
    val String, Boolean, Symbol, Character, Closure, Int, Real, Vector, Cons, Nil = Value
  }
  import Typ._

  val types: Map[Identifier, Set[Typ]] = Map[Identifier, Set[Typ]]().withDefaultValue(Set[Typ]())
  val typesRet: Map[SchemeExp, Set[Typ]] = Map[SchemeExp, Set[Typ]]().withDefaultValue(Set[Typ]())
  val numClo: Map[SchemeExp, Int] = Map[SchemeExp, Int]().withDefaultValue(0)

  def closures[Exp](site: Exp, num: Int): Unit = {
    numClo += site.asInstanceOf[SchemeExp] -> numClo(site.asInstanceOf[SchemeExp]).max(num)
  }
  def identifier(id: Identifier, ts: Set[Typ]): Unit = {
    types += id -> (types(id) ++ ts)
  }
  def funRet[Exp](clo: Exp, ts: Set[Typ]): Unit = {
    typesRet += clo.asInstanceOf[SchemeExp] -> (typesRet(clo.asInstanceOf[SchemeExp]) ++ ts)
  }

  def print: Unit = {
    println("================")
    println("Number of closures:")
    numClo.foreach({ case (k, v) => {
      val ks = s"${k}:${k.pos}"
      println(s"$ks: $v")
    }})
    println("================")
    println("Types:")
    types.foreach({ case (k, v) => {
      val ks = s"${k.name}:${k.pos}"
      val vs = v.mkString(",")
      println(s"$ks: $vs")
    }})
    println("================")
    println("Return types:")
    typesRet.foreach({ case (k, v) => {
      val ks = s"${k}:${k.pos}"
      val vs = v.mkString(",")
      println(s"$ks: $vs")
    }})
  }

  def extract: (scala.collection.immutable.Map[String, Set[Typ]], scala.collection.immutable.Map[String, Set[Typ]], scala.collection.immutable.Map[String, Int]) = {
    (types.map({ case (k: Identifier, v: Set[Typ]) => (s"$k:${k.pos}", v) }).toMap,
      typesRet.map({ case (k: SchemeExp, v: Set[Typ]) => (s"$k:${Expression[SchemeExp].pos(k)}", v) }).toMap,
      numClo.map({ case (k: SchemeExp, v: Int) => (s"$k:${Expression[SchemeExp].pos(k)}", v) }).toMap)
  }

  def clear(): Unit = {
    types.clear
    typesRet.clear
    numClo.clear
  }
}

object Precision {
  /* Return the number of over-approximations performed by the less precise map */
  def compareNumClo[A](less: Map[A, Int], more: Map[A, Int]): Int = {
//    println(s"less - more = ${less.keySet -- more.keySet}, more - less = ${more.keySet -- less.keySet}")
    if (!more.keySet.subsetOf(less.keySet)) {
      println(s"!!!!!!! Not the same keys: less - more = ${less.keySet -- more.keySet}, more - less = ${more.keySet -- less.keySet}")
      -1
    } else {
      less.keySet.foldLeft(0)({ case (acc, k) =>
        if (more.get(k) == None) { acc /* + less(k) */ } else {
        val (x, y) = (less(k), more(k))
        if (x > y) {
          /* Over-approximation */
          acc + (x - y)
        } else if (x == y) {
          /* Full precision */
          acc
        } else {
          /* Under-approximation!? */
          println(s"!!!!UNDER-APPROX FOR $k: less is $x, more is $y")
          acc
        }
      }})
    }
  }

  def numCloSize[A](m: Map[A, Int]): Int =
    m.keySet.foldLeft(0)({ case (acc, k) => acc + m(k) })
  def typSize[A, B](m: Map[A, Set[B]]): Int =
    m.keySet.foldLeft(0)({ case (acc, k) => acc + m(k).size })

  /* Similar, return number of over-approxs */
  def compareTyp[A, B](less: Map[A, Set[B]], more: Map[A, Set[B]]): Int = {
//    println(s"less - more = ${less.keySet -- more.keySet}, more - less = ${more.keySet -- less.keySet}")
    if (!more.keySet.subsetOf(less.keySet)) {
      println(s"!!!!!!! Not the same keys: less - more = ${less.keySet -- more.keySet}, more - less = ${more.keySet -- less.keySet}")
      -1
    } else {
      less.keySet.foldLeft(0)({ case (acc, k) =>
        if (more.get(k) == None) { acc /* + less(k).size */ } else {
        val (x, y) = (less(k), more(k))
        if (x.subsetOf(y) || y.subsetOf(x)) {
          if (x.size > y.size) {
            /* Over-approximation */
            acc + (x.size - y.size)
          } else if (x.size == y.size) {
            /* Full precision */
            acc
          } else {
            /* Under-approximation!? */
            println(s"!!!!UNDER-APPROX FOR $k: less is $x, more is $y")
            acc
          }
        } else {
          println(s"INCOMPATIBLE SETS for $k: less is $x, more is $y")
          acc
        }
      }})
    }

  }
}



object ModFExperiments {
  val benchFiles: List[(String, String)] = List(
   //    ("foo", "foo.scm")
   //     ("boyer", "test/gabriel/boyer.scm"), too slow
       ("cpstak", "test/gabriel/cpstak.scm"),
       ("dderiv", "test/gabriel/dderiv.scm"),
       ("deriv", "test/gabriel/deriv.scm"),
//       ("divrec", "test/gabriel/divrec.scm"),
//       ("takl", "test/gabriel/takl.scm"),
//       ("ack", "test/kernighanvanwyk/ack.scm"),
       ("nqueens", "test/gambit/nqueens.scm"),
   //    ("array1", "test/gambit/array1.scm"), // do
   //    ("destruc", "test/gambit/destruc.scm"), // do
   //    ("diviter", "test/gambit/diviter.scm"), // do
   //    ("graphs", "test/gambit/graphs.scm"), // do
       ("lattice", "test/gambit/lattice.scm"),
       ("mazefun", "test/gambit/mazefun.scm"),
       ("paraffins", "test/gambit/paraffins.scm"),
//       ("perm9", "test/gambit/perm9.scm"), do
   //    ("peval", "test/gambit/peval.scm"), // too slow
//       ("primes", "test/gambit/primes.scm"),
//       ("sum", "test/gambit/sum.scm"),
//       ("sumloop", "test/gambit/sumloop.scm"),
//       ("tak", "test/gambit/tak.scm"),
  //     ("easter", "test/rosetta/easter.scm"),
//       ("quadratic", "test/rosetta/quadratic.scm"),
//       ("blur", "test/blur.scm"),
//       ("collatz", "test/collatz.scm"),
//       ("eta", "test/eta.scm"),
//       ("gcipd", "test/gcipd.scm"),
       ("grid", "test/grid.scm"),
//       ("kcfa2", "test/kcfa2.scm"),
//       ("kcfa3", "test/kcfa3.scm"),
//       ("loop2", "test/loop2.scm"),
//       ("mj09", "test/mj09.scm"),
//       ("primtest", "test/primtest.scm"),
       ("regex", "test/regex.scm"),
//       ("rsa", "test/rsa.scm"),
//       ("sat", "test/sat.scm"),
       ("scm2java", "test/scm2java.scm"),
//       ("mceval", "test/mceval.scm"),
    ("church", "test/church.scm"),
    ("mem", "test/sigscheme/mem.scm"),
    ("7.14", "test/scp1/7.14.scm"),
    ("7.16", "test/scp1/7.16.scm"),
//    ("8.15", "test/scp1/8.15.scm"),
    ("9.16", "test/scp1/9.16.scm"),
//    ("takr", "test/sigscheme/takr.scm"),
    ("5.14.3", "test/scp1/5.14.3.scm"),
    ("9.12", "test/scp1/9.12.scm")
  )
  def main(args: Array[String]): Unit = {
    val lattice = new MakeSchemeLattice[Type.S, Concrete.B, Type.I, Type.F, Type.C, Type.Sym](false)
    implicit val isSchemeLattice = lattice.isSchemeLattice
    val address = ClassicalAddress
    implicit val isAddress = address.isAddress
    val time = ZeroCFA
    implicit val isTimestamp = time.isTimestamp
    val modularmachine = new ModularAAM[SchemeExp, lattice.L, address.A, time.T]
    val nonmodularmachine = new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](AAMKAlloc)
    val sem = new SchemeSemantics[lattice.L, address.A, time.T](new SchemePrimitives[address.A, lattice.L])

    val concLattice = new MakeSchemeLattice[Concrete.S, Concrete.B, Concrete.I, Concrete.F, Concrete.C, Concrete.Sym](true)
    implicit val isConcSchemeLattice = concLattice.isSchemeLattice
    val concTime = ConcreteTimestamp
    implicit val isConcreteTimestamp = concTime.isTimestamp
    val concretemachine = new ConcreteMachine[SchemeExp, concLattice.L, address.A, concTime.T]
    val concsem = new SchemeSemantics[concLattice.L, address.A, concTime.T](new SchemePrimitives[address.A, concLattice.L])

    def runMod(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lattice.L, address.A, time.T](modularmachine, sem)(_, true, ExperimentsConfig.timeout))
    }
    def runNonMod(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, lattice.L, address.A, time.T](nonmodularmachine, sem)(_, true, ExperimentsConfig.timeout))
    }
    def runConc(file: String) = {
      Util.runOnFile(file, ScalaAM.run[SchemeExp, concLattice.L, address.A, concTime.T](concretemachine, concsem)(_, true, ExperimentsConfig.timeout))
    }


    if (args.size == 0) {
      val N = ExperimentsConfig.nruns
      val warmup = ExperimentsConfig.warmupRuns
      for ((name, file) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")
        val times = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            runMod(file).time
          })
        val time = (times.drop(warmup).sum / N * 1000).toInt
          print(s"MOD: $time | ")
          val modinfo = Recorder.extract
          Recorder.clear

        val times2 = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            runNonMod(file).time
          })
        val time2 = (times2.drop(warmup).sum / N * 1000).toInt
          print(s"NONMOD: $time2 | ")
          val nonmodinfo = Recorder.extract
          Recorder.clear

          val times3 = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            runConc(file).time
          })

        val time3 = (times3.drop(warmup).sum / N * 1000).toInt
          println(s"CONC: $time3")
          val concinfo = Recorder.extract
          Recorder.clear


          println(s"Observed: ${Precision.typSize(concinfo._1)} and ${Precision.numCloSize(concinfo._3)}")


          val imprec1mod = Precision.compareTyp(modinfo._1, concinfo._1)
          val imprec3mod = Precision.compareNumClo(modinfo._3, concinfo._3)
          if (imprec1mod > 0 || imprec3mod > 0) { println(s"Imprecision mod: $imprec1mod $imprec3mod") }

          val imprec1nonmod = Precision.compareTyp(nonmodinfo._1, concinfo._1)
          val imprec3nonmod = Precision.compareNumClo(nonmodinfo._3, concinfo._3)
          if (imprec1nonmod > 0 || imprec3nonmod > 0) { println(s"Imprecision nonmod: $imprec1nonmod $imprec3nonmod") }

                   //          println(s"2: ${Precision.compareTyp(modinfo._2, nonmodinfo._2)}")
//          println(s"MOD vs. CONC:")
//          val imprec1Precision.compareTyp(modinfo._1, concinfo._1)}")
//          println(s"2: ${Precision.compareTyp(modinfo._2, concinfo._2)}")
//          println(s"3: ${Precision.compareNumClo(modinfo._3, concinfo._3)}")
//          println(s"NONMOD vs. CONC:")
//          println(s"1: ${Precision.compareTyp(nonmodinfo._1, concinfo._1)}")
//          println(s"2: ${Precision.compareTyp(nonmodinfo._2, concinfo._2)}")
//          println(s"3: ${Precision.compareNumClo(nonmodinfo._3, concinfo._3)}")
      }
    } else {
      val result = runMod(args(0))
      val graph = args(1)
      if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
      println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
      result.toFile(graph)(GraphDOTOutput)
      Recorder.print
    }

  }
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
    val machine = new ActorsModular[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](args.size > 0)
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
     ("COUNT-SEQ", "actors/savina/count-seq.scm"),
     ("FJT-SEQ", "actors/savina/fjt-seq.scm"),
     ("FJC-SEQ", "actors/savina/fjc-seq.scm"),
     ("FACTORIAL", "actors/factorial.scm"),
     ("STACK", "actors/stack.scm"),
     ("CELL", "actors/cell.scm"),
     ("PARIKH", "actors/soter/parikh.scm"),
     ("PIPE-SEQ", "actors/soter/pipe-seq.scm"),
     ("SAFE-SEND", "actors/soter/safe_send.scm"),
     ("STATE-FACTORY", "actors/soter/state_factory.scm"),
     ("STUTTER", "actors/soter/stutter.scm"))
/*
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
  )*/
  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox, args.size > 0, ActorMacrostepping)
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

object ActorExperimentsMacrosteppingBoundedMultiset {
   val benchFiles: List[(String, String, Int)] = List(
     ("PP", "actors/savina/pp.scm", 1),
     ("COUNT-SEQ", "actors/savina/count-seq.scm", 1),
     ("FJT-SEQ", "actors/savina/fjt-seq.scm", 1),
     ("FJC-SEQ", "actors/savina/fjc-seq.scm", 1),
     ("FACTORIAL", "actors/factorial.scm", 1),
     ("STACK", "actors/stack.scm", 1),
     ("CELL", "actors/cell.scm", 1),
     ("PARIKH", "actors/soter/parikh.scm", 1),
     ("PIPE-SEQ", "actors/soter/pipe-seq.scm", 1),
     ("SAFE-SEND", "actors/soter/safe_send.scm", 1),
     ("STATE-FACTORY", "actors/soter/state_factory.scm", 1),
     ("STUTTER", "actors/soter/stutter.scm", 1))

/*  val benchFiles: List[(String, String, Int)] = List(
//      ("PP", "actors/savina/pp.scm", 1),
//      ("COUNT", "actors/savina/count.scm", 1),
//      ("FJT", "actors/savina/fjt.scm", 1),
//      ("FJC", "actors/savina/fjc.scm", 1),
//      ("THR", "actors/savina/thr.scm", 1),
//     ("CHAM", "actors/savina/cham.scm", 1),
     ("BIG", "actors/savina/big.scm", 1),
//     ("CDICT", "actors/savina/cdict.scm", 1),
//     ("CSLL", "actors/savina/csll.scm", 1),
//     ("PCBB", "actors/savina/pcbb.scm", 1),
//     ("PHIL", "actors/savina/phil.scm", 1),
     ("SBAR", "actors/savina/sbar.scm", 1),
     ("CIG", "actors/savina/cig.scm", 1),
     ("LOGM", "actors/savina/logm.scm", 1),
     ("BTX", "actors/savina/btx.scm", 1),
     ("RSORT", "actors/savina/rsort.scm", 1),
     ("FBANK", "actors/savina/fbank.scm", 1),
     ("SIEVE", "actors/savina/sieve.scm", 1),
     ("UCT", "actors/savina/uct.scm", 1),
     ("OFL", "actors/savina/ofl.scm", 1),
     ("TRAPR", "actors/savina/trapr.scm", 1),
     ("PIPREC", "actors/savina/piprec.scm", 1),
     ("RMM", "actors/savina/rmm.scm", 1),
     ("QSORT", "actors/savina/qsort.scm", 1),
     ("APSP", "actors/savina/apsp.scm", 1),
    ("SOR", "actors/savina/sor.scm", 1),
    ("ASTAR", "actors/savina/astar.scm", 1),
    ("NQN", "actors/savina/nqn.scm", 1)
  )*/
  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
//    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val timestamp: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = timestamp.isActorTimestamp
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, timestamp.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    if (args.size == 0) {
      val N = ExperimentsConfig.nruns
      val warmup = ExperimentsConfig.warmupRuns
      for ((name, file, bound) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")

        val mbox = new BoundedMultisetMboxImpl[ContextSensitiveTID, lat.L](bound)
        val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T, ContextSensitiveTID](mbox, args.size > 0, ActorMacrostepping)

        val results = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T](machine, sem)(_, true, ExperimentsConfig.timeout))

          }
        )
        val times = results.map(_.time)
        val time = (times.drop(warmup).sum / N * 1000).toInt
        val n = results(0).numberOfStates
        println(s"$time (n: $n)")
      }
    } else {
      val mbox = new BoundedMultisetMboxImpl[ContextSensitiveTID, lat.L](args(1).toInt)
      val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T, ContextSensitiveTID](mbox, args.size > 0, ActorMacrostepping)

      val result = Util.runOnFile(args(0), ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T](machine, sem)(_, true, ExperimentsConfig.timeout))
      val graph = args(2)
      if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
      println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
      result.toFile(graph)(GraphDOTOutput)
    }
  }
}


object ActorExperimentsMacrosteppingBoundedList {
     val benchFiles: List[(String, String, Int)] = List(
     ("PP", "actors/savina/pp.scm", 1),
     ("COUNT-SEQ", "actors/savina/count-seq.scm", 1),
     ("FJT-SEQ", "actors/savina/fjt-seq.scm", 2),
     ("FJC-SEQ", "actors/savina/fjc-seq.scm", 1),
     ("FACTORIAL", "actors/factorial.scm", 1),
     ("STACK", "actors/stack.scm", 4),
     ("CELL", "actors/cell.scm", 2),
     ("PARIKH", "actors/soter/parikh.scm", 2),
     ("PIPE-SEQ", "actors/soter/pipe-seq.scm", 1),
     ("SAFE-SEND", "actors/soter/safe_send.scm", 4),
     ("STATE-FACTORY", "actors/soter/state_factory.scm", 1),
     ("STUTTER", "actors/soter/stutter.scm", 1))

/*  val benchFiles: List[(String, String, Int)] = List(
////     ("PP", "actors/savina/pp.scm", 1),
////     ("COUNT", "actors/savina/count.scm", 1),
////     ("FJT", "actors/savina/fjt.scm", 1),
////     ("FJC", "actors/savina/fjc.scm", 1),

//     ("THR", "actors/savina/thr.scm", 2),
//     ("CHAM", "actors/savina/cham.scm", 2),
//     ("BIG", "actors/savina/big.scm", 2),
////     ("CDICT", "actors/savina/cdict.scm", 1),
//     ("CSLL", "actors/savina/csll.scm", 2),
////     ("PCBB", "actors/savina/pcbb.scm", 1),
//     ("PHIL", "actors/savina/phil.scm", 2),
//     ("SBAR", "actors/savina/sbar.scm", 2),
////     ("CIG", "actors/savina/cig.scm", 2),
//     ("LOGM", "actors/savina/logm.scm", 2),
////     ("BTX", "actors/savina/btx.scm", 1),
//     ("RSORT", "actors/savina/rsort.scm", 2),
//     ("FBANK", "actors/savina/fbank.scm", 2),
//     ("SIEVE", "actors/savina/sieve.scm", 2),
//     ("UCT", "actors/savina/uct.scm", 2),
//     ("OFL", "actors/savina/ofl.scm", 2),
//     ("TRAPR", "actors/savina/trapr.scm", 2),
//     ("PIPREC", "actors/savina/piprec.scm", 2),
//     ("RMM", "actors/savina/rmm.scm", 2),
//     ("QSORT", "actors/savina/qsort.scm", 2),
//     ("APSP", "actors/savina/apsp.scm", 2),
//    ("SOR", "actors/savina/sor.scm", 2),
//    ("ASTAR", "actors/savina/astar.scm", 2),
    ("NQN", "actors/savina/nqn.scm", 2)
  )*/
  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
//    val timeout: Option[Long] = Some(120 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val timestamp: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = timestamp.isActorTimestamp
    val visitor = new EmptyActorVisitor[SchemeExp, lat.L, ClassicalAddress.A]
    val sem = new ASchemeSemanticsWithVisitorAndOptimization[lat.L, ClassicalAddress.A, timestamp.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L], visitor)

    if (args.size == 0) {
      val N = ExperimentsConfig.nruns
      val warmup = ExperimentsConfig.warmupRuns
      for ((name, file, bound) <- benchFiles) {
        val sname = name.padTo(10, " ").mkString
        print(s"$sname | ")

        val mbox = new BoundedListMboxImpl[ContextSensitiveTID, lat.L](bound)
        val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T, ContextSensitiveTID](mbox, args.size > 0, ActorMacrostepping)

        val results = (1 to N+warmup).map(i =>
          scala.Console.withOut(new java.io.OutputStream { override def write(b: Int) {} }) {
            Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T](machine, sem)(_, true, ExperimentsConfig.timeout))

          }
        )
        val times = results.map(_.time)
        val time = (times.drop(warmup).sum / N * 1000).toInt
        val n = results(0).numberOfStates
        println(s"$time (n: $n)")
      }
    } else {
      val mbox = new BoundedMultisetMboxImpl[ContextSensitiveTID, lat.L](args(1).toInt)
      val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T, ContextSensitiveTID](mbox, args.size > 0, ActorMacrostepping)

      val result = Util.runOnFile(args(0), ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, timestamp.T](machine, sem)(_, true, ExperimentsConfig.timeout))
      val graph = args(2)
      if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
      println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
      result.toFile(graph)(GraphDOTOutput)
    }
  }
}

object ActorExperimentsMacrosteppingGraph {
   val benchFiles: List[(String, String)] = List(
     ("PP", "actors/savina/pp.scm"),
     ("COUNT-SEQ", "actors/savina/count-seq.scm"),
     ("FJT-SEQ", "actors/savina/fjt-seq.scm"),
     ("FJC-SEQ", "actors/savina/fjc-seq.scm"),
     ("FACTORIAL", "actors/factorial.scm"),
     ("STACK", "actors/stack.scm"),
     ("CELL", "actors/cell.scm"),
     ("PARIKH", "actors/soter/parikh.scm"),
     ("PIPE-SEQ", "actors/soter/pipe-seq.scm"),
     ("SAFE-SEND", "actors/soter/safe_send.scm"),
     ("STATE-FACTORY", "actors/soter/state_factory.scm"),
     ("STUTTER", "actors/soter/stutter.scm"))

//  val benchFiles: List[(String, String)] = List(
//    ("PP", "actors/savina/pp.scm"),
//    ("COUNT", "actors/savina/count.scm"),
//    ("FJT", "actors/savina/fjt.scm"),
//    ("FJC", "actors/savina/fjc.scm"),
//    ("THR", "actors/savina/thr.scm"),
//    ("CHAM", "actors/savina/cham.scm"),
//    ("BIG", "actors/savina/big.scm"),
//    ("CDICT", "actors/savina/cdict.scm"),
//    ("CSLL", "actors/savina/csll.scm"),
//    ("PCBB", "actors/savina/pcbb.scm"),
//    ("PHIL", "actors/savina/phil.scm"),
//    ("SBAR", "actors/savina/sbar.scm"),
//    ("CIG", "actors/savina/cig.scm"),
//    ("LOGM", "actors/savina/logm.scm"),
//    ("BTX", "actors/savina/btx.scm"),
//    ("RSORT", "actors/savina/rsort.scm"),
//    ("FBANK", "actors/savina/fbank.scm"),
//    ("SIEVE", "actors/savina/sieve.scm"),
//    ("UCT", "actors/savina/uct.scm"),
//    ("OFL", "actors/savina/ofl.scm"),
//    ("TRAPR", "actors/savina/trapr.scm"),
//    ("PIPREC", "actors/savina/piprec.scm"),
//    ("RMM", "actors/savina/rmm.scm"),
//    ("QSORT", "actors/savina/qsort.scm"),
//    ("APSP", "actors/savina/apsp.scm"),
//    ("SOR", "actors/savina/sor.scm"),
//    ("ASTAR", "actors/savina/astar.scm"),
//    ("NQN", "actors/savina/nqn.scm")
//  )
  def main(args: Array[String]): Unit = {
    val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
    val timeout: Option[Long] = Some(1800 * 1e9.toLong)
    implicit val isASchemeLattice = lat.isASchemeLattice
    val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
    implicit val isActorTimestamp = time.isActorTimestamp
    val mbox = new GraphMboxImpl[ContextSensitiveTID, lat.L]
    val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox, args.size > 0, ActorMacrostepping)
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

