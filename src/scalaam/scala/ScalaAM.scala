package scalaam
import scalaam.core._

object Main {
  def main(args: Array[String]) = {
    ()
  }
}

abstract class Interpreter {
  def run(file: String, timeout: Timeout.T = Timeout.seconds(10), outputDot: Boolean = true): (Long, Int)
  def main(args: Array[String]): Unit = {
    if (args.size == 1) {
      val (time, states) = run(args.head)
      println(s"Evaluation took ${time}ms, computed $states states")
    } else {
      println(s"File expected as argument")
    }
  }
}

object RunGabriel {
  import scalaam.language.scheme._
  def main(args: Array[String]) = {
    val benchmarks = List(
      "boyer",
      "browse",
      "cpstak",
      "dderiv",
      "deriv",
      "destruc",
      "diviter",
      "divrec",
      "puzzle", // vectors
      "takl",
      "triangl"  // vectors
    )
    val pre  = "test/gabriel/"
    val post = ".scm"

    val t0 = System.nanoTime
    val parsed = benchmarks.map(b => {
      val f       = scala.io.Source.fromFile(pre + b + post)
      val content = f.getLines.mkString("\n")
      f.close()
      (b, SchemeParser.parse(content))
    })
    val t1          = System.nanoTime
    val parsingTime = (t1 - t0) / 1000000
    println(s"Parsing time: ${parsingTime}ms")
    println(s"Press enter to start benchmarking...")
    //scala.io.StdIn.readLine()

    parsed.foreach(b => {
      try {
        val timeout = Timeout.seconds(10)
        val (t, s)  = SchemeRunAAM.runProgram(b._2, timeout, false)
        println(s"${b._1} | $t | $s")
      } catch {
        case e: Exception => println(s"$b failed ($e)")
      }
    })
    Profiler.printResults()
  }
}

/* To be used with the console: `sbt console`, then scalaam.SchemeRunAAM.run(file) */
object SchemeRunAAM extends Interpreter {
  import scalaam.language.scheme._
  import scalaam.machine._
  import scalaam.graph._
  import scalaam.lattice._

  val address   = NameAddress
  val timestamp = ZeroCFA[SchemeExp]()
  val lattice =
    new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  val sem = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
    address.Alloc[timestamp.T, SchemeExp]
  )
  val machine = new AAM[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = new DotGraph[machine.State, machine.Transition]

  def run(
      file: String,
      timeout: Timeout.T = Timeout.seconds(10),
      outputDot: Boolean = true
  ): (Long, Int) = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    runProgram(SchemeParser.parse(content), timeout, outputDot)
  }
  def runProgram(
      content: SchemeExp,
      timeout: Timeout.T = Timeout.seconds(10),
      outputDot: Boolean = true
  ): (Long, Int) = {
    val t0     = System.nanoTime
    val result = machine.run[graph.G](content, timeout)
    val t1     = System.nanoTime
    val time   = (t1 - t0) / 1000000
    if (outputDot) result.toFile("foo.dot")
    import Graph.GraphOps
    val states = result.nodes
    (time, states)
  }

  def logValues(
      file: String,
      timeout: Timeout.T = Timeout.seconds(10)
  ): Map[Identifier, lattice.L] = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    val result = machine.run[graph.G](SchemeUndefiner.undefine(List(SchemeParser.parse(content))), timeout)
    result
    /* Let's collect all nodes that evaluate a variable */
      .findNodes(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              env.lookup(id.name) match {
                case Some(_) => true
                case None    =>
                  // println(s"Identifier is unbound: $id")
                  false
              }
            case _ => false
          }
      )
      /* And evaluate the value of each variable */
      .collect(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              (id, s.store.lookup(env.lookup(id.name).get).get)
          }
      )
      /* We now have a list of pairs (variable, value).
     Let's join all of them by variable in a single map */
      .foldLeft(
        Map
          .empty[Identifier, lattice.L]
          .withDefaultValue(SchemeLattice[lattice.L, SchemeExp, address.A].bottom)
      )(
        (map, pair) =>
          pair match {
            case (id, value) =>
              map + (id -> SchemeLattice[lattice.L, SchemeExp, address.A].join(map(id), value))
          }
      )
  }

}

object SchemeRunConcrete extends Interpreter {
  import scalaam.language.scheme._
  import scalaam.machine._
  import scalaam.graph._
  import scalaam.lattice._

  val timestamp = ConcreteTimestamp[SchemeExp]()
  val address   = TimestampAddress[timestamp.T, SchemeExp]
  val lattice =
    new MakeSchemeLattice[
      SchemeExp,
      address.A,
      Concrete.S,
      Concrete.B,
      Concrete.I,
      Concrete.R,
      Concrete.C,
      Concrete.Sym
    ]
  val sem     = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](address.Alloc)
  val machine = new ConcreteMachine[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = new DotGraph[machine.State, machine.Transition]

  def run(
      file: String,
      timeout: Timeout.T = Timeout.seconds(10),
      outputDot: Boolean = true
  ): (Long, Int) = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    runProgram(SchemeUndefiner.undefine(List(SchemeParser.parse(content))), timeout, outputDot)
  }
  def runProgram(
      content: SchemeExp,
      timeout: Timeout.T = Timeout.seconds(10),
      outputDot: Boolean = true
  ): (Long, Int) = {
    val t0     = System.nanoTime
    val result = machine.run[graph.G](content, timeout)
    val t1     = System.nanoTime
    val time   = (t1 - t0) / 1000000
    if (outputDot) result.toFile("foo.dot")
    import Graph.GraphOps
    val states = result.nodes
    (time, states)
  }
  def logValues(
      file: String,
      timeout: Timeout.T = Timeout.seconds(10)
  ): Map[Identifier, lattice.L] = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    val result = machine.run[graph.G](SchemeUndefiner.undefine(List(SchemeParser.parse(content))), timeout)
    result
    /* Let's collect all nodes that evaluate a variable */
      .findNodes(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              env.lookup(id.name) match {
                case Some(_) => true
                case None    =>
                  // println(s"Identifier is unbound: $id")
                  false
              }
            case _ => false
          }
      )
      /* And evaluate the value of each variable */
      .collect(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              (id, s.store.lookup(env.lookup(id.name).get).get)
          }
      )
      /* We now have a list of pairs (variable, value).
         Let's join all of them by variable in a single map */
      .foldLeft(
        Map
          .empty[Identifier, lattice.L]
          .withDefaultValue(SchemeLattice[lattice.L, SchemeExp, address.A].bottom)
      )(
        (map, pair) =>
          pair match {
            case (id, value) =>
              map + (id -> SchemeLattice[lattice.L, SchemeExp, address.A].join(map(id), value))
          }
      )
  }
}

object SchemeRunAAMLKSS extends Interpreter {
  import scalaam.language.scheme._
  import scalaam.machine._
  import scalaam.core._
  import scalaam.graph._
  import scalaam.lattice._

  val address   = NameAddress
  val timestamp = ZeroCFA[SchemeExp]()
  val lattice =
    new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  val sem = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
    address.Alloc[timestamp.T, SchemeExp]
  )
  val machine = new AAMLKSS[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = new DotGraph[machine.State, machine.Transition]

  def run(file: String, timeout: Timeout.T = Timeout.seconds(10), outputDot: Boolean = true) = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    val t0     = System.nanoTime
    val result = machine.run[graph.G](SchemeUndefiner.undefine(List(SchemeParser.parse(content))), timeout)
    val t1     = System.nanoTime
    val time   = (t1 - t0) / 1000000
    if (outputDot) result.toFile("foo.dot")
    import Graph.GraphOps
    val states = result.nodes
    (time, states)
  }
}

object SchemeRunGAAM extends Interpreter {
  import scalaam.language.scheme._
  import scalaam.machine._
  import scalaam.core._
  import scalaam.graph._
  import scalaam.lattice._

  val address   = NameAddress
  val timestamp = ZeroCFA[SchemeExp]()
  val lattice =
    new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  val sem = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
    address.Alloc[timestamp.T, SchemeExp]
  )
  val machine = new GAAM[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = new DotGraph[machine.State, machine.Transition]

  def run(file: String, timeout: Timeout.T = Timeout.seconds(10), outputDot: Boolean = true) = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    val t0     = System.nanoTime
    val result = machine.run[graph.G](SchemeUndefiner.undefine(List(SchemeParser.parse(content))), timeout)
    val t1     = System.nanoTime
    val time   = (t1 - t0) / 1000000
    if (outputDot) result.toFile("foo.dot")
    import Graph.GraphOps
    val states = result.nodes
    (time, states)
  }
}

object CompareMachines {
  def compare(file: String): Unit = {
    println("Running concrete")
    val conc = SchemeRunConcrete.logValues(file)
    println("Running abstract")
    val abs = SchemeRunAAM.logValues(file)
    if (conc.keySet != abs.keySet) {
      if (conc.keySet.subsetOf(abs.keySet)) {
        println(s"Abstract has observed extra variables: ${abs.keySet.diff(conc.keySet)}")
      } else {
        println("!!!SOUNDNESS PROBLEM!!!")
        /* If the concrete execution observed variables not observed in the abstract, the abstract is not sound! */
        println(s"Concrete has observed extra variables: ${conc.keySet.diff(abs.keySet)}")
        return () /* And we can directly abort */
      }
    }
    import scalaam.core.ConcreteVal._
    import scalaam.language.scheme._
    val conclat = SchemeLattice[SchemeRunConcrete.lattice.L, SchemeExp, SchemeRunConcrete.address.A]
    val abslat  = SchemeLattice[SchemeRunAAM.lattice.L, SchemeExp, SchemeRunAAM.address.A]
    conc.keySet.foreach(id => {
      val concval = conc(id)
      val abstractedconcval = conclat
        .concreteValues(concval)
        .foldLeft(abslat.bottom)(
          (lat, v) =>
            abslat.join(
              lat,
              v match {
                case ConcreteNumber(x) => abslat.number(x)
                case ConcreteReal(x)   => abslat.real(x)
                case ConcreteString(x) => abslat.string(x)
                case ConcreteBool(x)   => abslat.bool(x)
                case ConcreteChar(x)   => abslat.char(x)
                case ConcreteSymbol(x) => abslat.symbol(x)
                case ConcretePrim(prim: SchemeRunConcrete.sem.Primitive) =>
                  abslat
                    .primitive(SchemeRunAAM.sem.allPrimitives.find(p => p.name == prim.name).get)
                case ConcreteNil => abslat.nil
                case ConcreteClosure(exp, env, name) =>
                  val env2 = env.keys.foldLeft(Environment.empty[SchemeRunAAM.address.A])(
                    (env2, k) =>
                      env2.extend(k, env.lookup(k).get match {
                        case SchemeRunConcrete.address.A(nameaddr, _) => nameaddr
                      })
                  )
                  abslat.closure((exp.asInstanceOf[SchemeExp], env2), name)
                case ConcretePointer(SchemeRunConcrete.address.A(nameaddr, _)) =>
                  abslat.pointer(nameaddr)
              }
            )
        )
      val absval = abs(id)
      if (absval == abstractedconcval) {
        println(s"${id.fullString}: full precision! ($absval)")
      } else if (!abslat.subsumes(absval, abstractedconcval)) {
        println(
          s"${id.fullString}: SOUNDNESS PROBLEM, inferred $absval while concrete shows $abstractedconcval"
        )
      } else {
        println(
          s"${id.fullString}: overapproximative, inferred as $absval while best abstraction is $abstractedconcval"
        )
      }
    })
  }
}
