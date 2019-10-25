package scalaam.cli

import java.text.SimpleDateFormat
import java.util.concurrent.TimeoutException
import java.util.{Calendar, Date}

import scalaam.core._
import scalaam.language.scheme.SchemeInterpreter.Value
import scalaam.language.scheme.SchemeInterpreter.Value._
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme._

import scala.concurrent.duration._

object MODComparison extends App {

  type Machine = ModAnalysis[SchemeExp] with FullArgumentSensitivity with ConstantPropagationDomain

  val benchmarks: List[String] = List(
    "test/ad/abstrct.scm",
    //"test/ad/bfirst.scm", // VARARG
    // "test/ad/bst.scm", // VARARG
    "test/ad/btree.scm",
    "test/ad/bubsort.scm",
    "test/ad/dict.scm",
    // "test/ad/dictExamples.scm", // EMPTY
    //"test/ad/heap.scm", // PARSER ERROR TODO
    "test/ad/inssort.scm",
    //"test/ad/linear.scm", // VARARG
    //"test/ad/list.scm", // VARARG
    "test/ad/mesort.scm",
    "test/ad/prioq.scm",
    "test/ad/qsort.scm",
    "test/ad/qstand.scm",
    //"test/ad/queue.scm", // VARARG
    "test/ad/quick.scm",
    //"test/ad/RBtreeADT.scm", // VARARG
    //"test/ad/selsort.scm", // PARSER ERROR TODO
    "test/ad/stack.scm",
    //"test/ad/stspaceCODE.scm", // VARARG
    "test/blur.scm",
    "test/bound-precision.scm",
    "test/church-2-num.scm",
    "test/church-6.scm",
    "test/church.scm",
    "test/collatz.scm",
    "test/count.scm",
    "test/eta.scm",
    "test/fact.scm",
    "test/fib.scm",
    "test/gabriel/boyer.scm",
    "test/gabriel/browse.scm",
    "test/gabriel/cpstak.scm",
    "test/gabriel/dderiv.scm",
    "test/gabriel/deriv.scm",
    "test/gabriel/destruc.scm",
    "test/gabriel/diviter.scm",
    "test/gabriel/divrec.scm",
    "test/gabriel/puzzle.scm",
    "test/gabriel/takl.scm",
    "test/gabriel/triangl.scm",
    "test/gambit/array1.scm",
    "test/gambit/browse.scm",
    "test/gambit/cat.scm",
    //"test/gambit/compiler.scm", // PARSER ERROR TODO
    "test/gambit/ctak.scm",
    "test/gambit/deriv.scm",
    "test/gambit/destruc.scm",
    "test/gambit/diviter.scm",
    "test/gambit/earley.scm",
    "test/gambit/fibc.scm",
    "test/gambit/graphs.scm",
    "test/gambit/lattice.scm",
    "test/gambit/matrix.scm",
    "test/gambit/mazefun.scm",
    //"test/gambit/nboyer.scm", // VARARG
    "test/gambit/nqueens.scm",
    "test/gambit/paraffins.scm",
    "test/gambit/perm9.scm",
    //"test/gambit/peval.scm", // VARARG
    "test/gambit/primes.scm",
    "test/gambit/puzzle.scm",
    //"test/gambit/sboyer.scm", // VARARG
    //"test/gambit/scheme.scm", // VARARG
    //"test/gambit/slatex.scm", // PARSER LIMITATION TODO
    "test/gambit/string.scm",
    "test/gambit/sum.scm",
    "test/gambit/sumloop.scm",
    "test/gambit/tail.scm",
    "test/gambit/tak.scm",
    //"test/gambit/trav1.scm", // PARSER ERROR TODO
    "test/gambit/triangl.scm",
    "test/gambit/wc.scm",
    "test/gcipd.scm",
    "test/grid.scm",
    "test/inc.scm",
    "test/infinite-1.scm",
    "test/infinite-2.scm",
    "test/infinite-3.scm",
    "test/kcfa2.scm",
    "test/kcfa3.scm",
    "test/kernighanvanwyk/ack.scm",
    "test/letrec-begin.scm",
    "test/loop2.scm",
    "test/mceval.scm",
    "test/mj09.scm",
    "test/mut-rec.scm",
    "test/nested-defines.scm",
    "test/primtest.scm",
    //"test/quasiquoting-simple.scm", // PARSER ERROR TODO
    // "test/quasiquoting.scm", // PARSER LIMITATION TODO // VARARG
    "test/regex.scm",
    "test/rosetta/easter.scm",
    "test/rosetta/quadratic.scm",
    "test/rotate.scm",
    "test/rsa.scm",
    "test/sat.scm",
    //"test/scm2c.scm", // PARSER ERROR TODO
    "test/scm2java.scm",
    "test/scp1/2.1.scm",
    "test/scp1/2.4.scm",
    "test/scp1/3.1.scm",
    "test/scp1/3.2.1.scm",
    "test/scp1/3.2.scm",
    "test/scp1/3.3.scm",
    "test/scp1/3.4.scm",
    "test/scp1/3.6.scm",
    "test/scp1/3.8.scm",
    "test/scp1/3.9.scm",
    "test/scp1/4.1.scm",
    "test/scp1/4.8.scm",
    "test/scp1/5.14.3.scm",
    "test/scp1/5.19.scm",
    "test/scp1/5.20.4.scm",
    "test/scp1/5.21.scm",
    "test/scp1/5.22.scm",
    "test/scp1/5.6.scm",
    "test/scp1/5.7.scm",
    "test/scp1/7.11.scm",
    "test/scp1/7.12.scm",
    "test/scp1/7.13.scm",
    "test/scp1/7.14.scm",
    "test/scp1/7.15.scm",
    "test/scp1/7.16.scm",
    "test/scp1/7.17.scm",
    "test/scp1/7.18.scm",
    "test/scp1/7.2.scm",
    "test/scp1/7.3.scm",
    "test/scp1/7.4.scm",
    // "test/scp1/7.5.scm", // DOT NOTATION
    // "test/scp1/7.6.scm", // DOT NOTATION
    //"test/scp1/7.9.scm",
    "test/scp1/8.1.1.scm",
    "test/scp1/8.1.3.scm",
    "test/scp1/8.10.scm",
    //"test/scp1/8.11.scm", // VARARG
    "test/scp1/8.12.scm",
    "test/scp1/8.13.scm",
    "test/scp1/8.14.scm",
    "test/scp1/8.15.scm",
    //"test/scp1/8.16.scm", // VARARG
    //"test/scp1/8.5.scm", // VARARG
    "test/scp1/8.6.scm",
    "test/scp1/9.12.scm",
    "test/scp1/9.13.scm",
    "test/scp1/9.14.scm",
    "test/scp1/9.15.scm",
    "test/scp1/9.16.scm",
    "test/scp1/9.17.scm",
    "test/scp1/9.18.scm",
    "test/scp1/9.2.scm",
    "test/scp1/9.3.scm",
    "test/scp1/9.5.scm",
    "test/scp1/9.6.scm",
    "test/scp1/9.7.scm",
    "test/scp1/9.8.scm",
    "test/scp1/9.9.scm",
    //"test/SICP-compiler.scm", // PARSER ERROR TODO
    "test/sigscheme/arithint.scm",
    "test/sigscheme/case.scm",
    "test/sigscheme/let-loop.scm",
    "test/sigscheme/loop.scm",
    "test/sigscheme/mem.scm",
    "test/sigscheme/rec.scm",
    "test/sigscheme/takr.scm",
    "test/sq.scm",
    //"test/Streams.scm", // VARARG
    "test/sym.scm",
    "test/widen.scm",
    "test/work.scm",
  )

  def readFile(file: String): SchemeExp = {
    val f   = scala.io.Source.fromFile(file)
    val exp = SchemeParser.parse(f.getLines().mkString("\n"))
    f.close()
    exp
  }

  // Avoid output being buffered.
  def display(data: String): Unit = {
    print(data)
    Console.out.flush()
  }

  def displayErr(data: String): Unit = {
    System.err.print(data)
    System.err.flush()
  }

  val outputDir: String = "./"

  /** Creates a fileName including the given name, suffix and a timestamp. */
  def ts(name: String, suffix: String): String = {
    val now: Date                = Calendar.getInstance().getTime
    val format: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd-HH'h'mm_")
    outputDir + format.format(now) + name + suffix
  }

  def checkSubsumption[A <: Address, L](v: Set[Value], lat: SchemeLattice[L, SchemeExp, A], abs: L): Boolean = v.forall( _ match {
      case Value.Undefined(_) => true
      case Value.Unbound(_)   => true
      case Clo(_, _)          => lat.getClosures(abs).nonEmpty
      case Primitive(p)       => lat.subsumes(abs, lat.primitive(p))
      case Str(s)             => lat.subsumes(abs, lat.string(s))
      case Symbol(s)          => lat.subsumes(abs, lat.symbol(s))
      case Integer(i)         => lat.subsumes(abs, lat.number(i))
      case Real(r)            => lat.subsumes(abs, lat.real(r))
      case Bool(b)            => lat.subsumes(abs, lat.bool(b))
      case Character(c)       => lat.subsumes(abs, lat.char(c))
      case Nil                => lat.subsumes(abs, lat.nil)
      case Cons(_, _)         => lat.getPointerAddresses(abs).nonEmpty
      case Vector(_)          => lat.getPointerAddresses(abs).nonEmpty
      case v                  => throw new Exception(s"Unknown concrete value type: $v")
    })

  def check[A <: Address, L](name: String, v: Set[Value], p: Position, lat: SchemeLattice[L, SchemeExp, A], abs: L): Unit = {
    if (!checkSubsumption(v, lat, abs))
      display(s"$name: subsumption check failed: $v > $abs at $p.\n")
  }

  def forMachine(name: String, machine: Machine, cMap: Map[Position, Set[Value]], timeout: Duration): Unit = try {
    displayErr("* " + name + "\n")
    Interruptable.inFuture({ machine.analyze() }, timeout)

    val deps  = machine.deps
    val store = machine.store

    val pMap: Map[Position, machine.Value] = store.groupBy({_._1 match {
      case machine.GlobalAddr(addr)       => addr.pos()
      case machine.ComponentAddr(_, addr) => addr.pos()
      case _                              => Position.none
    }}).mapValues(_.values.foldLeft(machine.lattice.bottom)((a, b) => machine.lattice.join(a, b)))

    display(s"Number of components: ${machine.allComponents.size}.\n")
    display(s"Store keyset size ${store.keySet.size}.\n")
    display(s"Dependency keyset size: ${deps.keySet.size}.\n")

    for (elem <- cMap.keySet)
      check(name, cMap(elem), elem, machine.lattice, pMap(elem))
  } catch {
    case _: TimeoutException => displayErr(s"$name timed out!")
    case e: Throwable => e.printStackTrace()
      println()
  }

  def forFile(file: String, timeout: Duration = Duration(10, MINUTES)): Unit = try {
    displayErr(file + "\n")

    val program = readFile(file)
    val machines: List[(String, Machine)] = List(
      ("bigStep",   new ModAnalysis(program) with FullArgumentSensitivity with ConstantPropagationDomain with BigStepSchemeModFSemantics),
      ("smallStep", new ModAnalysis(program) with FullArgumentSensitivity with ConstantPropagationDomain with SmallStepSchemeModFSemantics),
    )

    // For every position, cMap keeps the concrete values encountered by the concrete interpreter.
    var cMap: Map[Position, Set[Value]] = Map().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((p, v) => cMap = cMap + (p -> (cMap(p) + v)), false)
    Interruptable.inFuture(interpreter.run(SchemeUndefiner.undefine(List(program))), timeout) // Easy timeout: we do not have to add the timeout to the concrete interpreter itself.

    display(s"-> Inferred ${cMap.keySet.size} positions to check values.\n")

    machines.foreach(m => forMachine(m._1, m._2, cMap, timeout))

    display("\n")

  } catch {
    case _: TimeoutException => displayErr("Concrete machine timed out!")
    case e: Throwable => e.printStackTrace()
      println()
  }

  forFile("test/ad/dict.scm")
  //benchmarks.foreach(forFile)
}