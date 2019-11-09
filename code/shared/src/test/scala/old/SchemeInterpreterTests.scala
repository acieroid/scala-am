/*
import scalaam.core._
import scalaam.language.scheme._
import scalaam.machine._
import scalaam.lattice._

abstract class SchemeInterpreterTests[A <: Address, V, T, C](
  kind: BenchmarkTestKind.BenchmarkTestKind, machineName: String)(
  override implicit val timestamp: Timestamp[T, C],
  override implicit val lat: SchemeLattice[V, SchemeExp, A])
    extends SchemeTests[A, V, T, C] {

  import scalaam.language.sexp._
  import lat._
  def fromValue(v: Value): V = v match {
    case ValueString(s) => string(s)
    case ValueSymbol(sym) => symbol(sym)
    case ValueInteger(n) => number(n)
    case ValueReal(n) => real(n)
    case ValueBoolean(b) => bool(b)
    case ValueCharacter(c) => char(c)
    case ValueNil => nil
  }
  def processProgram(program: SchemeExp): SchemeExp = program

  Benchmarks.benchmarksFor(kind).foreach(b =>
    BenchmarksUtil.fileContent(b).foreach(program =>
      property(s"[$machineName] $b should result in ${fromValue(b.result)}") {
        System.gc() // Runs a GC before, to (maybe) avoid some GC overhead errors
        checkResult(processProgram(SchemeParser.parse(program)), fromValue(b.result), Timeout.seconds(10))
      }
    ))
}

object ConcreteLattice extends MakeSchemeLattice[SchemeExp, ConcreteSchemeAddress.A, Concrete.S, Concrete.B, Concrete.I, Concrete.R, Concrete.C, Concrete.Sym]
object ConstantPropagationSchemeLattice extends MakeSchemeLattice[SchemeExp, NameAddress.A, ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.R, ConstantPropagation.C, ConstantPropagation.Sym]
object TypeSchemeLattice extends MakeSchemeLattice[SchemeExp, NameAddress.A, Type.S, Concrete.B, Type.I, Type.R, Type.C, Type.Sym]

class ConcreteSchemeInterpreterTests extends SchemeInterpreterTests[ConcreteSchemeAddress.A, ConcreteSchemeLattice.L, ConcreteSchemeTimestamp.T, SchemeExp](BenchmarkTestKind.SchemeRunConcrete,  "concrete")(ConcreteSchemeTimestamp.T.typeclass, ConcreteSchemeLattice.L.lattice) {
  val sem = new BaseSchemeSemantics[ConcreteSchemeAddress.A, ConcreteSchemeLattice.L, ConcreteSchemeTimestamp.T, SchemeExp](ConcreteSchemeAddress.Alloc)
  val machine = new ConcreteMachine[SchemeExp, ConcreteSchemeAddress.A, ConcreteSchemeLattice.L, ConcreteSchemeTimestamp.T](sem)
  override def processProgram(program: SchemeExp) = SchemeUndefiner.undefine(List(program))
}

abstract class SchemeInterpreterGAAMTests[A <: Address, T, V](
  allocator: Allocator[A, T, SchemeExp],
  kind: BenchmarkTestKind.BenchmarkTestKind,
  machineName: String)(
  implicit val time: Timestamp[T, SchemeExp],
  implicit val l: SchemeLattice[V, SchemeExp, A])
    extends SchemeInterpreterTests[A, V, T, SchemeExp](kind, machineName) {
  val sem = new BaseSchemeSemantics[A, V, T, SchemeExp](allocator)
  val machine = new GAAM[SchemeExp, A, V, T](sem)
  override def processProgram(program: SchemeExp) = SchemeUndefiner.undefine(List(program))
}

class TypeSchemeInterpreterGAAMTests extends SchemeInterpreterGAAMTests[NameAddress.A, ZeroCFASchemeTimestamp.T, TypeSchemeLattice.L](NameAddress.Alloc[ZeroCFASchemeTimestamp.T, SchemeExp], BenchmarkTestKind.SchemeRunAbstract, "GAAM/Type")
*/
