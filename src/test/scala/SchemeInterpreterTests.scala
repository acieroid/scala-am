import scalaam.core._
import scalaam.language.scheme._
import scalaam.machine._
import scalaam.lattice._

abstract class SchemeInterpreterTests[A <: Address, V, T, C](
  kind: BenchmarkTestKind.BenchmarkTestKind)(
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

  Benchmarks.benchmarksFor(kind).foreach(b =>
    BenchmarksUtil.fileContent(b).foreach(program =>
      property(s"$b should result in ${fromValue(b.result)}") {
        System.gc() /* Runs a GC before, to (maybe) avoid some GC overhead errors */
        checkResult(program, fromValue(b.result), Timeout.seconds(10))
      }
    ))
}

abstract class SchemeInterpreterGAAMTests[A <: Address, T, V](
  allocator: Allocator[A, T, SchemeExp],
  kind: BenchmarkTestKind.BenchmarkTestKind)(
  implicit val time: Timestamp[T, SchemeExp],
  implicit val l: SchemeLattice[V, SchemeExp, A])
    extends SchemeInterpreterTests[A, V, T, SchemeExp](kind) {
  val sem = new BaseSchemeSemantics[A, V, T, SchemeExp](allocator)
  val machine = new GAAM[SchemeExp, A, V, T](sem)
}

class SchemeInterpreterConcreteTests extends SchemeInterpreterTests[ConcreteSchemeAddress.A, ConcreteSchemeLattice.L, ConcreteSchemeTimestamp.T, SchemeExp](BenchmarkTestKind.SchemeRunConcrete)(ConcreteSchemeTimestamp.T.typeclass, ConcreteSchemeLattice.L.lattice) {
  val sem = new BaseSchemeSemantics[ConcreteSchemeAddress.A, ConcreteSchemeLattice.L, ConcreteSchemeTimestamp.T, SchemeExp](ConcreteSchemeAddress.Alloc)
  val machine = new ConcreteMachine[SchemeExp, ConcreteSchemeAddress.A, ConcreteSchemeLattice.L, ConcreteSchemeTimestamp.T](sem)
}

// NOTE: we cannot use the concrete interpreter because we only have stores with weak updates. TODO[easy]: write a concrete machine that uses a concrete store, or introduce a store with abstract coutningn
//class ConcreteSchemeInterpreterAAMTests extends SchemeInterpreterAAMTests[NameAddress.A, ConcreteSchemeTimestamp.T, ConcreteSchemeLattice.L](NameAddress.Alloc[ConcreteSchemeTimestamp.T, SchemeExp], BenchmarkTestKind.SchemeRunConcrete)

object ConstantPropagationSchemeLattice extends MakeSchemeLattice[SchemeExp, NameAddress.A, ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.R, ConstantPropagation.C, ConstantPropagation.Sym]
object TypeSchemeLattice extends MakeSchemeLattice[SchemeExp, NameAddress.A, Type.S, Concrete.B, Type.I, Type.R, Type.C, Type.Sym]

// class ConstantPropagationSchemeInterpreterAAMTests extends SchemeInterpreterAAMTests[NameAddress.A, ZeroCFASchemeTimestamp.T, ConstantPropagationSchemeLattice.L](NameAddress.Alloc[ZeroCFASchemeTimestamp.T, SchemeExp], BenchmarkTestKind.SchemeRunAbstract)
class TypeSchemeInterpreterGAAMTests extends SchemeInterpreterGAAMTests[NameAddress.A, ZeroCFASchemeTimestamp.T, TypeSchemeLattice.L](NameAddress.Alloc[ZeroCFASchemeTimestamp.T, SchemeExp], BenchmarkTestKind.SchemeRunAbstract)
