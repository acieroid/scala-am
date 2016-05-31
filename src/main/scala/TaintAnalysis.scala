/* Here, we develop a static taint analysis of Scheme programs. */

/* We need a lattice tracking tainted values, that is compatible with Scheme
 * operations */

/* We represent the taint status by a lattice, with the following ordering:
 * Bottom < Untainted | Tainted < MaybeTainted. Tainted values are associated
 * with the source position of their taint */
import scala.util.parsing.input.Position
trait TaintStatus
case class Tainted(sources: Set[Position]) extends TaintStatus
case object Untainted extends TaintStatus
case class MaybeTainted(sources: Set[Position]) extends TaintStatus
case object BottomTaint extends TaintStatus

/* We need support for extra operations in our lattice, namely we need to taint
 * values, to sanitize them, and to retrieve their taint status */
trait TaintLattice[L] extends AbstractValue[L] {
  def taint(x: L, source: Position): L
  def sanitize(x: L): L
  def taintStatus(x: L): TaintStatus
}

class TaintLatticeImpl[Abs : AbstractValue] extends Lattice {
  type L = (TaintStatus, Abs)
  val abs = implicitly[AbstractValue[Abs]]
  implicit val isTaintLattice: TaintLattice[L] = new TaintLattice[L] {
    def taint(x: L, source: Position) = x._1 match {
      case BottomTaint => x /* cannot taint bottom */
      case Untainted => (Tainted(Set(source)), x._2)
      case Tainted(sources) => (Tainted(sources + source), x._2)
      case MaybeTainted(sources) => (MaybeTainted(sources + source), x._2)
    }
    def sanitize(x: L) = x._1 match {
      case BottomTaint => (BottomTaint, x._2) /* cannot sanitize bottom */
      case Untainted => (Untainted, x._2)
      case Tainted(_) => (Untainted, x._2)
      case MaybeTainted(_) => (Untainted, x._2)
    }
    def taintStatus(x: L) = x._1

    def joinTaint(x: TaintStatus, y: TaintStatus): TaintStatus = (x, y) match {
      case (BottomTaint, _) => y
      case (_, BottomTaint) => x
      case (Untainted, Untainted) => Untainted
      case (MaybeTainted(_), Untainted) => x
      case (Untainted, MaybeTainted(_)) => y
      case (MaybeTainted(s), MaybeTainted(s2)) => MaybeTainted(s ++ s2)
      case (MaybeTainted(s), Tainted(s2)) => MaybeTainted(s ++ s2)
      case (Tainted(s), MaybeTainted(s2)) => MaybeTainted(s ++ s2)
      case (Untainted, Tainted(s)) => MaybeTainted(s)
      case (Tainted(s), Untainted) => MaybeTainted(s)
    }
    def bottom = (BottomTaint, abs.bottom)
    def join(x: L, y: L) = (joinTaint(x._1, y._1), abs.join(x._2, y._2))
    def subsumes(x: L, y: L) = abs.subsumes(x._2, y._2) && (if (x._1 == y._1) { true } else {
      (x._1, y._1) match {
        case (_, BottomTaint) => true
        case (BottomTaint, _) => false
        case (MaybeTainted(s), MaybeTainted(s2)) => s2.subsetOf(s)
        case (Tainted(s), Tainted(s2)) => s2.subsetOf(s)
        case (MaybeTainted(s), Tainted(s2)) => s2.subsetOf(s)
        case (Tainted(_), MaybeTainted(_)) => false
        case (MaybeTainted(_), Untainted) => true
        case (Untainted, MaybeTainted(_)) => false
        case (Untainted, Tainted(_)) => false
        case (Tainted(_), Untainted) => false
      }
    })
    val name = "Taint(${abs.name})"
    val counting = abs.counting

    def isError(x: L) = abs.isError(x._2)
    def isPrimitiveValue(x: L) = abs.isPrimitiveValue(x._2)
    def isTrue(x: L) = abs.isTrue(x._2)
    def isFalse(x: L) = abs.isFalse(x._2)
    def unaryOp(op: SchemeOps.UnaryOperator)(x: L): L = (x._1, abs.unaryOp(op)(x._2))
    def binaryOp(op: SchemeOps.BinaryOperator)(x: L, y: L): L = (joinTaint(x._1, y._1), abs.binaryOp(op)(x._2, y._2))
    def and(x: L, y: => L): L = (joinTaint(x._1, y._1), abs.and(x._2, y._2))
    def or(x: L, y: => L): L = (joinTaint(x._1, y._1), abs.or(x._2, y._2))
    def car[Addr : Address](x: L) = abs.car[Addr](x._2)
    def cdr[Addr : Address](x: L) = abs.cdr[Addr](x._2)
    def vectorRef[Addr : Address](vector: L, index: L): Set[Either[L, Addr]] = ???
    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): (L, Set[Addr]) = ???
    def inject(x: Int): L = (Untainted, abs.inject(x))
    def inject(x: Float): L = (Untainted, abs.inject(x))
    def inject(x: String): L = (Untainted, abs.inject(x))
    def inject(x: Char): L = (Untainted, abs.inject(x))
    def inject(x: Boolean): L = (Untainted, abs.inject(x))
    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): L = (Untainted, abs.inject[Addr, Abs](x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = (Untainted, abs.inject[Exp, Addr](x))
    def injectSymbol(x: String): L = (Untainted, abs.injectSymbol(x))
    def cons[Addr : Address](car: Addr, cdr: Addr): L = (Untainted, abs.cons[Addr](car, cdr))
    def vector[Addr : Address](addr: Addr, size: L, init: Addr): (L, L) = ???
    def nil: L = (Untainted, abs.nil)
    def error(x: L): L = (x._1, abs.error(x._2))

    def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])] = abs.getClosures(x._2)
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L): Set[Primitive[Addr, Abs]] = abs.getPrimitives(x._2)
    def getVectors[Addr : Address](x: L): Set[Addr] = abs.getVectors(x._2)

    /* Stubs until ConcurrentSchemeLattice is removed from AbstractValue */
    def getLocks[Addr : Address](x: L): Set[Addr] = ???
    def getTids[TID : ThreadIdentifier](x: L): Set[TID] = ???
    def injectTid[TID : ThreadIdentifier](tid: TID): L = ???
    def lock[Addr : Address](addr: Addr): L = ???
    def lockedValue: L = ???
    def unlockedValue: L = ???
  }
  val isAbstractValue: AbstractValue[L] = isTaintLattice
}

/* We need to extend the language with primitives representing sources, sinks, and sanitizers */
class TSchemePrimitives[Addr : Address, Abs : TaintLattice] extends SchemePrimitives[Addr, Abs] {
  val tabs = implicitly[TaintLattice[Abs]]
  object Taint extends Primitive[Addr, Abs] {
    val name = "taint"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => Right((tabs.taint(x, implicitly[Expression[Exp]].pos(fexp)), store, Set()))
      case l => Left("taint: 1 operand expected, got ${l.size} instead")
    }
  }
  object Sink extends Primitive[Addr, Abs] {
    val name = "sink"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => tabs.taintStatus(x) match {
        case Untainted => Right((x, store, Set()))
        case MaybeTainted(sources) => Left(s"sink: called with a maybe tainted value originating from $sources, sink at ${implicitly[Expression[Exp]].pos(fexp)}")
        case Tainted(sources) => Left(s"sink: called with a tainted value originating from $sources, sink at ${implicitly[Expression[Exp]].pos(fexp)}")
        case BottomTaint => Right(x, store, Set())
      }
      case l => Left("sink: 1 operand expected, got ${l.size} instead")
    }
  }
  object Sanitize extends Primitive[Addr, Abs] {
    val name = "sanitize"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => Right((tabs.sanitize(x), store, Set()))
      case l => Left("sanitize: 1 operand expected, got ${l.size} instead")
    }
  }
  override def all = super.all ++ List(Taint, Sink, Sanitize)
}

/* The analysis itself only collects the error strings starting with "sink: " */
case class TaintAnalysis[Abs : JoinLattice, Addr : Address, Time : Timestamp]()
    extends BaseAnalysis[Set[String], SchemeExp, Abs, Addr, Time] {
  def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: Set[String]) = current
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: Set[String]) = current
  def errorValue(v: Abs, current: Set[String]) = current
  def errorState(reason: String, current: Set[String]) = if (reason.startsWith("sink: ")) { current + reason } else { current }
  def join(x: Set[String], y: Set[String]) = x ++ y
  def init = Set[String]()
}

/* We can finally run the analysis and detect when a tanted value flows to a sink */
object TaintAnalysis {
  def analyze[L : TaintLattice](program: String): Set[String] = {
    val sem = new SchemeSemantics[L, ClassicalAddress.A, ZeroCFA.T](new TSchemePrimitives[ClassicalAddress.A, L])
    val machine = new AAM[SchemeExp, L, ClassicalAddress.A, ZeroCFA.T]
    val analysis = TaintAnalysis[L, ClassicalAddress.A, ZeroCFA.T]
    machine.analyze(sem.parse(program), sem, analysis, None) match {
      case Some(v) => v
      case None => println("Analysis did not succeed..."); Set()
    }
  }
  def main(args: Array[String]) {
    if (args.length >= 1) {
      val cpLattice = new ConstantPropagationLattice(false)
      implicit val isAbstractValue = cpLattice.isAbstractValue
      val taintLattice = new TaintLatticeImpl[cpLattice.L]()
      implicit val isTaintLattice = taintLattice.isTaintLattice
      val errors = analyze[taintLattice.L](args(0))
      if (errors.isEmpty) {
        println("No taint errors detected")
      } else {
        errors.foreach(println _)
      }
    } else {
      println("Please provide input program as argument")
    }
  }
}
