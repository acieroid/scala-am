import scalaz.Scalaz._
import scalaz._
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
trait IsTaintLattice[L] extends IsSchemeLattice[L] {
  def taint(x: L, source: Position): L
  def sanitize(x: L): L
  def taintStatus(x: L): TaintStatus
}

class TaintLattice[Abs : IsSchemeLattice] extends SchemeLattice {
  type L = (TaintStatus, Abs)
  val abs = implicitly[IsSchemeLattice[Abs]]
  implicit val isTaintLattice: IsTaintLattice[L] = new IsTaintLattice[L] {
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

    def isPrimitiveValue(x: L) = abs.isPrimitiveValue(x._2)
    def isTrue(x: L) = abs.isTrue(x._2)
    def isFalse(x: L) = abs.isFalse(x._2)
    def unaryOp(op: SchemeOps.UnaryOperator)(x: L): MayFail[L] =
      abs.unaryOp(op)(x._2).map(res => (x._1, res))
    def binaryOp(op: SchemeOps.BinaryOperator)(x: L, y: L): MayFail[L] =
      abs.binaryOp(op)(x._2, y._2).map(res => (joinTaint(x._1, y._1), res))
    def car[Addr : Address](x: L) = abs.car[Addr](x._2)
    def cdr[Addr : Address](x: L) = abs.cdr[Addr](x._2)
    def vectorRef[Addr : Address](vector: L, index: L): MayFail[Set[Addr]] = abs.vectorRef(vector._2, index._2)
    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])] = abs.vectorSet(vector._2, index._2, addr).map({
      case (res, addrs) => ((Untainted, res), addrs) })
    def inject(x: Int): L = (Untainted, abs.inject(x))
    def inject(x: Float): L = (Untainted, abs.inject(x))
    def inject(x: String): L = (Untainted, abs.inject(x))
    def inject(x: Char): L = (Untainted, abs.inject(x))
    def inject(x: Boolean): L = (Untainted, abs.inject(x))
    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): L = (Untainted, abs.inject[Addr, Abs](x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = (Untainted, abs.inject[Exp, Addr](x))
    def injectSymbol(x: String): L = (Untainted, abs.injectSymbol(x))
    def cons[Addr : Address](car: Addr, cdr: Addr): L = (Untainted, abs.cons[Addr](car, cdr))
    def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)] = abs.vector(addr, size._2, init).map({
      case (v, va) => ((Untainted, v), (Untainted, va)) })
    def nil: L = (Untainted, abs.nil)

    def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])] = abs.getClosures(x._2)
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L): Set[Primitive[Addr, Abs]] = abs.getPrimitives(x._2)
    def getVectors[Addr : Address](x: L): Set[Addr] = abs.getVectors(x._2)
  }
  val isSchemeLattice: IsSchemeLattice[L] = isTaintLattice
}

case class TaintError(sources: Set[Position], sink: Position) extends SemanticError

/* We need to extend the language with primitives representing sources, sinks, and sanitizers */
class TSchemePrimitives[Addr : Address, Abs : IsTaintLattice] extends SchemePrimitives[Addr, Abs] {
  val tabs = implicitly[IsTaintLattice[Abs]]
  object Taint extends Primitive[Addr, Abs] {
    val name = "taint"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => MayFailSuccess((tabs.taint(x, implicitly[Expression[Exp]].pos(fexp)), store, Set()))
      case l => MayFailError(List(ArityError(name, 1, l.size)))
    }
  }
  object Sink extends Primitive[Addr, Abs] {
    val name = "sink"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => tabs.taintStatus(x) match {
        case Untainted => MayFailSuccess((x, store, Set()))
        case MaybeTainted(sources) => MayFailBoth((x, store, Set()), List(TaintError(sources, implicitly[Expression[Exp]].pos(fexp))))
        case Tainted(sources) => MayFailError(List(TaintError(sources, implicitly[Expression[Exp]].pos(fexp))))
        case BottomTaint => MayFailSuccess(x, store, Set())
      }
      case l => MayFailError(List(ArityError(name, 1, l.size)))
    }
  }
  object Sanitize extends Primitive[Addr, Abs] {
    val name = "sanitize"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => MayFailSuccess((tabs.sanitize(x), store, Set()))
      case l => MayFailError(List(ArityError(name, 1, l.size)))
    }
  }
  override def all = super.all ++ List(Taint, Sink, Sanitize)
}

/* The analysis itself only collects the error strings starting with "sink: " */
case class TaintAnalysis[Abs : JoinLattice, Addr : Address, Time : Timestamp]()
    extends BaseAnalysis[Set[(Position, Position)], SchemeExp, Abs, Addr, Time] {
  def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: Set[(Position, Position)]) = current
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: Set[(Position, Position)]) = current
  def error(error: SemanticError, current: Set[(Position, Position)]) = error match {
    case TaintError(sources, sink) => current ++ sources.map(source => (source, sink))
    case _ => current
  }
  def join(x: Set[(Position, Position)], y: Set[(Position, Position)]) = x ++ y
  def init = Set[(Position, Position)]()
}

/* We can finally run the analysis and detect when a tainted value flows to a sink */
object TaintAnalysis {
  def analyze[L : IsTaintLattice](program: String): Set[(Position, Position)] = {
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
      implicit val isSchemeLattice = cpLattice.isSchemeLattice
      val taintLattice = new TaintLattice[cpLattice.L]()
      implicit val isTaintLattice = taintLattice.isTaintLattice
      val errors = analyze[taintLattice.L](args(0))
      if (errors.isEmpty) {
        println("No taint errors detected")
      } else {
        errors.foreach({ case (source, sink) => println(s"tainted value flows from source at position $source to sink at position $sink") })
      }
    } else {
      println("Please provide input program as argument")
    }
  }
}
