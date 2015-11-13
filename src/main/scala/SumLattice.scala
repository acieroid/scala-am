import UnaryOperator._
import BinaryOperator._

/**
 * Sum of two lattices. Uses elements of lattice X first when there is a
 * decision (for injection, for the bottom element, primitives and closures)
 *
 * Lattice: X || Y > Bottom (no top element)
 */
class SumLattice[X, Y]
  (implicit xabs: AbstractValue[X], xabsi: AbstractInjection[X],
    yabs: AbstractValue[Y], yabsi: AbstractInjection[Y]) {
  trait Sum
  case class Prim[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends Sum {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Left(x: X) extends Sum
  case class Right(y: Y) extends Sum

  implicit object SumAbstractValue extends AbstractValue[Sum] {
    private def err(reason: String) = SumInjection.error(SumInjection.inject(reason))

    def isTrue(s: Sum) = s match {
      case Left(x) => xabs.isTrue(x)
      case Right(y) => yabs.isTrue(y)
      case Prim(_) => true
    }
    def isFalse(s: Sum) = s match {
      case Left(x) => xabs.isFalse(x)
      case Right(y) => yabs.isFalse(y)
      case Prim(_) => false
    }
    def isError(s: Sum) = s match {
      case Left(x) => xabs.isError(x)
      case Right(y) => yabs.isError(y)
      case Prim(_) => false
    }
    def unaryOp(op: UnaryOperator)(s: Sum) = s match {
      case Left(x) => Left(xabs.unaryOp(op)(x))
      case Right(y) => Right(yabs.unaryOp(op)(y))
      case Prim(_) => err("unary operation ($op) performed on primitive $s")
    }
    def binaryOp(op: BinaryOperator)(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.binaryOp(op)(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.binaryOp(op)(y1, y2))
      case _ => err("binary operation ($op) on sum lattice cannot mix elements from two lattices: $s1 and $s2")
    }
    def foldValues[B](s: Sum, f: Sum => Set[B]) = s match {
      case Left(x) => xabs.foldValues(x, (x) => f(Left(x)))
      case Right(y) => yabs.foldValues(y, (y) => f(Right(y)))
      case Prim(_) => f(s)
    }
    def join(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.join(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.join(y1, y2))
      case _ => err("cannot join different elements of a sum lattice: $s1 and $s2")
    }
    def meet(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.meet(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.meet(y1, y2))
      case _ => SumInjection.bottom
    }
    def subsumes(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => xabs.subsumes(x1, x2)
      case (Right(y1), Right(y2)) => yabs.subsumes(y1, y2)
      case (Prim(p1), Prim(p2)) => p1 == p2
      case _ => false
    }
    def and(s1: Sum, s2: => Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.and(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.and(y1, y2))
      case _ => err("and used on two different element of a sum lattice: $s1 and $s2")
    }
    def or(s1: Sum, s2: => Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.or(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.or(y1, y2))
      case _ => err("or used on two different element of a sum lattice: $s1 and $s2")
    }
    def car[Addr : Address](s: Sum) = s match {
      case Left(x) => xabs.car[Addr](x)
      case Right(y) => yabs.car[Addr](y)
      case Prim(_) => Set[Addr]()
    }
    def cdr[Addr : Address](s: Sum) = s match {
      case Left(x) => xabs.cdr[Addr](x)
      case Right(y) => yabs.cdr[Addr](y)
      case Prim(_) => Set[Addr]()
    }
    def toString[Addr : Address](s: Sum, store: Store[Addr, Sum]) = s.toString
    def getClosures[Exp : Expression, Addr : Address](s: Sum) = s match {
      case Left(x) => xabs.getClosures[Exp, Addr](x)
      case Right(y) => yabs.getClosures[Exp, Addr](y)
      case Prim(_) => Set()
    }
    def getPrimitive[Addr : Address, Abs : AbstractValue](s: Sum) = s match {
      case Prim(p: Primitive[Addr, Abs]) => Some(p)
      case _ => None
    }
  }
  implicit object SumInjection extends AbstractInjection[Sum] {
    def name = s"(${xabsi.name} | ${xabsi.name})"
    def bottom = Left(xabsi.bottom)
    def error(s: Sum) = s match {
      case Left(x) => Left(xabsi.error(x))
      case Right(y) => Right(yabsi.error(y))
    }
    /* TODO: have an execption raised by inject if a lattice doesn't support some
     * type of elements, and fallback on Y when it is the case */
    def inject(x: Int) = try {
      Left(xabsi.inject(x))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.inject(x))
    }
    def inject(x: String) = try {
      Left(xabsi.inject(x))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.inject(x))
    }
    def inject(x: Char) = try {
      Left(xabsi.inject(x))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.inject(x))
    }
    def inject(x: Boolean) = try {
      Left(xabsi.inject(x))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.inject(x))
    }
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = try {
      Left(xabsi.inject[Addr, Abs](x))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.inject[Addr, Abs](x))
    }
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = try {
      Left(xabsi.inject[Exp, Addr](x))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.inject[Exp, Addr](x))
    }
    def injectSymbol(x: String) = try {
      Left(xabsi.injectSymbol(x))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.injectSymbol(x))
    }
    def nil = try {
      Left(xabsi.nil)
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.nil)
    }
    def cons[Addr : Address](car: Addr, cdr: Addr) = try {
      Left(xabsi.cons[Addr](car, cdr))
    } catch {
      case UnsupportedLatticeElement =>
        Right(yabsi.cons[Addr](car, cdr))
    }
  }
}
