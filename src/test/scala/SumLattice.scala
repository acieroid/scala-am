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
  case class Left(x: X) extends Sum
  case class Right(y: Y) extends Sum

  implicit object SumAbstractValue extends AbstractValue[Sum] {
    private def err(reason: String) = SumInjection.error(SumInjection.inject(reason))

    def isTrue(s: Sum) = s match {
      case Left(x) => xabs.isTrue(x)
      case Right(y) => yabs.isTrue(y)
    }
    def isFalse(s: Sum) = s match {
      case Left(x) => xabs.isFalse(x)
      case Right(y) => yabs.isFalse(y)
    }
    def isError(s: Sum) = s match {
      case Left(x) => xabs.isError(x)
      case Right(y) => yabs.isError(y)
    }
    def unaryOp(op: UnaryOperator)(s: Sum) = s match {
      case Left(x) => Left(xabs.unaryOp(op)(x))
      case Right(y) => Right(yabs.unaryOp(op)(y))
    }
    def binaryOp(op: BinaryOperator)(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.binaryOp(op)(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.binaryOp(op)(y1, y2))
      case _ => err("binary operation ($op) on sum lattice cannot mix elements from two lattices: $s1 and $s2")
    }
    def foldValues[B](s: Sum, f: Sum => Set[B]) = s match {
      case Left(x) => xabs.foldValues(x, (x) => f(Left(x)))
      case Right(y) => yabs.foldValues(y, (y) => f(Right(y)))
    }
    def join(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.join(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.join(y1, y2))
      case _ => err("cannot join different elements of a sum lattice: $s1 and $s2")
    }
    def meet(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.meet(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.meet(y1, y2))
      case (Left(_), Right(_)) => SumInjection.bottom
      case (Right(_), Left(_)) => SumInjection.bottom
    }
    def subsumes(s1: Sum, s2: Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => xabs.subsumes(x1, x2)
      case (Right(y1), Right(y2)) => yabs.subsumes(y1, y2)
      case _ => false
    }
    def and(s1: Sum, s2: => Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.and(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.and(y1, y2))
      case _ => ??? /* should be implementable by using isTrue and isFalse */
    }
    def or(s1: Sum, s2: => Sum) = (s1, s2) match {
      case (Left(x1), Left(x2)) => Left(xabs.or(x1, x2))
      case (Right(y1), Right(y2)) => Right(yabs.or(y1, y2))
      case _ => ??? /* also, should be implementable by using isTrue and isFalse */
    }
    def car[Addr : Address](s: Sum) = s match {
      case Left(x) => xabs.car[Addr](x)
      case Right(y) => yabs.car[Addr](y)
    }
    def cdr[Addr : Address](s: Sum) = s match {
      case Left(x) => xabs.cdr[Addr](x)
      case Right(y) => yabs.cdr[Addr](y)
    }
    def toString[Addr : Address](s: Sum, store: Store[Addr, Sum]) = s.toString
    def getClosures[Exp : Expression, Addr : Address](s: Sum) = s match {
      case Left(x) => xabs.getClosures[Exp, Addr](x)
      case Right(y) => yabs.getClosures[Exp, Addr](y)
    }
    def getPrimitive[Addr : Address](s: Sum) = s match {
      case Left(x) => ??? // TODO
      case Right(y) => ??? // TODO
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
    def inject(x: Int) = Left(xabsi.inject(x))
    def inject(x: String) = Left(xabsi.inject(x))
    def inject(x: Char) = Left(xabsi.inject(x))
    def inject(x: Boolean) = Left(xabsi.inject(x))
    def inject[Addr : Address](x: Primitive[Addr, Sum]) = ??? // TODO: have a primitive element
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = Left(xabsi.inject[Exp, Addr](x))
    def injectSymbol(x: String) = Left(xabsi.injectSymbol(x))
    def nil = Left(xabsi.nil)
    def cons[Addr : Address](car: Addr, cdr: Addr) = Left(xabsi.cons[Addr](car, cdr))
  }
}
