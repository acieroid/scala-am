import UnaryOperator._
import BinaryOperator._

/**
 * Transform a lattice of elements into a lattice of sets of elements
 */
class PowerSetLattice(lattice: Lattice) extends Lattice {
  private type X = lattice.L
  private val abs = lattice.isAbstractValue

  trait El
  type L = El
  private case class Element(x: X) extends L {
    override def toString = x.toString
  }
  private case class Elements(xs: Set[X]) extends L {
    require(xs.size > 0, "Power set element created with no elements inside, X's bottom should be used instead")
    override def toString = "{" + xs.mkString(", ") + "}"
  }

  implicit val isAbstractValue = new AbstractValue[L] {
    def name = s"P(${abs.name})"
    def isTrue(p: L) = p match {
      case Element(x) => abs.isTrue(x)
      case Elements(xs) => xs.exists(x => abs.isTrue(x))
    }
    def isFalse(p: L) = p match {
      case Element(x) => abs.isFalse(x)
      case Elements(xs) => xs.exists(x => abs.isFalse(x))
    }
    def isError(p: L) = p match {
      case Element(x) => abs.isError(x)
      case Elements(xs) => {
        val result = xs.exists(x => abs.isError(x))
        require(!result || xs.forall(x => abs.isError(x)), s"Power set element combines error values with non-error values ($xs)")
        result
      }
    }
    private def wrapped(x: => X): L = try {
      Element(x)
    } catch {
      case CannotJoin(x1: X, x2: X) => Elements(Set[X](x1, x2))
    }
    def unaryOp(op: UnaryOperator)(p: L): L = p match {
      case Element(x) => wrapped(abs.unaryOp(op)(x))
      case Elements(xs) => xs.foldLeft(bottom)((acc, x) => join(acc, wrapped(abs.unaryOp(op)(x))))
    }
    def binaryOp(op: BinaryOperator)(p1: L, p2: L): L = (p1, p2) match {
      case (Element(x1), Element(x2)) => wrapped(abs.binaryOp(op)(x1, x2))
      case (Element(x), Elements(xs)) => binaryOp(op)(Elements(Set[X](x)), p2)
      case (Elements(xs), Element(x)) => binaryOp(op)(p1, Elements(Set[X](x)))
      case (Elements(xs1), Elements(xs2)) =>
        xs1.foldLeft(bottom)((acc, x1) => xs2.foldLeft(acc)((acc, x2) => join(acc, wrapped(abs.binaryOp(op)(x1, x2)))))
    }
    def and(p1: L, p2: => L): L = (p1, p2) match {
      case (Element(x1), Element(x2)) => wrapped(abs.and(x1, x2))
      case (Element(x), Elements(xs)) => and(Elements(Set[X](x)), p2)
      case (Elements(xs), Element(x)) => and(p1, Elements(Set[X](x)))
      case (Elements(xs1), Elements(xs2)) =>
        xs1.foldLeft(bottom)((acc, x1) => xs2.foldLeft(acc)((acc, x2) => join(acc, wrapped(abs.and(x1, x2)))))
    }
    def or(p1: L, p2: => L): L = (p1, p2) match {
      case (Element(x1), Element(x2)) => wrapped(abs.or(x1, x2))
      case (Element(x), Elements(xs)) => or(Elements(Set[X](x)), p2)
      case (Elements(xs), Element(x)) => or(p1, Elements(Set[X](x)))
      case (Elements(xs1), Elements(xs2)) =>
        xs1.foldLeft(bottom)((acc, x1) => xs2.foldLeft(acc)((acc, x2) => join(acc, wrapped(abs.or(x1, x2)))))
    }
    def join(p1: L, p2: L): L = (p1, p2) match {
      case (Element(x1), Element(x2)) => wrapped(abs.join(x1, x2))
      case (Elements(xs), Element(x)) => join(p1, Elements(Set[X](x)))
      case (Element(x), Elements(xs)) => join(Elements(Set[X](x)), p2)
      case (Elements(xs1), Elements(xs2)) =>
        /* every element in the other set has to be joined in this set */
        Elements(xs2.foldLeft(xs1)((acc, x2) =>
          if (acc.exists(x1 => abs.subsumes(x1, x2))) { acc } else { acc + x2 }))
    }
    def meet(p1: L, p2: L): L = (p1, p2) match {
      case (Element(x1), Element(x2)) => wrapped(abs.meet(x1, x2))
      case (Elements(xs), Element(x)) => meet(p1, Elements(Set[X](x)))
      case (Element(x), Elements(xs)) => meet(Elements(Set[X](x)), p2)
      case (Elements(xs1), Elements(xs2)) =>
        /* assumption: the elements contained in the set form a flat lattice,
         * e.g., we will not need to compute the meet of {Int} with {1} */
        Elements(xs1.intersect(xs2))
    }
    def subsumes(p1: L, p2: L) = (p1, p2) match {
      case (Element(x1), Element(x2)) => abs.subsumes(x1, x2)
      case (Elements(xs), Element(x2)) => xs.exists(x1 => abs.subsumes(x1, x2))
      case (Element(x1), Elements(xs)) => xs.forall(x2 => abs.subsumes(x1, x2))
      case (Elements(xs1), Elements(xs2)) =>
        /* every element in xs2 should be subsumed by an element of xs1 */
        xs2.forall(x2 => xs1.exists(x1 => abs.subsumes(x1, x2)))
    }
    def car[Addr : Address](p: L) = p match {
      case Element(x) => abs.car[Addr](x)
      case Elements(xs) => xs.flatMap(x => abs.car[Addr](x))
    }
    def cdr[Addr : Address](p: L) = p match {
      case Element(x) => abs.cdr[Addr](x)
      case Elements(xs) => xs.flatMap(x => abs.cdr[Addr](x))
    }
    def toString[Addr : Address](p: L, store: Store[Addr, L]) = p.toString /* TODO */
    def getClosures[Exp : Expression, Addr : Address](p: L) = p match {
      case Element(x) => abs.getClosures[Exp, Addr](x)
      case Elements(xs) => xs.flatMap(x => abs.getClosures[Exp, Addr](x))
    }
    def getPrimitives[Addr : Address, Abs : AbstractValue](p: L) = p match {
      case Element(x) => abs.getPrimitives[Addr, Abs](x)
      case Elements(xs) => xs.flatMap(x => abs.getPrimitives[Addr, Abs](x))
    }
    def getTids[TID : ThreadIdentifier](p: L) = p match {
      case Element(x) => abs.getTids[TID](x)
      case Elements(xs) => xs.flatMap(x => abs.getTids[TID](x))
    }
    def bottom: L = Element(abs.bottom)
    def error(p: L) = p match {
      case Element(x) => Element(abs.error(x))
      case Elements(xs) => xs.foldLeft(bottom)((acc, x) => join(acc, Element(abs.error(x))))
    }
    def inject(x: Int): L = Element(abs.inject(x))
    def inject(x: String): L = Element(abs.inject(x))
    def inject(x: Char): L = Element(abs.inject(x))
    def inject(x: Boolean): L = Element(abs.inject(x))
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]): L = Element(abs.inject[Addr, Abs](x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = Element(abs.inject[Exp, Addr](x))
    def injectTid[TID : ThreadIdentifier](t: TID): L = Element(abs.injectTid[TID](t))
    def injectSymbol(x: String): L = Element(abs.injectSymbol(x))
    def nil: L = Element(abs.nil)
    def cons[Addr : Address](car: Addr, cdr: Addr): L = Element(abs.cons(car, cdr))
  }
}
