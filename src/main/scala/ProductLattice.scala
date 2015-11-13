import UnaryOperator._
import BinaryOperator._

/**
 * Product lattice, combines two lattices X and Y as a product (X, Y).
 * Here's an example on how to use it, to combine the type lattice with the typeset lattice:
 *   val prod = new ProductLattice[AbstractType, AbstractTypeSet] // create the product lattice
 *   import prod._ // import its elements (and most importantly, Product)
 *   run(new Free[SchemeExp, Product, ClassicalAddress], new SchemeSemantics[Product, ClassicalAddress]) _ // run a machine with it
 */
class ProductLattice[X : AbstractValue, Y : AbstractValue] {
  val xabs = implicitly[AbstractValue[X]]
  val yabs = implicitly[AbstractValue[Y]]

  trait Product
  case class Prim[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends Product {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Prod(x: X, y: Y) extends Product

  implicit object ProductAbstractValue extends AbstractValue[Product] {
    def name = s"(${xabs.name}, ${yabs.name})"

    private def err(reason: String) = error(inject(reason))

    def isTrue(p: Product) = p match {
      case Prod(x, y) => xabs.isTrue(x) || yabs.isTrue(y)
      case Prim(_) => true
    }
    def isFalse(p: Product) = p match {
      case Prod(x, y) => xabs.isFalse(x) || yabs.isFalse(y)
      case Prim(_) => false
    }
    def isError(p: Product) = p match {
      case Prod(x, y) => xabs.isError(x) || yabs.isError(y)
      case Prim(_) => false
    }
    def unaryOp(op: UnaryOperator)(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.unaryOp(op)(x), yabs.unaryOp(op)(y))
      case Prim(_) => op match {
        case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger => inject(false)
        case _ => err(s"operator $op cannot work on primitive (argument was $p)")
      }
    }
    def binaryOp(op: BinaryOperator)(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.binaryOp(op)(x1, x2), yabs.binaryOp(op)(y1, y2))
      case _ => err("operator $op cannot work on primitives (arguments were $p1 and $p2)")
    }
    def foldValues[B](p: Product, f: Product => Set[B]) = ???
    def join(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.join(x1, x2), yabs.join(y1, y2))
      case _ => ???
    }
    def meet(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.meet(x1, x2), yabs.meet(y1, y2))
      case _ => ???
    }
    def subsumes(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => xabs.subsumes(x1, x2) && yabs.subsumes(y1, y2)
      case (Prim(prim1), Prim(prim2)) => prim1 == prim2
      case _ => false
    }
    def and(p1: Product, p2: => Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.and(x1, x2), yabs.and(y1, y2))
      case _ => err(s"and cannot work on primitives (arguments were $p1 and $p2)")
    }
    def or(p1: Product, p2: => Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.or(x1, x2), yabs.or(y1, y2))
      case _ => err(s"or cannot work on primitives (arguments were $p1 and $p2)")
    }
    def car[Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.car[Addr](x) ++ yabs.car[Addr](y)
      case _ => Set[Addr]()
    }
    def cdr[Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.cdr[Addr](x) ++ yabs.cdr[Addr](y)
      case _ => Set[Addr]()
    }
    def toString[Addr : Address](p: Product, store: Store[Addr, Product]) = p.toString // s"(${xabs.toString(p.x, store)}, ${yabs.toString(p.y, store)})"
    def getClosures[Exp : Expression, Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.getClosures[Exp, Addr](x) ++ yabs.getClosures[Exp, Addr](y)
      case Prim(_) => Set()
    }
    def getPrimitive[Addr : Address, Abs : AbstractValue](p: Product) = p match {
      case Prim(prim: Primitive[Addr, Abs]) => Some(prim)
      case _ => None
    }

    def bottom = Prod(xabs.bottom, yabs.bottom)
    def error(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.error(x), yabs.error(y))
      case Prim(_) => error(inject(p.toString))
    }
    def inject(x: Int) = Prod(xabs.inject(x), yabs.inject(x))
    def inject(x: String) = Prod(xabs.inject(x), yabs.inject(x))
    def inject(x: Char) = Prod(xabs.inject(x), yabs.inject(x))
    def inject(x: Boolean) = Prod(xabs.inject(x), yabs.inject(x))
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = Prod(xabs.inject[Exp, Addr](x), yabs.inject[Exp, Addr](x))
    def injectSymbol(x: String) = Prod(xabs.injectSymbol(x), yabs.injectSymbol(x))
    def nil = Prod(xabs.nil, yabs.nil)
    def cons[Addr : Address](car: Addr, cdr: Addr) = Prod(xabs.cons[Addr](car, cdr), yabs.cons[Addr](car, cdr))
  }
}
