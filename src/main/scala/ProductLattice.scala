class ProductLattice[X, Y]
  (implicit xabs: AbstractValue[X], xabsi: AbstractInjection[X],
    yabs: AbstractValue[Y], yabsi: AbstractInjection[Y]) {
  trait Product
  case class Prim[Addr : Address](prim: Primitive[Addr, Product]) extends Product {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Prod(x: X, y: Y) extends Product
  implicit object ProductAbstractValue extends AbstractValue[Product] {
    def isTrue(p: Product) = p match {
      case Prod(x, y) => xabs.isTrue(x) || yabs.isTrue(y) /* TODO: && or || ? */
      case Prim(_) => true
    }
    def isFalse(p: Product) = p match {
      case Prod(x, y) => xabs.isFalse(x) || yabs.isFalse(y) /* TODO: && or || ? */
      case Prim(_) => false
    }
    def isError(p: Product) = p match {
      case Prod(x, y) => xabs.isError(x) || yabs.isError(y)
      case Prim(_) => false
    }
    def isNull(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.isNull(x), yabs.isNull(y))
      case Prim(_) => Prod(xabsi.inject(false), yabsi.inject(false))
    }
    def isCons(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.isCons(x), yabs.isCons(y))
      case Prim(_) => Prod(xabsi.inject(false), yabsi.inject(false))
    }
    def isChar(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.isChar(x), yabs.isChar(y))
      case Prim(_) => Prod(xabsi.inject(false), yabsi.inject(false))
    }
    def isSymbol(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.isSymbol(x), yabs.isSymbol(y))
      case Prim(_) => Prod(xabsi.inject(false), yabsi.inject(false))
    }
    def isString(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.isString(x), yabs.isString(y))
      case Prim(_) => Prod(xabsi.inject(false), yabsi.inject(false))
    }
    def isInteger(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.isInteger(x), yabs.isInteger(y))
      case Prim(_) => Prod(xabsi.inject(false), yabsi.inject(false))
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
    def plus(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.plus(x1, x2), yabs.plus(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def minus(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.minus(x1, x2), yabs.minus(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def times(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.times(x1, x2), yabs.times(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def div(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.div(x1, x2), yabs.div(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def modulo(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.modulo(x1, x2), yabs.modulo(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def ceiling(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.ceiling(x), yabs.ceiling(y))
      case _ => ??? /* TODO: error */
    }
    def log(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.log(x), yabs.log(y))
      case _ => ??? /* TODO: error */
    }
    def lt(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.lt(x1, x2), yabs.lt(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def numEq(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.numEq(x1, x2), yabs.numEq(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def not(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.not(x), yabs.not(y))
    }
    def and(p1: Product, p2: => Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.and(x1, x2), yabs.and(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def or(p1: Product, p2: => Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.or(x1, x2), yabs.or(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def eq(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.eq(x1, x2), yabs.eq(y1, y2))
      case _ => ??? /* TODO: error */
    }
    def car[Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.car[Addr](x) ++ yabs.car[Addr](y)
      case _ => Set[Addr]()
    }
    def cdr[Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.cdr[Addr](x) ++ yabs.cdr[Addr](y)
      case _ => Set[Addr]()
    }
    def random(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.random(x), yabs.random(y))
      case _ => ??? /* TODO: error */
    }
    def toString[Addr : Address](p: Product, store: Store[Addr, Product]) = ??? // s"(${xabs.toString(p.x, store)}, ${yabs.toString(p.y, store)})"
    def getClosures[Exp : Expression, Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.getClosures[Exp, Addr](x) ++ yabs.getClosures[Exp, Addr](y)
      case Prim(_) => Set()
    }
    def getPrimitive[Addr : Address](p: Product) = p match {
      case Prim(prim: Primitive[Addr, Product]) => Some(prim)
      case _ => None
    }
  }
  implicit object ProductInjection extends AbstractInjection[Product] {
    def name = s"(${xabsi.name}, ${yabsi.name})"
    def bottom = Prod(xabsi.bottom, yabsi.bottom)
    def error(p: Product) = p match {
      case Prod(x, y) => Prod(xabsi.error(x), yabsi.error(y))
      case Prim(_) => ???
    }
    def inject(x: Int) = Prod(xabsi.inject(x), yabsi.inject(x))
    def inject(x: String) = Prod(xabsi.inject(x), yabsi.inject(x))
    def inject(x: Char) = Prod(xabsi.inject(x), yabsi.inject(x))
    def inject(x: Boolean) = Prod(xabsi.inject(x), yabsi.inject(x))
    def inject[Addr : Address](x: Primitive[Addr, Product]) = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = Prod(xabsi.inject[Exp, Addr](x), yabsi.inject[Exp, Addr](x))
    def injectSymbol(x: String) = Prod(xabsi.injectSymbol(x), yabsi.injectSymbol(x))
    def nil = Prod(xabsi.nil, yabsi.nil)
    def cons[Addr : Address](car: Addr, cdr: Addr) = Prod(xabsi.cons[Addr](car, cdr), yabsi.cons[Addr](car, cdr))
  }
}
