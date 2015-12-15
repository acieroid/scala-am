import UnaryOperator._
import BinaryOperator._

/** Type lattice without top element. It distinguishes #t and #f, but also has a
  * bool element (it is therefore *not* a flat lattice) */
object TypeLattice {
  trait Type {
    def unaryOp(op: UnaryOperator): Type = op match {
      case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => False
      case Not => False
      case _ => Error
    }
    def binaryOp(op: BinaryOperator)(that: Type): Type = op match {
      case Eq => throw CannotJoin[Type](True, False)
      case _ => Error
    }
  }
  object Error extends Type {
    override def toString = "error"
    override def unaryOp(op: UnaryOperator) = this
    override def binaryOp(op: BinaryOperator)(that: Type) = this
  }
  object Int extends Type {
    override def toString = "Int"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsInteger => True
      case Ceiling | Log | Random => Int
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: Type) = that match {
      case Int => op match {
        case Plus | Minus | Times | Div | Modulo => Int
        case Lt | NumEq => throw CannotJoin[Type](True, False)
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  object String extends Type {
    override def toString = "String"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsString => True
      case _ => super.unaryOp(op)
    }
  }
  object Char extends Type {
    override def toString = "Char"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsChar => True
      case _ => super.unaryOp(op)
    }
  }
  object Symbol extends Type {
    override def toString = "Symbol"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsSymbol => True
      case _ => super.unaryOp(op)
    }
  }
  object True extends Type {
    override def toString = "#t"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsBoolean => True
      case Not => True
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: Type) = op match {
      case Eq => that match {
        case True => True
        case _ => super.binaryOp(op)(that)
      }
    }
  }
  object False extends Type {
    override def toString = "#f"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsBoolean => True
      case Not => False
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: Type) = op match {
      case Eq => that match {
        case False => True
        case _ => super.binaryOp(op)(that)
      }
    }
  }
  object Bottom extends Type {
    override def toString = "⊥"
    override def unaryOp(op: UnaryOperator) = this
    override def binaryOp(op: BinaryOperator)(that: Type) = this
  }
  case class Prim[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends Type {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Clo[Exp : Expression, Addr : Address](clo: (Exp, Environment[Addr])) extends Type {
    override def toString = "#<clo>"
  }
  case class Tid[TID : ThreadIdentifier](t: TID) extends Type {
    override def toString = s"#<thread $t>"
  }
  object Nil extends Type {
    override def toString = "()"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsNull => True
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: Type) =
      if (op == Eq && that == Nil) { True } else { super.binaryOp(op)(that) }
  }
  case class Cons[Addr : Address](car: Addr, cdr: Addr) extends Type {
    override def toString = "#<cons>"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsCons => True
      case _ => super.unaryOp(op)
    }
  }

  implicit object TypeAbstractValue extends AbstractValue[Type] {
    def name = "Type"
    def isTrue(x: Type) = x match {
      case False => false
      case Error => false
      case Bottom => false
      case _ => true
    }
    def isFalse(x: Type) = x match {
      case False => true
      case _ => false
    }
    def isError(x: Type) = x == Error
    def unaryOp(op: UnaryOperator)(x: Type) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: Type, y: Type) = x.binaryOp(op)(y)
    def foldValues[B](x: Type, f: Type => Set[B]) = f(x)
    def join(x: Type, y: Type) = if (x == y) { x } else { throw CannotJoin[Type](x, y) }
    def meet(x: Type, y: Type) = if (x == y) { x } else { bottom }
    def subsumes(x: Type, y: Type) = x == y || (x == Bottom && (y == True || y == False))
    def and(x: Type, y: => Type) = x match {
      case False => False
      case _ => y
    }
    def or(x: Type, y: => Type) = x match {
      case False => y
      case _ => x
    }
    def car[Addr : Address](x: Type) = x match {
      case Cons(car: Addr, cdr: Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: Type) = x match {
      case Cons(car: Addr, cdr: Addr) => Set(cdr)
      case _ => Set()
    }

    private def toString[Addr : Address](x: Type, store: Store[Addr, Type], inside: Boolean, visited: Set[Type]): String = x match {
      case Cons(car : Addr, cdr : Addr) =>
        if (visited.contains(x)) {
          "#loop"
        } else {
          val carstr =  toString(store.lookup(car), store, false, visited + x)
          val cdrval = store.lookup(cdr)
          val cdrstr =  toString(store.lookup(cdr), store, true, visited + x)
          val content = cdrval match {
            case Nil => s"$carstr"
            case Cons(_, _) => s"$carstr $cdrstr"
            case _ => s"$carstr . $cdrstr"
          }
          if (inside) { content } else { s"($content)" }
        }
      case _ => x.toString
    }
    def toString[Addr : Address](x: Type, store: Store[Addr, Type]) = toString(x, store, false, Set())

    def getClosures[Exp : Expression, Addr : Address](x: Type) = x match {
      case v: Clo[Exp, Addr] => Set(v.clo)
      case _ => Set()
    }
    def getPrimitives[Addr : Address, Abs : AbstractValue](x: Type) = x match {
      case Prim(prim: Primitive[Addr, Abs]) => Set(prim)
      case _ => Set()
    }
    def getTids[TID : ThreadIdentifier](x: Type) = x match {
      case Tid(t: TID) => Set(t)
      case _ => Set()
    }

    def bottom = Bottom
    def error(x: Type) = Error
    def inject(x: Int) = Int
    def inject(x: String) = String
    def inject(x: Boolean) = if (x) { True } else { False }
    def inject(x: Char) = Char
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = Clo[Exp, Addr](x)
    def injectTid[TID : ThreadIdentifier](t: TID) = Tid(t)
    def injectSymbol(x: String) = Symbol
    def nil = Nil
    def cons[Addr : Address](car: Addr, cdr: Addr) = Cons(car, cdr)
  }
}
