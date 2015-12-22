import UnaryOperator._
import BinaryOperator._

/** Type lattice without top element. It distinguishes #t and #f, but has no
  * bool element (this is handled by its powerset lattice) */
object TypeLattice extends Lattice {
  trait Element {
    def unaryOp(op: UnaryOperator): L = op match {
      case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsFloat | IsBoolean | IsVector => False
      case Not => False
      case _ => Error(s"Unsupported unary operation $op on $this")
    }
    def binaryOp(op: BinaryOperator)(that: L): L = op match {
      case Eq => if (this == that) { throw CannotJoin[L](Set(True, False)) } else { False }
      case _ => Error(s"Unsupported binary operation $op on $this and $that")
    }
  }
  type L = Element
  object Error extends L {
    override def toString = "error"
    override def unaryOp(op: UnaryOperator) = this
    override def binaryOp(op: BinaryOperator)(that: L) = this
    def apply(reason: String) = { println(reason); Error }
  }
  object Int extends L {
    override def toString = "Int"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsInteger => True
      case Ceiling | Random => Int
      case Log => Float
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: L) = that match {
      case Int => op match {
        case Plus | Minus | Times | Div | Modulo => Int
        case Lt | NumEq => throw CannotJoin[L](Set(True, False))
        case _ => super.binaryOp(op)(that)
      }
      case Float => op match {
        case Plus | Minus | Times | Div => Float
        case Lt | NumEq => throw CannotJoin[L](Set(True, False))
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  object Float extends L {
    override def toString = "Float"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsFloat => True
      case Ceiling | Log | Random => Float
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: L) = that match {
      case Float => op match {
        case Plus | Minus | Times | Div => Float
        case Lt | NumEq => throw CannotJoin[L](Set(True, False))
        case _ => super.binaryOp(op)(that)
      }
      case Int => op match {
        case Plus | Minus | Times | Div | Modulo => Float
        case Lt | NumEq => throw CannotJoin[L](Set(True, False))
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  object String extends L {
    override def toString = "String"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsString => True
      case StringLength => Int
      case _ => super.unaryOp(op)
    }
  }
  object Char extends L {
    override def toString = "Char"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsChar => True
      case _ => super.unaryOp(op)
    }
  }
  object Symbol extends L {
    override def toString = "Symbol"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsSymbol => True
      case _ => super.unaryOp(op)
    }
  }
  object True extends L {
    override def toString = "#t"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsBoolean => True
      case Not => False
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: L) = op match {
      case Eq => that match {
        case True => True
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  object False extends L {
    override def toString = "#f"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsBoolean => True
      case Not => True
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: L) = op match {
      case Eq => that match {
        case False => True
        case _ => super.binaryOp(op)(that)
      }
    }
  }
  object Bottom extends L {
    override def toString = "⊥"
    override def unaryOp(op: UnaryOperator) = this
    override def binaryOp(op: BinaryOperator)(that: L) = this
  }
  case class Prim[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends L {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Clo[Exp : Expression, Addr : Address](clo: (Exp, Environment[Addr])) extends L {
    override def toString = "#<clo>"
  }
  case class Tid[TID : ThreadIdentifier](t: TID) extends L {
    override def toString = s"#<thread $t>"
  }
  object Nil extends L {
    override def toString = "()"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsNull => True
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: L) =
      if (op == Eq && that == Nil) { True } else { super.binaryOp(op)(that) }
  }
  case class Cons[Addr : Address](car: Addr, cdr: Addr) extends L {
    override def toString = "#<cons>"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsCons => True
      case _ => super.unaryOp(op)
    }
  }
  case class VectorAddress[Addr : Address](addr : Addr) extends L {
    override def unaryOp(op: UnaryOperator) = op match {
      case IsVector => True
      case _ => super.unaryOp(op)
    }
  }
  /** Since ints are abstracted, all elements are joined together and access
    * returns the join of all possible elements of the vector */
  case class Vector(content: Set[L]) extends L {
    override def unaryOp(op: UnaryOperator) = op match {
      case IsVector => True
      case VectorLength => Int
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: L) = op match {
      case VectorRef => that match {
        case Int => content.size match {
          case 0 => Bottom
          case 1 => content.head
          case _ => throw CannotJoin[L](content)
        }
        case _ => Error(s"Vector reference with non-integer: $that")
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  case class LockAddress[Addr : Address](addr: Addr) extends L {
    override def unaryOp(op: UnaryOperator) = op match {
      case IsLock => True
      case _ => super.unaryOp(op)
    }
  }
  object Locked extends L {
    override def toString = "#<locked>"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsLock => True
      case IsLocked => True
      case _ => super.unaryOp(op)
    }
  }
  object Unlocked extends L {
    override def toString = "#<unlocked>"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsLock => True
      case IsLocked => False
      case _ => super.unaryOp(op)
    }
  }

  implicit val isAbstractValue: AbstractValue[L] = new AbstractValue[L] {
    def name = "Type"
    def isTrue(x: L) = x match {
      case False => false
      case Error => false
      case Bottom => false
      case _ => true
    }
    def isFalse(x: L) = x match {
      case False => true
      case _ => false
    }
    def isError(x: L) = x == Error
    def unaryOp(op: UnaryOperator)(x: L) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: L, y: L) = x.binaryOp(op)(y)
    def join(x: L, y: L) = if (x == y || y == Bottom) { x } else if (x == Bottom) { y } else { throw CannotJoin[L](Set(x, y)) }
    def meet(x: L, y: L) = if (x == y) { x } else { bottom }
    def subsumes(x: L, y: L) = x == y || y == Bottom
    def and(x: L, y: => L) = x match {
      case False => False
      case _ => y
    }
    def or(x: L, y: => L) = x match {
      case False => y
      case _ => x
    }
    def car[Addr : Address](x: L) = x match {
      case Cons(car: Addr, cdr: Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: L) = x match {
      case Cons(car: Addr, cdr: Addr) => Set(cdr)
      case _ => Set()
    }
    def vectorSet[Addr : Address](vector: L, index: L, value: L) = vector match {
      case Vector(content) => index match {
        case Int => Vector((content + value).filter(v => v != Bottom))
        case _ => Error(s"Vector set with non-integer index ($index)")
      }
      case _ => Error(s"Vector set on non-vector ($vector)")
    }

    private def toString[Addr : Address](x: L, store: Store[Addr, L], inside: Boolean, visited: Set[L]): String = x match {
      case Cons(car: Addr, cdr: Addr) =>
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
      case VectorAddress(addr: Addr) => toString(store.lookup(addr), store, false, visited + x)
      case LockAddress(addr: Addr) => toString(store.lookup(addr), store, false, visited + x)
      case _ => x.toString
    }
    def toString[Addr : Address](x: L, store: Store[Addr, L]) = toString(x, store, false, Set())

    def getClosures[Exp : Expression, Addr : Address](x: L) = x match {
      case v: Clo[Exp, Addr] => Set(v.clo)
      case _ => Set()
    }
    def getPrimitives[Addr : Address, Abs : AbstractValue](x: L) = x match {
      case Prim(prim: Primitive[Addr, Abs]) => Set(prim)
      case _ => Set()
    }
    def getTids[TID : ThreadIdentifier](x: L) = x match {
      case Tid(t: TID) => Set(t)
      case _ => Set()
    }
    def getVectors[Addr : Address](x: L) = x match {
      case VectorAddress(a: Addr) => Set(a)
      case _ => Set()
    }
    def getLocks[Addr : Address](x: L) = x match {
      case LockAddress(a: Addr) => Set(a)
      case _ => Set()
    }

    def bottom = Bottom
    def error(x: L) = Error
    def inject(x: Int) = Int
    def inject(x: Float) = Float
    def inject(x: String) = String
    def inject(x: Boolean) = if (x) { True } else { False }
    def inject(x: Char) = Char
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = Clo[Exp, Addr](x)
    def injectTid[TID : ThreadIdentifier](t: TID) = Tid(t)
    def injectSymbol(x: String) = Symbol
    def nil = Nil
    def cons[Addr : Address](car: Addr, cdr: Addr) = Cons(car, cdr)
    def vector[Addr : Address](addr: Addr, size: L, init: L) = size match {
      case Int => (VectorAddress(addr), Vector(Set(init)))
      case _ => (Error(s"Vector created with non-integer size ($size)"), Bottom)
    }
    def lock[Addr : Address](addr: Addr) = LockAddress(addr)
    def lockedValue = Locked
    def unlockedValue = Unlocked
  }
}

object TypeSetLattice extends PowerSetLattice(TypeLattice)
