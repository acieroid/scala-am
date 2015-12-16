import UnaryOperator._
import BinaryOperator._

/** Concrete value lattice. Throws errors when joining elements (and therefore
    has to be used only with a infinite precision allocator) */
object ConcreteLattice extends Lattice {
  trait Element {
    def isTrue: Boolean = true
    def isFalse: Boolean = false
    def isError: Boolean = false
    def unaryOp(op: UnaryOperator): L = op match {
      case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => False
      case Not => False
      case _ => ConcreteError(s"$op not applicable with operand $this")
    }
    def binaryOp(op: BinaryOperator)(that: L): L = op match {
      case Eq => if (this == that) { True } else { False }
      case _ => ConcreteError(s"$op not applicable with operands $this and $that")
    }
    def join(that: L): L =
      if (this.equals(that) || that == Bottom) {
        this
      } else {
        throw new CannotJoin[L](this, that)
      }
    def meet(that: L): L = if (this.equals(that)) { this } else { Bottom }
    def subsumes(that: L): Boolean = this.equals(that)
    def and(that: => L): L = ConcreteError(s"and not applicable with operands $this and $that")
    def or(that: => L): L = ConcreteError(s"or not applicable with operands $this and $that")
  }
  type L = Element

  case class ConcreteInt(v: Int) extends L {
    override def toString = v.toString
    override def unaryOp(op: UnaryOperator) = op match {
      case IsInteger => True
      case Ceiling => ConcreteInt(v)
      case Log => ConcreteInt(scala.math.log(v).toInt) /* TODO: float */
      case Random => ConcreteInt(scala.util.Random.nextInt % v)
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: L) = that match {
      case ConcreteInt(v2) => op match {
        case Plus => ConcreteInt(v + v2)
        case Minus => ConcreteInt(v - v2)
        case Times => ConcreteInt(v * v2)
        case Div => ConcreteInt(v / v2)
        case Modulo => ConcreteInt(v % v2)
        case Lt => ConcreteBool(v < v2)
        case NumEq => ConcreteBool(v == v2)
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  case class ConcreteString(v: String) extends L {
    override def toString = '"' + v.toString + '"'
    override def unaryOp(op: UnaryOperator) = op match {
      case IsString => True
      case _ => super.unaryOp(op)
    }
  }
  case class ConcreteChar(v: Char) extends L {
    override def toString = s"#\\$v"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsChar => True
      case _ => super.unaryOp(op)
    }
  }
  case class ConcreteSymbol(v: String) extends L {
    override def toString = v.toString
    override def unaryOp(op: UnaryOperator) = op match {
      case IsSymbol => True
      case _ => super.unaryOp(op)
    }
  }
  case class ConcreteBool(v: Boolean) extends L {
    override def toString = if (v) "#t" else "#f"
    override def isTrue = v
    override def isFalse = !v
    override def unaryOp(op: UnaryOperator) = op match {
      case IsBoolean => True
      case Not => ConcreteBool(!v)
      case _ => super.unaryOp(op)
    }
    override def and(that: => L) = if (v) { that } else { False }
    override def or(that: => L) = if (v) { this } else { that }
  }
  val True: L = ConcreteBool(true)
  val False: L = ConcreteBool(false)
  case class ConcreteError(reason: String) extends L {
    override def toString = s"error: $reason"
    override def isError = true
  }
  object Bottom extends L {
    override def toString = "⊥"
    override def isTrue = false
    override def isFalse = false
    override def join(that: L) = that
    override def unaryOp(op: UnaryOperator) = ConcreteError(s"operation ($op) performed on bottom value")
    override def binaryOp(op: BinaryOperator)(that: L) = ConcreteError(s"operation ($op) performed on bottom value")
  }
  case class Prim[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends L {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Closure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends L {
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
  }
  case class Cons[Addr : Address](car: Addr, cdr: Addr) extends L {
    override def unaryOp(op: UnaryOperator) = op match {
      case IsCons => True
      case _ => super.unaryOp(op)
    }
  }

  val isAbstractValue = new AbstractValue[L] {
    def name = "Concrete"

    def isTrue(x: L) = x.isTrue
    def isFalse(x: L) = x.isFalse
    def isError(x: L) = x.isError
    def unaryOp(op: UnaryOperator)(x: L) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: L, y: L) = x.binaryOp(op)(y)
    def join(x: L, y: L) = x.join(y)
    def meet(x: L, y: L) = x.meet(y)
    def subsumes(x: L, y: L) = x.subsumes(y)
    def and(x: L, y: => L) = x.and(y)
    def or(x: L, y: => L) = x.or(y)
    def car[Addr : Address](x: L) = x match {
      case Cons(car : Addr, cdr : Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: L) = x match {
      case Cons(car : Addr, cdr : Addr) => Set(cdr)
      case _ => Set()
    }
    private def toString[Addr : Address](x: L, store: Store[Addr, L], inside: Boolean, visited: Set[L]): String =
      if (visited.contains(x)) {
        "#loop"
      } else {
        x match {
          case Cons(car : Addr, cdr : Addr) => {
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
          case _ => {
            x.toString
          }
        }
    }
    def toString[Addr : Address](x: L, store: Store[Addr, L]) = toString(x, store, false, Set())

    def getClosures[Exp : Expression, Addr : Address](x: L) = x match {
      case Closure(λ: Exp, ρ: Environment[Addr]) => Set((λ, ρ))
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

    def bottom = Bottom
    def error(x: L): L = ConcreteError(x.toString)
    def inject(x: Int): L = ConcreteInt(x)
    def inject(x: String): L = ConcreteString(x)
    def inject(x: Char): L = ConcreteChar(x)
    def inject(x: Boolean): L = ConcreteBool(x)
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]): L = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = Closure[Exp, Addr](x._1, x._2)
    def injectTid[TID : ThreadIdentifier](tid: TID): L = Tid(tid)
    def injectSymbol(x: String): L = ConcreteSymbol(x)
    def nil: L = Nil
    def cons[Addr : Address](car: Addr, cdr: Addr): L = Cons(car, cdr)
  }
}

object ConcreteSetLattice extends PowerSetLattice(ConcreteLattice)
