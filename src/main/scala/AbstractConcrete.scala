import UnaryOperator._
import BinaryOperator._

/** Concrete value lattice. Throws errors when joining elements (and therefore
    has to be used only with a infinite precision allocator) */
trait AbstractConcrete {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
  def unaryOp(op: UnaryOperator): AbstractConcrete = op match {
    case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => AbstractConcrete.AbstractFalse
    case Not => AbstractConcrete.AbstractFalse
    case _ => AbstractConcrete.AbstractError(s"$op not applicable with operand $this")
  }
  def binaryOp(op: BinaryOperator)(that: AbstractConcrete): AbstractConcrete = op match {
    case Eq => if (this == that) { AbstractConcrete.AbstractTrue } else { AbstractConcrete.AbstractFalse }
    case _ => AbstractConcrete.AbstractError(s"$op not applicable with operands $this and $that")
  }
  def foldValues[A](f: AbstractConcrete => Set[A]): Set[A] = f(this)
  def join(that: AbstractConcrete): AbstractConcrete =
    if (this.equals(that) || that == AbstractConcrete.AbstractBottom) { this } else { throw new Exception(s"AbstractConcrete lattice cannot join elements") }
  def meet(that: AbstractConcrete): AbstractConcrete = if (this.equals(that)) { this } else { AbstractConcrete.AbstractBottom }
  def subsumes(that: AbstractConcrete): Boolean = this.equals(that)
  def and(that: => AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"and not applicable with operands $this and $that")
  def or(that: => AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"or not applicable with operands $this and $that")
}

object AbstractConcrete {
  case class AbstractInt(v: Int) extends AbstractConcrete {
    override def toString = v.toString
    override def unaryOp(op: UnaryOperator) = op match {
      case IsInteger => AbstractTrue
      case Ceiling => AbstractInt(v)
      case Log => AbstractInt(scala.math.log(v).toInt) /* TODO: float */
      case Random => AbstractInt(scala.util.Random.nextInt % v)
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => op match {
        case Plus => AbstractInt(v + v2)
        case Minus => AbstractInt(v - v2)
        case Times => AbstractInt(v * v2)
        case Div => AbstractInt(v / v2)
        case Modulo => AbstractInt(v % v2)
        case Lt => AbstractBool(v < v2)
        case NumEq => AbstractBool(v == v2)
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  case class AbstractString(v: String) extends AbstractConcrete {
    override def toString = '"' + v.toString + '"'
    override def unaryOp(op: UnaryOperator) = op match {
      case IsString => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }
  case class AbstractChar(v: Char) extends AbstractConcrete {
    override def toString = s"#\\$v"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsChar => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }
  case class AbstractSymbol(v: String) extends AbstractConcrete {
    override def toString = v.toString
    override def unaryOp(op: UnaryOperator) = op match {
      case IsSymbol => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }
  case class AbstractBool(v: Boolean) extends AbstractConcrete {
    override def toString = if (v) "#t" else "#f"
    override def isTrue = v
    override def isFalse = !v
    override def unaryOp(op: UnaryOperator) = op match {
      case IsBoolean => AbstractTrue
      case Not => AbstractBool(!v)
      case _ => super.unaryOp(op)
    }
    override def and(that: => AbstractConcrete) = if (v) { that } else { AbstractFalse }
    override def or(that: => AbstractConcrete) = if (v) { this } else { that }
  }
  val AbstractTrue: AbstractConcrete = AbstractBool(true)
  val AbstractFalse: AbstractConcrete = AbstractBool(false)
  case class AbstractError(reason: String) extends AbstractConcrete {
    override def toString = s"error: $reason"
    override def isError = true
  }
  object AbstractBottom extends AbstractConcrete {
    override def toString = "⊥"
    override def isTrue = false
    override def isFalse = false
    override def join(that: AbstractConcrete) = that
    override def unaryOp(op: UnaryOperator) = AbstractError(s"operation ($op) performed on bottom value")
    override def binaryOp(op: BinaryOperator)(that: AbstractConcrete) = AbstractError(s"operation ($op) performed on bottom value")
  }
  case class AbstractPrimitive[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends AbstractConcrete {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class AbstractClosure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends AbstractConcrete {
    override def toString = "#<clo>"
  }
  case class AbstractTid[T : Tid](t: T) extends AbstractConcrete {
    override def toString = s"#<thread $t>"
  }
  object AbstractNil extends AbstractConcrete {
    override def toString = "()"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsNull => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }
  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractConcrete {
    override def unaryOp(op: UnaryOperator) = op match {
      case IsCons => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }

  implicit object AbstractConcreteAbstractValue extends AbstractValue[AbstractConcrete] {
    def name = "Concrete"

    def isTrue(x: AbstractConcrete) = x.isTrue
    def isFalse(x: AbstractConcrete) = x.isFalse
    def isError(x: AbstractConcrete) = x.isError
    def unaryOp(op: UnaryOperator)(x: AbstractConcrete) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: AbstractConcrete, y: AbstractConcrete) = x.binaryOp(op)(y)
    def foldValues[B](x: AbstractConcrete, f: AbstractConcrete => Set[B]) = x.foldValues(f)
    def join(x: AbstractConcrete, y: AbstractConcrete) = x.join(y)
    def meet(x: AbstractConcrete, y: AbstractConcrete) = x.meet(y)
    def subsumes(x: AbstractConcrete, y: AbstractConcrete) = x.subsumes(y)
    def and(x: AbstractConcrete, y: => AbstractConcrete) = x.and(y)
    def or(x: AbstractConcrete, y: => AbstractConcrete) = x.or(y)
    def car[Addr : Address](x: AbstractConcrete) = x match {
      case AbstractCons(car : Addr, cdr : Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: AbstractConcrete) = x match {
      case AbstractCons(car : Addr, cdr : Addr) => Set(cdr)
      case _ => Set()
    }
    private def toString[Addr : Address](x: AbstractConcrete, store: Store[Addr, AbstractConcrete], inside: Boolean, visited: Set[AbstractConcrete]): String =
      if (visited.contains(x)) {
        "#loop"
      } else {
        x match {
          case AbstractCons(car : Addr, cdr : Addr) => {
            val carstr =  toString(store.lookup(car), store, false, visited + x)
            val cdrval = store.lookup(cdr)
            val cdrstr =  toString(store.lookup(cdr), store, true, visited + x)
            val content = cdrval match {
              case AbstractNil => s"$carstr"
              case AbstractCons(_, _) => s"$carstr $cdrstr"
              case _ => s"$carstr . $cdrstr"
            }
            if (inside) { content } else { s"($content)" }
          }
          case _ => {
            x.toString
          }
        }
    }
    def toString[Addr : Address](x: AbstractConcrete, store: Store[Addr, AbstractConcrete]) = toString(x, store, false, Set())

    def getClosures[Exp : Expression, Addr : Address](x: AbstractConcrete) = x match {
      case AbstractClosure(λ: Exp, ρ: Environment[Addr]) => Set((λ, ρ))
      case _ => Set()
    }
    def getPrimitive[Addr : Address, Abs : AbstractValue](x: AbstractConcrete) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, Abs]) => Some(prim)
      case _ => None
    }
    def getTids[T : Tid](x: AbstractConcrete) = x match {
      case AbstractTid(t: T) => Set(t)
      case _ => Set()
    }

    def bottom = AbstractBottom
    def error(x: AbstractConcrete) = AbstractError(x.toString)
    def inject(x: Int) = AbstractInt(x)
    def inject(x: String) = AbstractString(x)
    def inject(x: Char) = AbstractChar(x)
    def inject(x: Boolean) = AbstractBool(x)
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = AbstractPrimitive(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
    def injectTid[T : Tid](tid: T) = AbstractTid(tid)
    def injectSymbol(x: String) = AbstractSymbol(x)
    def nil = AbstractNil
    def cons[Addr : Address](car: Addr, cdr: Addr) = AbstractCons(car, cdr)
  }
}
