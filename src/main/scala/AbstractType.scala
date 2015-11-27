import UnaryOperator._
import BinaryOperator._

/** Simple type lattice, where incompatible elements get promoted to Top */
trait AbstractType {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
  def unaryOp(op: UnaryOperator): AbstractType = op match {
    case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => AbstractType.AbstractBool
    case Not => AbstractType.AbstractBool
    case _ => AbstractType.AbstractError
  }
  def binaryOp(op: BinaryOperator)(that: AbstractType): AbstractType = op match {
    case Eq => AbstractType.AbstractBool
    case _ => AbstractType.AbstractError
  }
  def foldValues[A](f: AbstractType => Set[A]): Set[A] = f(this)
  def join(that: AbstractType): AbstractType =
    if (this.equals(that) || that.equals(AbstractType.AbstractBottom)) { this } else { AbstractType.AbstractTop }
  def meet(that: AbstractType): AbstractType =
    if (this.equals(that) || that.equals(AbstractType.AbstractTop)) { this } else { AbstractType.AbstractBottom }
  def subsumes(that: AbstractType): Boolean = this.equals(that)
  def and(that: => AbstractType): AbstractType = AbstractType.AbstractError
  def or(that: => AbstractType): AbstractType = AbstractType.AbstractError
}

/** Lattice: Top > Error || String || Int || Boolean || Symbol > Bottom */
object AbstractType {
  object AbstractTop extends AbstractType {
    override def toString = "⊤"
    override def isTrue = true
    override def isFalse = true
    override def subsumes(that: AbstractType) = true
    override def unaryOp(op: UnaryOperator) = op match {
      case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => AbstractBool
      case Not => AbstractBool
      case Ceiling | Log | Random => AbstractTop
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractType) = op match {
      case Plus | Minus | Times | Div | Modulo | Lt | NumEq => AbstractTop
      case _ => super.binaryOp(op)(that)
    }
    override def and(that: => AbstractType) = AbstractTop
    override def or(that: => AbstractType) = AbstractTop
  }
  object AbstractError extends AbstractType {
    override def toString = "error"
    override def isError = true
  }
  object AbstractInt extends AbstractType {
    override def toString = "Int"
    override def unaryOp(op: UnaryOperator) = op match {
      case Ceiling | Log | Random => AbstractInt
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => op match {
        case Plus | Minus | Times | Div | Modulo => AbstractInt
        case Lt | NumEq => AbstractBool
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  object AbstractString extends AbstractType {
    override def toString = "String"
  }
  object AbstractChar extends AbstractType {
    override def toString = "Char"
  }
  object AbstractSymbol extends AbstractType {
    override def toString = "Symbol"
  }
  object AbstractBool extends AbstractType {
    override def toString = "Bool"
    override def isTrue = true
    override def isFalse = true
    override def and(that: => AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractBool => AbstractBool
      case _ => super.and(that)
    }
    override def or(that: => AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractBool => AbstractBool
      case _ => super.or(that)
    }
  }
  object AbstractBottom extends AbstractType {
    override def toString = "⊥"
    override def isTrue = false
    override def isFalse = false
    override def join(that: AbstractType) = that
  }
  case class AbstractPrimitive[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends AbstractType {
    override def toString = s"#<prim ${prim.name}>"
  }
  /* We need to be able to represent multiple closures in this lattice */
  case class AbstractClosures[Exp : Expression, Addr : Address](clos: Set[(Exp, Environment[Addr])]) extends AbstractType {
    override def toString = "#<clos>"
    override def join(that: AbstractType) = that match {
      case other: AbstractClosures[Exp, Addr] => AbstractClosures(clos ++ other.clos)
      case _ => throw new Error("Type lattice cannot join a closure with something else")
    }
  }
  case class AbstractTid[TID : ThreadIdentifier](t: TID) extends AbstractType {
    override def toString = "#<thread $t>"
  }
  object AbstractNil extends AbstractType {
    override def toString = "()"
  }
  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractType

  implicit object AbstractTypeAbstractValue extends AbstractValue[AbstractType] {
    def name = "Type"

    def isTrue(x: AbstractType) = x.isTrue
    def isFalse(x: AbstractType) = x.isFalse
    def isError(x: AbstractType) = x.isError
    def unaryOp(op: UnaryOperator)(x: AbstractType) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: AbstractType, y: AbstractType) = x.binaryOp(op)(y)
    def foldValues[B](x: AbstractType, f: AbstractType => Set[B]) = x.foldValues(f)
    def join(x: AbstractType, y: AbstractType) = x.join(y)
    def meet(x: AbstractType, y: AbstractType) = x.meet(y)
    def subsumes(x: AbstractType, y: AbstractType) = x.subsumes(y)
    def and(x: AbstractType, y: => AbstractType) = x.and(y)
    def or(x: AbstractType, y: => AbstractType) = x.or(y)
    def car[Addr : Address](x: AbstractType) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: AbstractType) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(cdr)
      case _ => Set()
    }
    private def toString[Addr : Address](x: AbstractType, store: Store[Addr, AbstractType], inside: Boolean): String = x match {
      case AbstractCons(car : Addr, cdr : Addr) =>
        val carstr = toString(store.lookup(car), store, false)
        val cdrval = store.lookup(cdr)
        val cdrstr = toString(store.lookup(cdr), store, true)
        val content = cdrval match {
          case AbstractNil => s"$carstr"
          case AbstractCons(_, _) => s"$carstr $cdrstr"
          case _ => s"$carstr . $cdrstr"
        }
        if (inside) { content } else { s"($content)" }
      case _ => {
        x.toString
      }
    }
    def toString[Addr : Address](x: AbstractType, store: Store[Addr, AbstractType]) = toString(x, store, false)

    def getClosures[Exp : Expression, Addr : Address](x: AbstractType) = x match {
      case v: AbstractClosures[Exp, Addr] => v.clos
      case _ => Set()
    }
    def getPrimitive[Addr : Address, Abs : AbstractValue](x: AbstractType) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, Abs]) => Some(prim)
      case _ => None
    }
    def getTids[TID : ThreadIdentifier](x: AbstractType) = x match {
      case AbstractTid(t: TID) => Set(t)
      case _ => Set()
    }

    def bottom = AbstractBottom
    def error(x: AbstractType) = AbstractError
    def inject(x: Int) = AbstractInt
    def inject(x: String) = AbstractString
    def inject(x: Boolean) = AbstractBool
    def inject(x: Char) = AbstractChar
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = AbstractPrimitive(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosures[Exp, Addr](Set((x._1, x._2)))
    def injectTid[TID : ThreadIdentifier](t: TID) = AbstractTid(t)
    def injectSymbol(x: String) = AbstractSymbol
    def nil = AbstractNil
    def cons[Addr : Address](car: Addr, cdr : Addr) = AbstractCons(car, cdr)
  }
}
