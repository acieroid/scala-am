import UnaryOperator._
import BinaryOperator._

/* Type lattice (with precise bools) that joins incompatible elements into a set. No top element is therefore needed */
trait AbstractTypeSet {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
  def unaryOp(op: UnaryOperator): AbstractTypeSet = op match {
    case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => AbstractTypeSet.AbstractFalse
    case Not => AbstractTypeSet.AbstractFalse
    case _ => AbstractTypeSet.AbstractError
  }
  def binaryOp(op: BinaryOperator)(that: AbstractTypeSet): AbstractTypeSet = op match {
    case Eq => that match {
      /* most elements of this lattice lose too much information to be compared precisely */
      case _ if this == that => AbstractTypeSet.AbstractBool
      case AbstractTypeSet.AbstractSet(content) => content.foldLeft(AbstractTypeSet.AbstractBottom)((acc, v) => acc.join(binaryOp(op)(v)))
      case _ => AbstractTypeSet.AbstractFalse
    }
    case _ => AbstractTypeSet.AbstractError
  }
  def foldValues[A](f: AbstractTypeSet => Set[A]): Set[A] = f(this)
  def join(that: AbstractTypeSet): AbstractTypeSet =
    if (this.equals(that) || that.equals(AbstractTypeSet.AbstractBottom)) {
      this
    } else if (that.isInstanceOf[AbstractTypeSet.AbstractSet]) {
      that.join(this)
    } else {
      AbstractTypeSet.AbstractSet(Set(this, that))
    }
  def meet(that: AbstractTypeSet): AbstractTypeSet =
    if (this.equals(that)) { this } else { AbstractTypeSet.AbstractBottom }
  def subsumes(that: AbstractTypeSet): Boolean = this.equals(that)
  def and(that: => AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def or(that: => AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
}

object AbstractTypeSet {
  type A = AbstractTypeSet

  object AbstractError extends AbstractTypeSet {
    override def toString = s"error"
    override def isError = true
  }

  object AbstractInt extends AbstractTypeSet {
    override def toString = "Int"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsInteger => AbstractTrue
      case Ceiling | Log | Random => AbstractInt
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractTypeSet) = op match {
      case Plus | Minus | Times | Div | Modulo => that match {
        case AbstractInt => AbstractInt
        case AbstractSet(content) => content.foldLeft(AbstractBottom)((acc, v) => acc.join(binaryOp(op)(that)))
        case _ => super.binaryOp(op)(that)
      }
      case Lt | NumEq => that match {
        case AbstractInt => AbstractBool
        case AbstractSet(content) => content.foldLeft(AbstractBottom)((acc, v) => acc.join(binaryOp(op)(v)))
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }

  object AbstractString extends AbstractTypeSet {
    override def toString = "String"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsString => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }
  object AbstractChar extends AbstractTypeSet {
    override def toString = "Char"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsChar => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }
  object AbstractSymbol extends AbstractTypeSet {
    override def toString = "Symbol"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsSymbol => AbstractTrue
      case _ => super.unaryOp(op)
    }
  }
  object AbstractTrue extends AbstractTypeSet {
    override def toString = "#t"
    override def unaryOp(op: UnaryOperator) = op match {
      case Not => AbstractFalse
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractTypeSet) = op match {
      case Eq => that match {
        case AbstractTrue => AbstractTrue
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
    override def and(that: => A) = that
    override def or(that: => A) = this
  }
  object AbstractFalse extends AbstractTypeSet {
    override def toString = "#f"
    override def isTrue = false
    override def isFalse = true
    override def unaryOp(op: UnaryOperator) = op match {
      case Not => AbstractTrue
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractTypeSet) = op match {
      case Eq => that match {
        case AbstractFalse => AbstractTrue
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
    override def and(that: => A) = this
    override def or(that: => A) = that
  }
  val AbstractBool = AbstractSet(Set(AbstractTrue, AbstractFalse))
  case class AbstractPrimitive[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends AbstractTypeSet {
    override def toString = s"#<prim ${prim.name}>"
    override def binaryOp(op: BinaryOperator)(that: AbstractTypeSet) = op match {
      case Eq => that match {
        case AbstractPrimitive(_) => if (this == that) { AbstractTrue } else { AbstractFalse }
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  case class AbstractClosure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends AbstractTypeSet {
    override def toString = "#<clo>"
    override def binaryOp(op: BinaryOperator)(that: AbstractTypeSet) = op match {
      case Eq => that match {
        case AbstractClosure(_, _) => if (this == that) { AbstractTrue } else { AbstractFalse }
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  case class AbstractSet(content: Set[A]) extends AbstractTypeSet {
    /* invariant: content does not contain any other AbstractSet, i.e., content.exists(_.isInstanceOf[AbstractSet]) == false */
    require(content.exists(_.isInstanceOf[AbstractSet]) == false, s"AbstractSet content contains another AbstractSet: $content")
    override def toString = "{" + content.mkString(", ") + "}"
    override def isTrue = content.exists(_.isTrue)
    override def isFalse = content.exists(_.isFalse)
    override def unaryOp(op: UnaryOperator) =
      content.foldLeft(AbstractBottom)((acc, v) => acc.join(v.unaryOp(op)))
    private def dropBottoms(set: Set[A]) =
      set.filter({ case AbstractSet(content) => content.size != 0
                   case _ => true })
    private def merge(set: Set[A]): Set[A] =
        set.foldLeft(Set[A]())((res, x) => x match {
          case AbstractSet(content) => res ++ merge(content)
          case _ => res + x
        })
    private def oper(f: A => A) =  AbstractSet(dropBottoms(merge(content.map(f))))
    override def binaryOp(op: BinaryOperator)(that: A) =
      oper((v) => v.binaryOp(op)(that))
    override def and(that: => A) = oper((v) => v.and(that))
    override def or(that: => A) = oper((v) => v.or(that))
    override def foldValues[B](f: A => Set[B]) =
      content.foldLeft(Set[B]())((s: Set[B], v: AbstractTypeSet) => s ++ v.foldValues(f))
    override def join(that: A) =
      if (content.isEmpty) {
        that
      }
      else {
        that match {
          case AbstractBottom => this
          case AbstractSet(content2) => {
            /* every element in the other set has to be joined in this set */
            AbstractSet(content2.foldLeft(Set[AbstractTypeSet]())((acc, v) =>
              if (acc.exists(_.subsumes(v))) { acc } else { content + v }))
          }
          case _ => join(AbstractSet(Set(that)))
        }
      }
    override def meet(that: A) = that match {
      case AbstractSet(content2) =>
        /* assumption: the elements contained in the set form a flat lattice,
         * e.g., we will not need to compute the meet of {Int} with {1} */
        AbstractSet(content.intersect(content2))
      case _ => meet(AbstractSet(Set(that)))
    }
    override def subsumes(that: A) =
        /* a set subsumes an abstract value if... */
        that match {
          /* ...the abstract value is a set, and for every element in that set, the current set subsumes it */
          case AbstractSet(content2) =>
            content2.forall(subsumes(_))
          /* ...or the abstract value is not a set itself and is contained in this set */
          case v => content.exists(_.subsumes(v))
        }
  }
  case class AbstractTid[TID : ThreadIdentifier](t: TID) extends AbstractTypeSet {
    override def toString = s"#<thread $t>"
  }
  object AbstractNil extends AbstractTypeSet {
    override def toString = "()"
    override def unaryOp(op: UnaryOperator) = op match {
      case IsNull => AbstractTrue
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractTypeSet) = op match {
      case Eq => that match {
        case AbstractNil => AbstractTrue
        case _ => super.binaryOp(op)(that)
      }
      case _ => super.binaryOp(op)(that)
    }
  }
  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractTypeSet {
    override def unaryOp(op: UnaryOperator) = op match {
      case IsCons => AbstractTrue
      case _ => super.unaryOp(op)
    }
    /* eq cannot be redefined to do pointer equality, because it would probably be
     * incorrect since an address can be allocated more than once in the
     * abstract. For example, for (cons x y), if x is stored in address xa, y
     * in address ya, a different (in the concrete) cons cell (cons x z) where
     * z resides in ya will be considered eq. Returning AbstractBool is the
     * safest solution. Looking the number of times an address has been
     * allocated is a solution to improve precision */
  }

  val AbstractBottom: AbstractTypeSet = new AbstractSet(Set())

  implicit object AbstractTypeSetAbstractValue extends AbstractValue[AbstractTypeSet] {
    def name = "TypeSet"

    def isTrue(x: A) = x.isTrue
    def isFalse(x: A) = x.isFalse
    def isError(x: A) = x.isError
    def unaryOp(op: UnaryOperator)(x: A) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: A, y: A) = x.binaryOp(op)(y)
    def foldValues[B](x: A, f: A => Set[B]) = x.foldValues(f)
    def join(x: A, y: A) = x.join(y)
    def meet(x: A, y: A) = x.meet(y)
    def subsumes(x: A, y: A) = x.subsumes(y)
    def and(x: A, y: => A) = x.and(y)
    def or(x: A, y: => A) = x.or(y)
    def car[Addr : Address](x: AbstractTypeSet) = x match {
      case AbstractCons(car : Addr, cdr : Addr) => Set(car)
      case AbstractSet(_) => x.foldValues(y => car[Addr](y))
      case _ => Set()
    }
    def cdr[Addr : Address](x: AbstractTypeSet) = x match {
      case AbstractCons(car : Addr, cdr : Addr) => Set(cdr)
      case AbstractSet(_) => x.foldValues(y => cdr[Addr](y))
      case _ => Set()
    }
    def random(x: A) = x match {
      case AbstractInt => AbstractInt
      case _ => AbstractError
    }
    private def toString[Addr : Address](x: AbstractTypeSet, store: Store[Addr, AbstractTypeSet], inside: Boolean, visited: Set[AbstractTypeSet]): String =
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
          case AbstractSet(content) => {
            "{" + content.map(v => toString(v, store, false, visited + x)).mkString(", ") + "}"
          }
          case _ => {
            x.toString
          }
        }
    }
    def toString[Addr : Address](x: AbstractTypeSet, store: Store[Addr, AbstractTypeSet]) = toString(x, store, false, Set())

    def getClosures[Exp : Expression, Addr : Address](x: A) = x match {
      case AbstractClosure(λ: Exp, ρ: Environment[Addr]) => Set((λ, ρ))
      case AbstractSet(content) => content.flatMap(y => getClosures(y))
      case _ => Set()
    }
    def getPrimitive[Addr : Address, Abs : AbstractValue](x: A) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, Abs]) => Some(prim)
        /* TODO: AbstractSet case */
      case _ => None
    }
    def getTids[TID : ThreadIdentifier](x: A) = x match {
      case AbstractTid(t: TID) => Set(t)
      case AbstractSet(content) => content.flatMap(y => getTids[TID](y))
      case _ => Set()
    }

    def bottom = AbstractBottom
    def error(x: AbstractTypeSet) = AbstractError
    def inject(x: Int) = AbstractInt
    def inject(x: String) = AbstractString
    def inject(x: Char) = AbstractChar
    def inject(x: Boolean) = if (x) { AbstractTrue } else { AbstractFalse }
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]) = AbstractPrimitive(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
    def injectTid[TID : ThreadIdentifier](t: TID) = AbstractTid(t)
    def injectSymbol(x: String) = AbstractSymbol
    def nil = AbstractNil
    def cons[Addr : Address](car: Addr, cdr : Addr) = AbstractCons(car, cdr)
  }
}
