/** Concrete value lattice. Throws errors when joining elements (and therefore
    has to be used only with a infinite precision allocator) */
trait AbstractConcrete {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
  def isNull: AbstractConcrete = AbstractConcrete.AbstractFalse
  def isCons: AbstractConcrete = AbstractConcrete.AbstractFalse
  def isChar: AbstractConcrete = AbstractConcrete.AbstractFalse
  def isSymbol: AbstractConcrete = AbstractConcrete.AbstractFalse
  def isString: AbstractConcrete = AbstractConcrete.AbstractFalse
  def isInteger: AbstractConcrete = AbstractConcrete.AbstractFalse
  def foldValues[A](f: AbstractConcrete => Set[A]): Set[A] = f(this)
  def join(that: AbstractConcrete): AbstractConcrete =
    if (this.equals(that) || that == AbstractConcrete.AbstractBottom) { this } else { throw new Exception(s"AbstractConcrete lattice cannot join elements") }
  def meet(that: AbstractConcrete): AbstractConcrete = if (this.equals(that)) { this } else { AbstractConcrete.AbstractBottom }
  def subsumes(that: AbstractConcrete): Boolean = this.equals(that)
  def plus(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"plus not applicable with operands $this and $that")
  def minus(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"minus not applicable with operands $this and $that")
  def times(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"times not applicable with operands $this and $that")
  def div(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"div not applicable with operands $this and $that")
  def modulo(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"modulo not applicable with operands $this and $that")
  def ceiling: AbstractConcrete = AbstractConcrete.AbstractError(s"ceiling not applicable with operand $this")
  def log: AbstractConcrete = AbstractConcrete.AbstractError(s"log not applicable with operand $this")
  def lt(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"lt not applicable with operands $this and $that")
  def numEq(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"numEq not applicable with operands $this and $that")
  def not: AbstractConcrete = AbstractConcrete.AbstractFalse
  def and(that: => AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"and not applicable with operands $this and $that")
  def or(that: => AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"or not applicable with operands $this and $that")
  def eq(that: AbstractConcrete): AbstractConcrete = if (this == that) { AbstractConcrete.AbstractTrue } else { AbstractConcrete.AbstractFalse }
}

object AbstractConcrete {
  case class AbstractInt(v: Int) extends AbstractConcrete {
    override def toString = v.toString
    override def isInteger = AbstractTrue
    override def plus(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v + v2)
      case _ => super.plus(that)
    }
    override def minus(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v - v2)
      case _ => super.minus(that)
    }
    override def times(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v * v2)
      case _ => super.times(that)
    }
    override def div(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v / v2) /* TODO: no support for floats nor fractions yet */
      case _ => super.div(that)
    }
    override def modulo(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v % v2)
      case _ => super.modulo(that)
    }
    override def ceiling = AbstractInt(v) /* TODO: float */
    override def log = AbstractInt(scala.math.log(v).toInt) /* TODO: float */
    override def lt(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractBool(v < v2)
      case _ => super.lt(that)
    }
    override def numEq(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractBool(v == v2)
      case _ => super.numEq(that)
    }
  }
  case class AbstractString(v: String) extends AbstractConcrete {
    override def toString = '"' + v.toString + '"'
    override def isString = AbstractTrue
  }
  case class AbstractChar(v: Char) extends AbstractConcrete {
    override def toString = s"#\\$v"
    override def isChar = AbstractTrue
  }
  case class AbstractSymbol(v: String) extends AbstractConcrete {
    override def toString = v.toString
    override def isSymbol = AbstractTrue
  }
  case class AbstractBool(v: Boolean) extends AbstractConcrete {
    override def toString = if (v) "#t" else "#f"
    override def isTrue = v
    override def isFalse = !v
    override def not = AbstractBool(!v)
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
    override def eq(that: AbstractConcrete) = AbstractFalse
  }
  case class AbstractPrimitive[Addr : Address](prim: Primitive[Addr, AbstractConcrete]) extends AbstractConcrete {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class AbstractClosure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends AbstractConcrete {
    override def toString = "#<clo>"
  }
  object AbstractNil extends AbstractConcrete {
    override def toString = "()"
    override def isNull = AbstractTrue
  }
  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractConcrete {
    override def isCons = AbstractTrue
  }

  implicit object AbstractConcreteAbstractValue extends AbstractValue[AbstractConcrete] {
    def isTrue(x: AbstractConcrete) = x.isTrue
    def isFalse(x: AbstractConcrete) = x.isFalse
    def isError(x: AbstractConcrete) = x.isError
    def isNull(x: AbstractConcrete) = x.isNull
    def isCons(x: AbstractConcrete) = x.isCons
    def isChar(x: AbstractConcrete) = x.isChar
    def isSymbol(x: AbstractConcrete) = x.isSymbol
    def isString(x: AbstractConcrete) = x.isString
    def isInteger(x: AbstractConcrete) = x.isInteger
    def foldValues[B](x: AbstractConcrete, f: AbstractConcrete => Set[B]) = x.foldValues(f)
    def join(x: AbstractConcrete, y: AbstractConcrete) = x.join(y)
    def meet(x: AbstractConcrete, y: AbstractConcrete) = x.meet(y)
    def subsumes(x: AbstractConcrete, y: AbstractConcrete) = x.subsumes(y)
    def plus(x: AbstractConcrete, y: AbstractConcrete) = x.plus(y)
    def minus(x: AbstractConcrete, y: AbstractConcrete) = x.minus(y)
    def times(x: AbstractConcrete, y: AbstractConcrete) = x.times(y)
    def div(x: AbstractConcrete, y: AbstractConcrete) = x.div(y)
    def modulo(x: AbstractConcrete, y: AbstractConcrete) = x.modulo(y)
    def ceiling(x: AbstractConcrete) = x.ceiling
    def log(x: AbstractConcrete) = x.log
    def lt(x: AbstractConcrete, y: AbstractConcrete) = x.lt(y)
    def numEq(x: AbstractConcrete, y: AbstractConcrete) = x.numEq(y)
    def not(x: AbstractConcrete) = x.not
    def and(x: AbstractConcrete, y: => AbstractConcrete) = x.and(y)
    def or(x: AbstractConcrete, y: => AbstractConcrete) = x.or(y)
    def eq(x: AbstractConcrete, y: AbstractConcrete) = x.eq(y)
    def car[Addr : Address](x: AbstractConcrete) = x match {
      case AbstractCons(car : Addr, cdr : Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: AbstractConcrete) = x match {
      case AbstractCons(car : Addr, cdr : Addr) => Set(cdr)
      case _ => Set()
    }
    def random(x: AbstractConcrete) = x match {
      case AbstractInt(n) => AbstractInt(scala.util.Random.nextInt % n)
      case _ => AbstractError(s"random: bound is not an integer, but $x")
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
    def getPrimitive[Addr : Address](x: AbstractConcrete) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, AbstractConcrete]) => Some(prim)
      case _ => None
    }
  }

  implicit object AbstractConcreteInjection extends AbstractInjection[AbstractConcrete] {
    def name = "Concrete"
    def bottom = AbstractBottom
    def error(x: AbstractConcrete) = AbstractError(x.toString)
    def inject(x: Int) = AbstractInt(x)
    def inject(x: String) = AbstractString(x)
    def inject(x: Char) = AbstractChar(x)
    def inject(x: Boolean) = AbstractBool(x)
    def inject[Addr : Address](x: Primitive[Addr, AbstractConcrete]) = AbstractPrimitive(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
    def injectSymbol(x: String) = AbstractSymbol(x)
    def nil = AbstractNil
    def cons[Addr : Address](car: Addr, cdr : Addr) = AbstractCons(car, cdr)
  }
}
