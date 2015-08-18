import AbstractValue._

/** Concrete value lattice. Throws errors when joining elements (and therefore
    has to be used only with a infinite precision allocator) */
trait AbstractConcrete {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
  def foldValues[A](f: AbstractConcrete => Set[A]): Set[A] = f(this)
  def join(that: AbstractConcrete): AbstractConcrete =
    if (this.equals(that)) { this } else { throw new Exception(s"AbstractConcrete lattice cannot join elements") }
  def meet(that: AbstractConcrete): AbstractConcrete = if (this.equals(that)) { this } else { AbstractConcrete.AbstractBottom }
  def subsumes(that: AbstractConcrete): Boolean = this.equals(that)
  def plus(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"plus not applicable with operands $this and $that")
  def minus(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"minus not applicable with operands $this and $that")
  def times(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"times not applicable with operands $this and $that")
  def div(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"div not applicable with operands $this and $that")
  def lt(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"lt not applicable with operands $this and $that")
  def numEq(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"numEq not applicable with operands $this and $that")
  def not: AbstractConcrete = AbstractConcrete.AbstractError(s"not not applicable with operand $this")
  def and(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"and not applicable with operands $this and $that")
  def or(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractError(s"or not applicable with operands $this and $that")
}

object AbstractConcrete {
  case class AbstractInt(v: Int) extends AbstractConcrete {
    override def toString = v.toString
    override def plus(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v + v2)
      case _ => super.plus(that)
    }
    override def minus(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v - v2)
      case _ => super.minus(that)
    }
    override def div(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v / v2) /* TODO: no support for floats nor fractions yet */
      case _ => super.div(that)
    }
    override def times(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v * v2)
      case _ => super.times(that)
    }
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
  }
  case class AbstractSymbol(v: String) extends AbstractConcrete {
    override def toString = v.toString
  }
  case class AbstractBool(v: Boolean) extends AbstractConcrete {
    override def toString = if (v) "#t" else "#f"
    override def isTrue = v
    override def isFalse = !v
    override def not = if (v) AbstractBool(false) else AbstractBool(true)
    override def and(that: AbstractConcrete) = that match {
      case AbstractBool(v2) => AbstractBool(v && v2)
      case _ => super.and(that)
    }
    override def or(that: AbstractConcrete) = that match {
      case AbstractBool(v2) => AbstractBool(v || v2)
      case _ => super.and(that)
    }
  }
  case class AbstractError(reason: String) extends AbstractConcrete {
    override def toString = s"error: $reason"
    override def isError = true
  }
  object AbstractBottom extends AbstractConcrete {
    override def toString = "⊥"
    override def isTrue = false
    override def isFalse = false
    override def join(that: AbstractConcrete) = that
  }
  case class AbstractPrimitive(name: String, f: List[AbstractConcrete] => Either[String, AbstractConcrete]) extends AbstractConcrete {
    override def toString = s"#<prim $name>"
  }
  case class AbstractKontinuation[Kont <: Kontinuation](kont: Kont) extends AbstractConcrete {
    override def toString = s"#<kont $kont>"
  }
  case class AbstractClosure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends AbstractConcrete {
    override def toString = "#<clo>"
  }

  implicit object AbstractConcreteAbstractValue extends AbstractValue[AbstractConcrete] {
    def isTrue(x: AbstractConcrete) = x.isTrue
    def isFalse(x: AbstractConcrete) = x.isFalse
    def isError(x: AbstractConcrete) = x.isError
    def foldValues[B](x: AbstractConcrete, f: AbstractConcrete => Set[B]) = x.foldValues(f)
    def join(x: AbstractConcrete, y: AbstractConcrete) = x.join(y)
    def meet(x: AbstractConcrete, y: AbstractConcrete) = x.meet(y)
    def subsumes(x: AbstractConcrete, y: AbstractConcrete) = x.subsumes(y)
    def plus(x: AbstractConcrete, y: AbstractConcrete) = x.plus(y)
    def minus(x: AbstractConcrete, y: AbstractConcrete) = x.minus(y)
    def times(x: AbstractConcrete, y: AbstractConcrete) = x.times(y)
    def div(x: AbstractConcrete, y: AbstractConcrete) = x.div(y)
    def lt(x: AbstractConcrete, y: AbstractConcrete) = x.lt(y)
    def numEq(x: AbstractConcrete, y: AbstractConcrete) = x.numEq(y)
    def not(x: AbstractConcrete) = x.not
    def and(x: AbstractConcrete, y: AbstractConcrete) = x.and(y)
    def or(x: AbstractConcrete, y: AbstractConcrete) = x.or(y)

    def random = AbstractInt(scala.util.Random.nextInt)

    def getKont(x: AbstractConcrete) = x match {
      case AbstractKontinuation(κ) => Some(κ)
      case _ => None
    }
    def getClosure[Exp : Expression, Addr : Address](x: AbstractConcrete) = x match {
      case AbstractClosure(λ: Exp, ρ: Environment[Addr]) => Some((λ, ρ))
      case _ => None
    }
    def getPrimitive(x: AbstractConcrete) = x match {
      case AbstractPrimitive(name, f) => Some((name, f))
      case _ => None
    }
  }

  implicit object AbstractConcreteInjection extends AbstractInjection[AbstractConcrete] {
    def name = "Concrete"
    def bottom = AbstractBottom
    def inject(x: Int) = AbstractInt(x)
    def inject(x: String) = AbstractString(x)
    def inject(x: Boolean) = AbstractBool(x)
    def inject(x: (String, List[AbstractConcrete] => Either[String, AbstractConcrete])) = AbstractPrimitive(x._1, x._2)
    def inject[Kont <: Kontinuation](x: Kont) = AbstractKontinuation(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
    def injectSymbol(x: String) = AbstractSymbol(x)
  }
}
