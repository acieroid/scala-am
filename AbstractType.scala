import AbstractValue._

/** Simple type lattice, where incompatible elements get promoted to Top */
trait AbstractType {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
  def foldValues[A](f: AbstractType => Set[A]): Set[A] = f(this)
  def join(that: AbstractType): AbstractType =
    if (this.equals(that) || that.equals(AbstractType.AbstractBottom)) { this } else { AbstractType.AbstractTop }
  def meet(that: AbstractType): AbstractType =
    if (this.equals(that) || that.equals(AbstractType.AbstractTop)) { this } else { AbstractType.AbstractBottom }
  def subsumes(that: AbstractType): Boolean = this.equals(that)
  def plus(that: AbstractType): AbstractType = AbstractType.AbstractError
  def minus(that: AbstractType): AbstractType = AbstractType.AbstractError
  def times(that: AbstractType): AbstractType = AbstractType.AbstractError
  def div(that: AbstractType): AbstractType = AbstractType.AbstractError
  def modulo(that: AbstractType): AbstractType = AbstractType.AbstractError
  def ceiling: AbstractType = AbstractType.AbstractError
  def log: AbstractType = AbstractType.AbstractError
  def lt(that: AbstractType): AbstractType = AbstractType.AbstractError
  def numEq(that: AbstractType): AbstractType = AbstractType.AbstractError
  def not: AbstractType = AbstractType.AbstractError
  def and(that: AbstractType): AbstractType = AbstractType.AbstractError
  def or(that: AbstractType): AbstractType = AbstractType.AbstractError
}

/** Lattice: Top > Error || String || Int || Boolean || Symbol > Bottom */
object AbstractType {
  object AbstractTop extends AbstractType {
    override def toString = "⊤"
    override def isTrue = true
    override def isFalse = true
    override def subsumes(that: AbstractType) = true
    override def plus(that: AbstractType) = AbstractTop
    override def minus(that: AbstractType) = AbstractTop
    override def times(that: AbstractType) = AbstractTop
    override def div(that: AbstractType) = AbstractTop
    override def modulo(that: AbstractType) = AbstractTop
    override def ceiling = AbstractTop
    override def log = AbstractTop
    override def lt(that: AbstractType) = AbstractTop
    override def numEq(that: AbstractType) = AbstractTop
    override def not = AbstractTop
    override def and(that: AbstractType) = AbstractTop
    override def or(that: AbstractType) = AbstractTop
  }
  object AbstractError extends AbstractType {
    override def toString = "error"
    override def isError = true
  }
  object AbstractInt extends AbstractType {
    override def toString = "Int"
    override def plus(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => AbstractInt
      case _ => super.plus(that)
    }
    override def minus(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => AbstractInt
      case _ => super.minus(that)
    }
    override def times(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => AbstractInt
      case _ => super.times(that)
    }
    override def div(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => AbstractInt
      case _ => super.div(that)
    }
    override def modulo(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => AbstractInt
      case _ => super.modulo(that)
    }
    override def ceiling = AbstractInt
    override def log = AbstractInt
    override def lt(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => AbstractBool
      case _ => super.lt(that)
    }
    override def numEq(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractInt => AbstractBool
      case _ => super.numEq(that)
    }
  }
  object AbstractString extends AbstractType {
    override def toString = "String"
  }
  object AbstractSymbol extends AbstractType {
    override def toString = "Symbol"
  }
  object AbstractBool extends AbstractType {
    override def toString = "Bool"
    override def isTrue = true
    override def isFalse = true
    override def not = AbstractBool
    override def and(that: AbstractType) = that match {
      case AbstractTop => AbstractTop
      case AbstractBool => AbstractBool
      case _ => super.and(that)
    }
    override def or(that: AbstractType) = that match {
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
  case class AbstractPrimitive(name: String, f: List[AbstractType] => Either[String, AbstractType]) extends AbstractType {
    override def toString = s"#<prim $name>"
  }
  case class AbstractKontinuation[Kont <: Kontinuation](kont: Kont) extends AbstractType {
    override def toString = s"#<kont $kont>"
  }
  case class AbstractClosure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends AbstractType {
    override def toString = "#<clo>"
  }

  implicit object AbstractTypeAbstractValue extends AbstractValue[AbstractType] {
    def isTrue(x: AbstractType) = x.isTrue
    def isFalse(x: AbstractType) = x.isFalse
    def isError(x: AbstractType) = x.isError
    def foldValues[B](x: AbstractType, f: AbstractType => Set[B]) = x.foldValues(f)
    def join(x: AbstractType, y: AbstractType) = x.join(y)
    def meet(x: AbstractType, y: AbstractType) = x.meet(y)
    def subsumes(x: AbstractType, y: AbstractType) = x.subsumes(y)
    def plus(x: AbstractType, y: AbstractType) = x.plus(y)
    def minus(x: AbstractType, y: AbstractType) = x.minus(y)
    def times(x: AbstractType, y: AbstractType) = x.times(y)
    def div(x: AbstractType, y: AbstractType) = x.div(y)
    def modulo(x: AbstractType, y: AbstractType) = x.modulo(y)
    def ceiling(x: AbstractType) = x.ceiling
    def log(x: AbstractType) = x.ceiling
    def lt(x: AbstractType, y: AbstractType) = x.lt(y)
    def numEq(x: AbstractType, y: AbstractType) = x.numEq(y)
    def not(x: AbstractType) = x.not
    def and(x: AbstractType, y: AbstractType) = x.and(y)
    def or(x: AbstractType, y: AbstractType) = x.or(y)

    def random(x: AbstractType) = x match {
      case AbstractInt => AbstractInt
      case _ => AbstractError
    }

    def getKont(x: AbstractType) = x match {
      case AbstractKontinuation(κ) => Some(κ)
      case _ => None
    }
    def getClosure[Exp : Expression, Addr : Address](x: AbstractType) = x match {
      case AbstractClosure(λ: Exp, ρ: Environment[Addr]) => Some((λ, ρ))
      case _ => None
    }
    def getPrimitive(x: AbstractType) = x match {
      case AbstractPrimitive(name, f) => Some((name, f))
      case _ => None
    }
  }

  implicit object AbstractTypeInjection extends AbstractInjection[AbstractType] {
    def name = "Type"
    def bottom = AbstractBottom
    def inject(x: Int) = AbstractInt
    def inject(x: String) = AbstractString
    def inject(x: Boolean) = AbstractBool
    def inject(x: (String, List[AbstractType] => Either[String, AbstractType])) = AbstractPrimitive(x._1, x._2)
    def inject[Kont <: Kontinuation](x: Kont) = AbstractKontinuation(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
    def injectSymbol(x: String) = AbstractSymbol
  }
}
