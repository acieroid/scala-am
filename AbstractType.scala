import AbstractValue._

/** Simple type lattice, where incompatible elements get promoted to Top */
trait AbstractType {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def foldValues[A](f: AbstractType => Set[A]): Set[A] = f(this)
  def join(that: AbstractType): AbstractType = if (this.equals(that)) { this } else { AbstractTop }
  def meet(that: AbstractType): AbstractType = if (this.equals(that)) { this } else { AbstractBottom }
  def subsumes(that: AbstractType): Boolean = this.equals(that)
  def plus(that: AbstractType): AbstractType = AbstractBottom
}
object AbstractTop extends AbstractType {
  override def toString = "⊤"
  override def isTrue = true
  override def isFalse = true
  override def plus(that: AbstractType) = this
}
object AbstractInt extends AbstractType {
  override def toString = "Int"
  override def plus(that: AbstractType) = that match {
    case AbstractTop => AbstractTop
    case AbstractInt => AbstractInt
    case _ => AbstractBottom
  }
}
object AbstractString extends AbstractType {
  override def toString = "String"
  override def plus(that: AbstractType) = that match {
    case AbstractTop => AbstractTop
    case AbstractString => AbstractString
    case _ => AbstractBottom
  }
}
object AbstractBool extends AbstractType {
  override def toString = "Bool"
  override def isTrue = true
  override def isFalse = true
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

object AbstractType {
  implicit object AbstractTypeAbstractValue extends AbstractValue[AbstractType] {
    def isTrue(x: AbstractType) = x.isTrue
    def isFalse(x: AbstractType) = x.isFalse
    def foldValues[B](x: AbstractType, f: AbstractType => Set[B]) = x.foldValues(f)
    def join(x: AbstractType, y: AbstractType) = x.join(y)
    def meet(x: AbstractType, y: AbstractType) = x.meet(y)
    def subsumes(x: AbstractType, y: AbstractType) = x.subsumes(y)
    def plus(x: AbstractType, y: AbstractType) = x.plus(y)
    def getKont(x: AbstractType) = x match {
      case AbstractKontinuation(κ) => Some(κ)
      case _ => None
    }
    def getClosure[Exp : Expression, Addr : Address](x: AbstractType) = x match {
      case AbstractClosure(λ : Exp, ρ : Environment[Addr]) => Some((λ, ρ))
      case _ => None
    }
    def getPrimitive(x: AbstractType) = x match {
      case AbstractPrimitive(name, f) => Some((name, f))
      case _ => None
    }
  }

  implicit object AbstractTypeInjection extends AbstractInjection[AbstractType] {
    def bottom = AbstractBottom
    def inject(x: Int) = AbstractInt
    def inject(x: String) = AbstractString
    def inject(x: Boolean) = AbstractBool
    def inject(x: (String, List[AbstractType] => Either[String, AbstractType])) = AbstractPrimitive(x._1, x._2)
    def inject[Kont <: Kontinuation](x: Kont) = AbstractKontinuation(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
  }
}
