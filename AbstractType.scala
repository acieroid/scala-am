import AbstractValue._

/** Simple type lattice, where incompatible elements get promoted to Top */
trait AbstractType {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def foldValues[A](f: AbstractType => Set[A]): Set[A] = f(this)
  def join(that: AbstractType): AbstractType = if (this.equals(that)) { this } else { AbstractTop() }
  def meet(that: AbstractType): AbstractType = if (this.equals(that)) { this } else { AbstractBottom() }
  def subsumes(that: AbstractType): Boolean = this.equals(that)
  def plus(that: AbstractType): AbstractType = AbstractBottom()
}
case class AbstractTop() extends AbstractType {
  override def toString = "⊤"
  override def isTrue = true
  override def isFalse = true
  override def plus(that: AbstractType) = this
}
case class AbstractInt() extends AbstractType {
  override def toString = "Int"
  override def plus(that: AbstractType) = that match {
    case AbstractTop() => AbstractTop()
    case AbstractInt() => AbstractInt()
    case _ => AbstractBottom()
  }
}
case class AbstractString() extends AbstractType {
  override def toString = "String"
  override def plus(that: AbstractType) = that match {
    case AbstractTop() => AbstractTop()
    case AbstractString() => AbstractString()
    case _ => AbstractBottom()
  }
}
case class AbstractBool() extends AbstractType {
  override def toString = "Bool"
  override def isTrue = true
  override def isFalse = true
}
case class AbstractBottom() extends AbstractType {
  override def toString = "⊥"
  override def isTrue = false
  override def isFalse = false
  override def join(that: AbstractType) = that
}
case class AbstractPrimitive(name: String, f: List[AbstractType] => Option[AbstractType]) extends AbstractType {
  override def toString = s"#<prim $name>"
}
case class AbstractKontinuation[Kont <: Kontinuation](kont: Kont) extends AbstractType {
  override def toString = s"#<kont $kont>"
}
case class AbstractClosure[Addr : Address, Exp <: Expression](λ: Exp, ρ: Environment[Addr]) extends AbstractType {
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
  }

  implicit object AbstractTypeInjection extends AbstractInjection[AbstractType] {
    def bottom = AbstractBottom()
    def inject(x: Int) = AbstractInt()
    def inject(x: String) = AbstractString()
    def inject(x: Boolean) = AbstractBool()
    def inject(x: (String, List[AbstractType] => Option[AbstractType])) = AbstractPrimitive(x._1, x._2)
    def inject[Kont <: Kontinuation](x: Kont) = AbstractKontinuation(x)
    def inject[Addr : Address, Exp <: Expression](x: (Exp, Environment[Addr])) = AbstractClosure[Addr, Exp](x._1, x._2)
  }
}
