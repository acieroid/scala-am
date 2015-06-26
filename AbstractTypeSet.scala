import AbstractValue._
import AbstractType._

/* Type lattice (with precise bools) that joins incompatible elements into a set. No top element is therefore needed */
trait AbstractTypeSet {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def foldValues[A](f: AbstractTypeSet => Set[A]): Set[A] = f(this)
  def join(that: AbstractTypeSet): AbstractTypeSet = if (this.equals(that)) { this } else { AbstractTypeSet.AbstractSet(Set(this, that)) }
  def meet(that: AbstractTypeSet): AbstractTypeSet = if (this.equals(that)) { this } else { AbstractTypeSet.AbstractBottom }
  def subsumes(that: AbstractTypeSet): Boolean = this.equals(that)
  def plus(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractBottom
}

object AbstractTypeSet {
  type A = AbstractTypeSet

  object AbstractInt extends AbstractTypeSet {
    override def toString = "Int"
    override def plus(that: A) = that match {
      case AbstractInt => AbstractInt
    }
  }
  object AbstractString extends AbstractTypeSet {
    override def toString = "String"
  }
  object AbstractSymbol extends AbstractTypeSet {
    override def toString = "Symbol"
  }
  object AbstractTrue extends AbstractTypeSet {
    override def toString = "#t"
  }
  object AbstractFalse extends AbstractTypeSet {
    override def toString = "#f"
    override def isTrue = false
    override def isFalse = true
  }
  case class AbstractPrimitive(name: String, f: List[A] => Either[String, A]) extends AbstractTypeSet {
    override def toString = s"#<prim $name>"
  }
  case class AbstractKontinuation[Kont <: Kontinuation](kont: Kont) extends AbstractTypeSet {
    override def toString = s"#<kont $kont>"
  }
  case class AbstractClosure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends AbstractTypeSet {
    override def toString = "#<clo>"
  }
  case class AbstractSet(content: Set[A]) extends AbstractTypeSet {
    /* invariant: content does not contain any other AbstractSet, i.e., content.exists(_.isInstanceOf[AbstractSet]) == false */
    override def toString = "{" + content.mkString(", ") + "}"
    override def isTrue = content.exists(_.isTrue)
    override def isFalse = content.exists(_.isFalse)
    override def foldValues[B](f: A => Set[B]) =
      content.foldLeft(Set[B]())((s: Set[B], v: AbstractTypeSet) => s ++ v.foldValues(f))
    override def join(that: A) = that match {
      case AbstractSet(content2) =>
        /* every element in the other set has to be joined in this set */
        AbstractSet(content2.foldLeft(Set[AbstractTypeSet]())((acc, v) =>
          if (acc.exists(_.subsumes(v))) { acc } else { content + v }))
      case _ => join(AbstractSet(Set(that)))
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
    private def dropBottoms(set: Set[A]) =
      set.filter({ case AbstractSet(content) => content.size != 0
                   case _ => true })
    override def plus(that: A) =
      AbstractSet(dropBottoms(content.map((v) => v.plus(that))))
  }
  val AbstractBottom = new AbstractSet(Set())

  implicit object AbstractTypeSetAbstractValue extends AbstractValue[AbstractTypeSet] {
    def isTrue(x: A) = x.isTrue
    def isFalse(x: A) = x.isFalse
    def foldValues[B](x: A, f: A => Set[B]) = x.foldValues(f)
    def join(x: A, y: A) = x.join(y)
    def meet(x: A, y: A) = x.meet(y)
    def subsumes(x: A, y: A) = x.subsumes(y)
    def plus(x: A, y: A) = x.plus(y)
    def getKont(x: A) = x match {
      case AbstractKontinuation(κ) => Some(κ)
      case _ => None
    }
    def getClosure[Exp : Expression, Addr : Address](x: A) = x match {
      case AbstractClosure(λ: Exp, ρ: Environment[Addr]) => Some((λ, ρ))
      case _ => None
    }
    def getPrimitive(x: A) = x match {
      case AbstractPrimitive(name, f) => Some((name, f))
      case _ => None
    }
  }

  implicit object AbstractTypeSetInjection extends AbstractInjection[AbstractTypeSet] {
    def bottom = AbstractBottom
    def inject(x: Int) = AbstractInt
    def inject(x: String) = AbstractString
    def inject(x: Boolean) = if (x) { AbstractTrue } else { AbstractFalse }
    def inject(x: (String, List[A] => Either[String, A])) = AbstractPrimitive(x._1, x._2)
    def inject[Kont <: Kontinuation](x: Kont) = AbstractKontinuation(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
    def injectSymbol(x: String) = AbstractSymbol
  }
}
