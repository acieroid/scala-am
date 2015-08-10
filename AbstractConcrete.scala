import AbstractValue._

/** Concrete value lattice. Throws errors when joining elements (and therefore
    has to be used only with a infinite precision allocator) */
trait AbstractConcrete {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def foldValues[A](f: AbstractConcrete => Set[A]): Set[A] = f(this)
  def join(that: AbstractConcrete): AbstractConcrete =
    if (this.equals(that)) { this } else { throw new Exception(s"AbstractConcrete lattice cannot join elements") }
  def meet(that: AbstractConcrete): AbstractConcrete = if (this.equals(that)) { this } else { AbstractConcrete.AbstractBottom }
  def subsumes(that: AbstractConcrete): Boolean = this.equals(that)
  def plus(that: AbstractConcrete): AbstractConcrete = AbstractConcrete.AbstractBottom
}

object AbstractConcrete {
  case class AbstractInt(v: Int) extends AbstractConcrete {
    override def toString = v.toString
    override def plus(that: AbstractConcrete) = that match {
      case AbstractInt(v2) => AbstractInt(v + v2)
      case _ => AbstractBottom
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
    def foldValues[B](x: AbstractConcrete, f: AbstractConcrete => Set[B]) = x.foldValues(f)
    def join(x: AbstractConcrete, y: AbstractConcrete) = x.join(y)
    def meet(x: AbstractConcrete, y: AbstractConcrete) = x.meet(y)
    def subsumes(x: AbstractConcrete, y: AbstractConcrete) = x.subsumes(y)
    def plus(x: AbstractConcrete, y: AbstractConcrete) = x.plus(y)
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
