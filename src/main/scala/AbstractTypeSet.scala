import AbstractValue._

/* Type lattice (with precise bools) that joins incompatible elements into a set. No top element is therefore needed */
trait AbstractTypeSet {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
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
  def plus(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def minus(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def times(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def div(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def modulo(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def ceiling: AbstractTypeSet = AbstractTypeSet.AbstractError
  def log: AbstractTypeSet = AbstractTypeSet.AbstractError
  def lt(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def numEq(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def not: AbstractTypeSet = AbstractTypeSet.AbstractError
  def and(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
  def or(that: AbstractTypeSet): AbstractTypeSet = AbstractTypeSet.AbstractError
}

object AbstractTypeSet {
  type A = AbstractTypeSet

  object AbstractError extends AbstractTypeSet {
    override def toString = s"error"
    override def isError = true
  }

  object AbstractInt extends AbstractTypeSet {
    override def toString = "Int"
    override def plus(that: A) = that match {
      case AbstractInt => AbstractInt
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.plus(y))))
      case _ => super.plus(that)
    }
    override def minus(that: A) = that match {
      case AbstractInt => AbstractInt
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.minus(y))))
      case _ => super.minus(that)
    }
    override def times(that: A) = that match {
      case AbstractInt => AbstractInt
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.times(y))))
      case _ => super.times(that)
    }
    override def div(that: A) = that match {
      case AbstractInt => AbstractInt
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.div(y))))
      case _ => super.div(that)
    }
    override def modulo(that: A) = that match {
      case AbstractInt => AbstractInt
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.modulo(y))))
      case _ => super.div(that)
    }
    override def ceiling = AbstractInt
    override def log = AbstractInt
    override def lt(that: A) = that match {
      case AbstractInt => AbstractSet(Set(AbstractTrue, AbstractFalse))
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(AbstractTrue, AbstractFalse)))
      case _ => super.lt(that)
    }
    override def numEq(that: A) = that match {
      case AbstractInt => AbstractSet(Set(AbstractTrue, AbstractFalse))
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(AbstractTrue, AbstractFalse)))
      case _ => super.numEq(that)
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
    override def not = AbstractFalse
    override def and(that: A) = that match {
      case AbstractTrue => AbstractTrue
      case AbstractFalse => AbstractFalse
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.and(y))))
      case _ => super.and(that)
    }
    /* This isn't Scheme semantics. But Scheme semantics for or and and are
     * handled at the level of the semantics definition, not here */
    override def or(that: A) = that match {
      case AbstractTrue => AbstractTrue
      case AbstractFalse => AbstractTrue
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.and(y))))
      case _ => super.and(that)
    }
  }
  object AbstractFalse extends AbstractTypeSet {
    override def toString = "#f"
    override def isTrue = false
    override def isFalse = true
    override def and(that: A) = that match {
      case AbstractTrue => AbstractFalse
      case AbstractFalse => AbstractFalse
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.and(y))))
    }
    override def or(that: A) = that match {
      case AbstractTrue => AbstractTrue
      case AbstractFalse => AbstractFalse
      case AbstractSet(_) => AbstractSet(that.foldValues(y => Set(this.and(y))))
    }
  }
  case class AbstractPrimitive[Addr : Address](prim: Primitive[Addr, AbstractTypeSet]) extends AbstractTypeSet {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class AbstractKontinuation[Kont <: Kontinuation](kont: Kont) extends AbstractTypeSet {
    override def toString = s"#<kont $kont>"
  }
  case class AbstractClosure[Exp : Expression, Addr : Address](λ: Exp, ρ: Environment[Addr]) extends AbstractTypeSet {
    override def toString = "#<clo>"
  }
  case class AbstractSet(content: Set[A]) extends AbstractTypeSet {
    /* invariant: content does not contain any other AbstractSet, i.e., content.exists(_.isInstanceOf[AbstractSet]) == false */
    require(content.exists(_.isInstanceOf[AbstractSet]) == false, s"AbstractSet content contains another AbstractSet: $content")
    override def toString = "{" + content.mkString(", ") + "}"
    override def isTrue = content.exists(_.isTrue)
    override def isFalse = content.exists(_.isFalse)
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
    private def dropBottoms(set: Set[A]) =
      set.filter({ case AbstractSet(content) => content.size != 0
                   case _ => true })
    private def merge(set: Set[A]): Set[A] =
        set.foldLeft(Set[A]())((res, x) => x match {
          case AbstractSet(content) => res ++ merge(content)
          case _ => res + x
        })
    private def op(f: A => A) =  AbstractSet(dropBottoms(merge(content.map(f))))
    override def plus(that: A) = op((v) => v.plus(that))
    override def minus(that: A) = op((v) => v.minus(that))
    override def times(that: A) = op((v) => v.times(that))
    override def div(that: A) = op((v) => v.div(that))
    override def lt(that: A) = op((v) => v.lt(that))
    override def numEq(that: A) = op((v) => v.numEq(that))
    override def not = op((v) => v.not)
    override def and(that: A) = op((v) => v.and(that))
    override def or(that: A) = op((v) => v.or(that))
  }

  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractTypeSet

  val AbstractBottom = new AbstractSet(Set())

  implicit object AbstractTypeSetAbstractValue extends AbstractValue[AbstractTypeSet] {
    def isTrue(x: A) = x.isTrue
    def isFalse(x: A) = x.isFalse
    def isError(x: A) = x.isError
    def foldValues[B](x: A, f: A => Set[B]) = x.foldValues(f)
    def join(x: A, y: A) = x.join(y)
    def meet(x: A, y: A) = x.meet(y)
    def subsumes(x: A, y: A) = x.subsumes(y)

    def plus(x: A, y: A) = x.plus(y)
    def minus(x: A, y: A) = x.minus(y)
    def times(x: A, y: A) = x.times(y)
    def div(x: A, y: A) = x.div(y)
    def modulo(x: A, y: A) = x.modulo(y)
    def ceiling(x: A) = x.ceiling
    def log(x: A) = x.log
    def lt(x: A, y: A) = x.lt(y)
    def numEq(x: A, y: A) = x.numEq(y)
    def not(x: A) = x.not
    def and(x: A, y: A) = x.and(y)
    def or(x: A, y: A) = x.or(y)
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
    private def toString[Addr : Address](x: AbstractTypeSet, store: Store[Addr, AbstractTypeSet], inside: Boolean): String = x match {
      case AbstractCons(car : Addr, cdr : Addr) =>
        val carstr = toString(store.lookup(car), store, false)
        val cdrval = store.lookup(cdr)
        val cdrstr = toString(store.lookup(cdr), store, true)
        cdrval match {
          // TODO: case AbstractNil => if (inside) { "$carstr" } else { s"($carstr)" }
          case AbstractCons(_, _) => if (inside) { s"$carstr $cdrstr" } else { s"($carstr $cdrstr)" }
          case _ => if (inside) { s"$carstr . $cdrstr" } else { s"($carstr . $cdrstr)" }
        }
      case _ => {
        x.toString
      }
    }
    def toString[Addr : Address](x: AbstractTypeSet, store: Store[Addr, AbstractTypeSet]) = toString(x, store, false)

    def getKonts(x: A) = x match {
      case AbstractKontinuation(κ) => Set(κ)
      case AbstractSet(content) => content.flatMap(y => getKonts(y))
      case _ => Set()
    }
    def getClosures[Exp : Expression, Addr : Address](x: A) = x match {
      case AbstractClosure(λ: Exp, ρ: Environment[Addr]) => Set((λ, ρ))
      case AbstractSet(content) => content.flatMap(y => getClosures(y))
      case _ => Set()
    }
    def getPrimitive[Addr : Address](x: A) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, AbstractTypeSet]) => Some(prim)
      case _ => None
    }
  }

  implicit object AbstractTypeSetInjection extends AbstractInjection[AbstractTypeSet] {
    def name = "TypeSet"
    def bottom = AbstractBottom
    def inject(x: Int) = AbstractInt
    def inject(x: String) = AbstractString
    def inject(x: Boolean) = if (x) { AbstractTrue } else { AbstractFalse }
    def inject[Addr : Address](x: Primitive[Addr, AbstractTypeSet]) = AbstractPrimitive(x)
    def inject[Kont <: Kontinuation](x: Kont) = AbstractKontinuation(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosure[Exp, Addr](x._1, x._2)
    def injectSymbol(x: String) = AbstractSymbol
    def cons[Addr : Address](car: Addr, cdr : Addr) = AbstractCons(car, cdr)
  }
}
