sealed abstract class AbstractValue {
  def isTrue: Boolean
  def isFalse: Boolean = !isTrue
  /** Fold a function over the values contained in this abstract value. This
      should be redefined only for container-like abstract values. */
  def foldValues[A](f: AbstractValue => Set[A]): Set[A] = f(this)
  def ⊔(x: AbstractValue): AbstractValue = if (this.equals(x)) { this } else { x match {
    case v: AbstractValueSet => v ⊔ x /* TODO: is there a better solution than hardcoding this here? */
    case _ => AbstractValueSet(Set(this, x))
  }}
  def ⊓(x: AbstractValue): AbstractValue = if (this.equals(x)) { this } else { AbstractBottom() }
  def ⊒(x: AbstractValue): Boolean

  /* Primitive operations */
  def +(x: AbstractValue): AbstractValue = AbstractBottom()
  def -(x: AbstractValue): AbstractValue = AbstractBottom()
  def *(x: AbstractValue): AbstractValue = AbstractBottom()
  def <(x: AbstractValue): AbstractValue = AbstractBottom()
  def <=(x: AbstractValue): AbstractValue = AbstractBottom()
  def >(x: AbstractValue): AbstractValue = AbstractBottom()
  def >=(x: AbstractValue): AbstractValue = AbstractBottom()
  def absEq(x: AbstractValue): AbstractValue = AbstractBottom()
  def unary_!(): AbstractValue = AbstractBottom()
}
object AbstractBottom {
  def apply() = AbstractValueSet(Set())
  def unapply(x: AbstractValue) = x match {
    case AbstractValueSet(set) => set.size == 0
    case _ => false
  }
  def ⊒(x: AbstractValue) = false
}
case class AbstractClosure(λ: ANFLambda, ρ: Env) extends AbstractValue {
  override def toString: String = s"#<clo>"
  def isTrue = true
  def ⊒(x: AbstractValue) = x match {
    case AbstractClosure(_, _) => x == this
    case AbstractBottom() => true
    case _ => false
  }
}
case class AbstractPrimitive(name: String, f: List[AbstractValue] => AbstractValue) extends AbstractValue {
  override def toString: String = s"#<prim $name>"
  def isTrue = true
  def ⊒(x: AbstractValue) = x match {
    case AbstractPrimitive(n, _) => name == n
    case _ => false
  }
}
case class AbstractKont(κ: Kont) extends AbstractValue {
  override def toString: String = s"#<kont $κ>"
  def isTrue = false
  override def isFalse = false
  def ⊒(x: AbstractValue) = x match {
    case AbstractKont(κ2) => κ == κ2
    case AbstractBottom() => true
    case _ => false
  }
}
case class AbstractSimpleValue(value: Value) extends AbstractValue {
  override def toString: String = value.toString
  def isTrue = value match {
    case ValueBoolean(false) => false
    case _ => true
  }
  def ⊒(x: AbstractValue) = x match {
    case AbstractSimpleValue(v) => value == v
    case AbstractBottom() => true
    case _ => false
  }

  override def ⊔(x: AbstractValue): AbstractValue = (value, x) match {
    case (ValueInteger(a), AbstractSimpleValue(ValueInteger(b))) if a != b => AbstractInt
    case (ValueInteger(_), AbstractInt) => AbstractInt
    case _ => super.⊔(x)
  }

  def numOp(f: (Integer, Integer) => Integer): (Value, AbstractValue) => AbstractValue = {
    case (ValueInteger(a), AbstractSimpleValue(ValueInteger(b))) => AbstractSimpleValue(ValueInteger(f(a,b)))
    case (x, AbstractValueSet(s)) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ numOp(f)(x,y))
    case (x, AbstractInt) => AbstractInt
    case _ => AbstractBottom()
  }

  def cmpOp(f: (Integer, Integer) => Boolean): (Value, AbstractValue) => AbstractValue = {
    case (ValueInteger(a), AbstractSimpleValue(ValueInteger(b))) => AbstractSimpleValue(ValueBoolean(f(a,b)))
    case (x, AbstractValueSet(s)) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ cmpOp(f)(x,y))
    case (x, AbstractInt) => AbstractBool()
    case _ => AbstractBottom()
  }

  override def +(x: AbstractValue) = numOp((a, b) => a + b)(value, x)
  override def -(x: AbstractValue) = numOp((a, b) => a - b)(value, x)
  override def *(x: AbstractValue) = numOp((a, b) => a * b)(value, x)
  override def <(x: AbstractValue) = cmpOp((a, b) => a < b)(value, x)
  override def <=(x: AbstractValue) = cmpOp((a, b) => a <= b)(value, x)
  override def >(x: AbstractValue) = cmpOp((a, b) => a > b)(value, x)
  override def >=(x: AbstractValue) = cmpOp((a, b) => a >= b)(value, x)
  override def absEq(x: AbstractValue) = cmpOp((a, b) => a == b)(value, x)
  override def unary_!() = value match {
    case ValueBoolean(false) => AbstractSimpleValue(ValueBoolean(true))
    case _ => AbstractSimpleValue(ValueBoolean(false))
  }
}
object AbstractInt extends AbstractValue {
  override def toString: String = "Int"
  def isTrue = true
  def ⊒(x: AbstractValue) = x match {
    case AbstractBottom() => true
    case AbstractInt => true
    case AbstractSimpleValue(ValueInteger(_)) => true
    case AbstractValueSet(set) => set.foldLeft(true)((acc, v) => acc && this ⊒ v)
    case _ => false
  }

  override def ⊔(x: AbstractValue): AbstractValue = x match {
    case AbstractSimpleValue(ValueInteger(_)) => AbstractInt
    case _ => super.⊔(x)
  }

  def numOp(x: AbstractValue): AbstractValue = x match {
    case AbstractInt => AbstractInt
    case AbstractSimpleValue(ValueInteger(_)) => AbstractInt
    case AbstractValueSet(s) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ numOp(y))
    case _ => AbstractBottom()
  }

  def cmpOp(x: AbstractValue): AbstractValue = x match {
    case AbstractInt => AbstractBool()
    case AbstractSimpleValue(ValueInteger(_)) => AbstractBool()
    case AbstractValueSet(s) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ cmpOp(y))
    case _ => AbstractBottom()
  }

  override def +(x: AbstractValue) = numOp(x)
  override def -(x: AbstractValue) = numOp(x)
  override def *(x: AbstractValue) = numOp(x)
  override def <(x: AbstractValue) = cmpOp(x)
  override def <=(x: AbstractValue) = cmpOp(x)
  override def >(x: AbstractValue) = cmpOp(x)
  override def >=(x: AbstractValue) = cmpOp(x)
  override def absEq(x: AbstractValue) = cmpOp(x)
  override def unary_!() = AbstractSimpleValue(ValueBoolean(false))
}
object AbstractBool {
  def apply(): AbstractValue = AbstractValueSet(Set(AbstractSimpleValue(ValueBoolean(true)),
                                                    AbstractSimpleValue(ValueBoolean(false))))
}
case class AbstractValueSet(values: Set[AbstractValue]) extends AbstractValue {
  override def toString(): String = "{" + values.mkString(", ") + "}"
  def isTrue = values.exists(_.isTrue)
  override def isFalse = values.exists(_.isFalse)
  def ⊒(x: AbstractValue) = x match {
    case AbstractValueSet(set) => set.foldLeft(true)((acc, v) => this ⊒ v)
    case _ => values.exists(v => v ⊒ x)
  }

  override def foldValues[A](f: AbstractValue => Set[A]): Set[A] =
    values.foldLeft(Set[A]())((s: Set[A], v: AbstractValue) => s ++ v.foldValues(f))
  /** The join result will remain a set of values */
  override def ⊔(x: AbstractValue): AbstractValueSet = x match {
    case AbstractValueSet(set) =>
      if (set.size == 0) { this } else if (values.size == 0) { AbstractValueSet(set) } else {
        AbstractValueSet(values.foldLeft(Set[AbstractValue]())((acc, v) => {
          /* Tries to join v with an item of set such that the resulting element is not another set */
          set.foldLeft((Set[AbstractValue](), false))({
            case ((acc2, true), v2) => (acc2 + v2, true)
            case ((acc2, false), v2) => v ⊔ v2 match {
              case AbstractValueSet(_) => (acc2 + v2, false)
              case joined => (acc2 + joined, true)
            }
          }) match {
            case (s, true) => acc ++ s
            case (s, false) => acc ++ s + v /* if it did not succeed, just add v to the set as is */
          }
        }))
      }
    case _ => this ⊔ AbstractValueSet(Set(x))
  }
  override def ⊓(x: AbstractValue): AbstractValue = x match {
    case AbstractValueSet(s) => AbstractValueSet(s.intersect(values))
    case _ => if (values.contains(x)) { x } else { AbstractBottom() }
  }
  def op(f: (AbstractValue, AbstractValue) => AbstractValue): (Set[AbstractValue], AbstractValue) => AbstractValue = {
    case (s, x) => s.foldLeft(AbstractValueSet())((acc, v) => acc ⊔ (f(v, x)))
  }
  override def +(x: AbstractValue) = op((a, b) => a + b)(values, x)
  override def -(x: AbstractValue) = op((a, b) => a - b)(values, x)
  override def *(x: AbstractValue) = op((a, b) => a * b)(values, x)
  override def <(x: AbstractValue) = op((a, b) => a < b)(values, x)
  override def <=(x: AbstractValue) = op((a, b) => a <= b)(values, x)
  override def >(x: AbstractValue) = op((a, b) => a > b)(values, x)
  override def >=(x: AbstractValue) = op((a, b) => a >= b)(values, x)
  override def absEq(x: AbstractValue) = op((a, b) => a absEq b)(values, x)
  override def unary_!() = values.foldLeft(AbstractValueSet())((acc, v) => acc ⊔ !v)
}
object AbstractValueSet {
  def apply(): AbstractValueSet = AbstractValueSet(Set[AbstractValue]())
}

/* TODO: abstract pairs */

object AbstractValue /* to get this file as a compilation unit */
