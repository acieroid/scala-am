/** Abstract values are abstract representations of the possible values of a variable */
trait AbstractValue[A] {
  /** Can this abstract value be considered true for conditionals? */
  def isTrue(x: A): Boolean
  /** Can this abstract value be considered false for conditionals? */
  def isFalse(x: A): Boolean
  /** Fold a function over the values contained in this abstract values. This
      should be redefined only for container-like abstract values (e.g., for a
      set abstraction) */
  def foldValues[B](x: A, f: A => Set[B]): Set[B]
  def join(x: A, y: A): A
  def meet(x: A, y: A): A
  def subsumes(x: A, y: A): Boolean
  def plus(x: A, y: A): A
}

/** Concrete values have to be injected to become abstract */
trait AbstractInjection[A] {
  /* Bottom element of the lattice */
  def bottom: A
  /** Injection of an integer */
  def inject(x: Int): A
  /** Injection of a string */
  def inject(x: String): A
  /** Injection of a boolean */
  def inject(x: Boolean): A
  /** Injection of a primitive function */
  def inject(x: (String, List[A] => Option[A])): A
  /** Injection of a continuation */
  def inject[Kont <: Kontinuation](x: Kont): A
  /** Injection of a closure */
  def inject[Addr : Address, Exp <: Expression](x: (Exp, Environment[Addr])): A
}

class Primitives[Abs, Addr](implicit abs: AbstractValue[Abs], i: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]) {
  type Primitive = List[Abs] => Option[Abs]

  private def unOp(name: String, f: Abs => Abs): (String, Primitive) = (name, {
    case x :: Nil => Some(f(x))
    case _ => None
  })
  private def binOp(name: String, f: (Abs, Abs) => Abs): (String, Primitive) = (name, {
    case x :: y :: Nil => Some(f(x, y))
    case _ => None
  })

  val all: List[(String, Primitive)] = List(
    binOp("+", abs.plus)
  )

  val forEnv: List[(String, Addr)] = all.map({ case (name, _) => (name, addri.primitive(name)) })
  val forStore: List[(Addr, Abs)] = all.map({ case (name, f) => (addri.primitive(name), i.inject((name, f))) })
}

object AbstractValue
