import scalaz.Semigroup

/** Abstract values are abstract representations of the possible values of a variable */
trait AbstractValue[A] extends Semigroup[A] {
  /** Can this abstract value be considered true for conditionals? */
  def isTrue(x: A): Boolean
  /** Can this abstract value be considered false for conditionals? */
  def isFalse(x: A): Boolean
  /** Is this an erroneous value? */
  def isError(x: A): Boolean
  /** Fold a function over the values contained in this abstract values. This
      should be redefined only for container-like abstract values (e.g., for a
      set abstraction) */
  def foldValues[B](x: A, f: A => Set[B]): Set[B]
  /** Join operation on lattice elements  */
  def join(x: A, y: A): A
  def append(x: A, y: => A): A = join(x, y)
  /** Meet operation on lattice elements */
  def meet(x: A, y: A): A
  /** Checks whether x subsumes y */
  def subsumes(x: A, y: A): Boolean
  /** Adds two values */
  def plus(x: A, y: A): A
  /** Subtracts two values */
  def minus(x: A, y: A): A
  /** Multiplies two values */
  def times(x: A, y: A): A
  /** Divides two values */
  def div(x: A, y: A): A
  /** Is x strictly smaller than y? */
  def lt(x: A, y: A): A
  /** Is x numerically equal to y? */
  def numEq(x: A, y: A): A
  /** Negation */
  def not(x: A): A
  /** Conjunction */
  def and(x: A, y: A): A
  /** Disjunction */
  def or(x: A, y: A): A

  /** Returns a random integer bounded by x*/
  def random(x: A): A

  def getKont(x: A): Option[Kontinuation]
  def getClosure[Exp : Expression, Addr : Address](x: A): Option[(Exp, Environment[Addr])]
  def getPrimitive(x: A): Option[(String, List[A] => Either[String, A])]
}

/** Concrete values have to be injected to become abstract */
trait AbstractInjection[A] {
  /** Name of this lattice */
  def name: String
  /** Bottom element of the lattice */
  def bottom: A
  /** Injection of an integer */
  def inject(x: Int): A
  /** Injection of a string */
  def inject(x: String): A
  /** Injection of a boolean */
  def inject(x: Boolean): A
  /** Injection of a primitive function */
  def inject(x: (String, List[A] => Either[String, A])): A
  /** Injection of a continuation */
  def inject[Kont <: Kontinuation](x: Kont): A
  /** Injection of a closure */
  def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): A
  /** Injection of a symbol */
  def injectSymbol(x: String): A
}

class Primitives[Abs, Addr](implicit abs: AbstractValue[Abs], i: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]) {
  type Primitive = List[Abs] => Either[String, Abs]

  private def unOp(name: String, f: Abs => Abs): (String, Primitive) = (name, {
    case x :: Nil => Right(f(x))
    case l => Left(s"${name}: 1 operand expected, got ${l.size} instead")
  })
  private def binOp(name: String, f: (Abs, Abs) => Abs): (String, Primitive) = (name, {
    case x :: y :: Nil => Right(f(x, y))
    case l => Left(s"${name}: 2 operands expected, got ${l.size} instead")
  })

  /* TODO: handle +, -, etc. with no fixed number of argument (e.g., (+ 1), (+ 1 2 3), etc.) */
  val all: List[(String, Primitive)] = List(
    binOp("+", abs.plus),
    binOp("-", abs.minus),
    binOp("*", abs.times),
    binOp("/", abs.div),
    binOp("<", abs.lt),
    binOp("<=", (x, y) => abs.or(abs.lt(x, y), abs.numEq(x, y))),
    binOp("=", abs.numEq),
    binOp(">", (x, y) => abs.and(abs.not(abs.lt(x, y)), abs.not(abs.numEq(x, y)))),
    binOp(">=", (x, y) => abs.not(abs.lt(x, y))),
    unOp("not", abs.not),
    unOp("random", abs.random)
  )
  private val allocated = all.map({ case (name, f) => (name, addri.primitive(name), i.inject((name, f))) })
  val forEnv: List[(String, Addr)] = allocated.map({ case (name, a, _) => (name, a) })
  val forStore: List[(Addr, Abs)] = allocated.map({ case (_, a, v) => (a, v) })
}

object AbstractValue
