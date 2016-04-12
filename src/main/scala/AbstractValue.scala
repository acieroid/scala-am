import scalaz.Semigroup

/** These are the unary operations that should be supported by lattices */
object UnaryOperator extends Enumeration {
  type UnaryOperator = Value
  val IsNull, IsCons, IsChar, IsSymbol, IsString, IsInteger, IsFloat, IsBoolean, IsVector, IsLock, /* Checks the type of a value */
    IsLocked, /* Checks if a lock is considered locked */
    Not, /* Negate a value */
    Ceiling, Log, Random, /* Unary arithmetic operations */
    VectorLength, StringLength, /* Length operations */
    NumberToString /* Conversions */
  = Value
}

/** Binary operations thatt should be supported by lattices */
object BinaryOperator extends Enumeration {
  type BinaryOperator = Value
  val Plus, Minus, Times, Div, Modulo, /* Arithmetic operations */
    Lt, /* Arithmetic comparison */
    NumEq, Eq, /* Equality checking (number equality, physical equality) */
    StringAppend /* string operations */
  = Value
}

/**
 * Exception to be raised during injection if an element of the lattice is not
 * supported (e.g., a lattice that doesn't support primitives)
 */
object UnsupportedLatticeElement extends Exception

/**
 * Exception to be raised when multiple values cannot be joined
 */
case class CannotJoin[Abs](values: Set[Abs]) extends Exception {
  override def toString = "CannotJoin(" + values.mkString(", ") + ")"
}

trait StoreShow[A] {
  def shows[Addr : Address, Abs : AbstractValue](v: A, store: Store[Addr, Abs]): String
}

/** Abstract values are abstract representations of the possible values of a variable */
trait AbstractValue[A] extends Semigroup[A] with StoreShow[A] {
  /** Name of this lattice */
  def name: String
  /** Does this lattice support abstract counting? */
  def counting: Boolean

  def shows[Addr : Address, Abs : AbstractValue](v: A, store: Store[Addr, Abs]) = v.toString


  /** Can this abstract value be considered true for conditionals? */
  def isTrue(x: A): Boolean
  /** Can this abstract value be considered false for conditionals? */
  def isFalse(x: A): Boolean
  /** Is this an erroneous value? (and only an error) */
  def isError(x: A): Boolean
  /** Is this an error-free value? */
  def isNotError(x: A): Boolean
  /** Is this a primitive value, i.e. a value that contains no address? */
  def isPrimitiveValue(x: A): Boolean
  /** Performs an unary operation on the abstract value x */
  def unaryOp(op: UnaryOperator.UnaryOperator)(x: A): A
  /** Performs a binary operation on abstract values x and y */
  def binaryOp(op: BinaryOperator.BinaryOperator)(x: A, y: A): A
  /** Join operation on lattice elements  */
  def join(x: A, y: A): A
  def append(x: A, y: => A): A = join(x, y)
  /** Meet operation on lattice elements */
  def meet(x: A, y: A): A
  /** Checks whether x subsumes y */
  def subsumes(x: A, y: A): Boolean
  /** Conjunction */
  def and(x: A, y: => A): A
  /** Disjunction */
  def or(x: A, y: => A): A
  /** Takes the car of a cons cell */
  def car[Addr : Address](x: A): Set[Addr]
  /** Takes the cdr of a cons cell */
  def cdr[Addr : Address](x: A): Set[Addr]
  /** Get a value from a vector. Returns either an error or the addresses where to look for the values */
  def vectorRef[Addr : Address](vector: A, index: A): Set[Either[A, Addr]]
  /** Changes a value inside a vector. The address given is an address where the
   * value can be stored if needed.  Returns the vector value, as well as the
   * addresses to update in the store. The value stored is not passed to
   * vectorSet, but will be stored in the returned addresses. */
  def vectorSet[Addr : Address](vector: A, index: A, addr: Addr): (A, Set[Addr])
  /** Returns the string representation of this value */
  def toString[Addr : Address](x: A, store: Store[Addr, A]): String

  /** Extract closures contained in this value */
  def getClosures[Exp : Expression, Addr : Address](x: A): Set[(Exp, Environment[Addr])]
  /** Extract primitives contained in this value */
  def getPrimitives[Addr : Address, Abs : AbstractValue](x: A): Set[Primitive[Addr, Abs]]
  /** Extract thread ids contained in this value */
  def getTids[TID : ThreadIdentifier](x: A): Set[TID]
  /** Extract vector addresses contained in this value */
  def getVectors[Addr : Address](x: A): Set[Addr]
  /** Extract lock addresses contained in this value */
  def getLocks[Addr : Address](x: A): Set[Addr]

  /** Bottom element of the lattice */
  def bottom: A
  /** Injection of an error value */
  def error(x: A): A
  /** Injection of an integer */
  def inject(x: Int): A
  /** Injection of a float */
  def inject(x: Float): A
  /** Injection of a string */
  def inject(x: String): A
  /** Injection of a boolean */
  def inject(x: Boolean): A
  /** Injection of a character */
  def inject(x: Char): A
  /** Injection of a primitive function */
  def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]): A
  /** Injection of a closure */
  def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): A
  /** Inject a thread id */
  def injectTid[TID : ThreadIdentifier](tid: TID): A
  /** Injection of a symbol */
  def injectSymbol(x: String): A
  /** Creates a cons cell */
  def cons[Addr : Address](car: Addr, cdr: Addr): A
  /** Creates a vector that resides at address @param addr, has size @param size
    * and initial stored at address @param init. Returns two values: one containing
    * the address of the vector, and the other being the vector value itself. */
  def vector[Addr : Address](addr: Addr, size: A, init: Addr): (A, A)
  /** Creates a lock wrapper (that contains the address of the lock) */
  def lock[Addr : Address](addr: Addr): A
  /** The locked value */
  def lockedValue: A
  /** The unlocked value */
  def unlockedValue: A
  /** Nil value */
  def nil: A
}

object AbstractValue

trait Lattice {
  type L
  val isAbstractValue: AbstractValue[L]
}
