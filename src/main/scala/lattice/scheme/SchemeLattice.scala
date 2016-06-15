import scalaz.{Plus => _, _}
import scalaz.Scalaz._
import SchemeOps._

case class CannotAccessVector(vector: String) extends SemanticError
case class CannotAccessCar(v: String) extends SemanticError
case class CannotAccessCdr(v: String) extends SemanticError

/** A lattice for Scheme should support the following operations */
trait IsSchemeLattice[L] extends JoinLattice[L] {
  /** Can this value be considered true for conditionals? */
  def isTrue(x: L): Boolean
  /** Can this value be considered false for conditionals? */
  def isFalse(x: L): Boolean
  /** Performs a unary operation on the abstract value x */
  def unaryOp(op: SchemeOps.UnaryOperator)(x: L): MayFail[L]
  /** Performs a binary operation on abstract values x and y */
  def binaryOp(op: SchemeOps.BinaryOperator)(x: L, y: L): MayFail[L]
  /** Conjunction */
  def and(x: L, y: => L): L
  /** Disjunction */
  def or(x: L, y: => L): L
  /** Extract closures contained in this value */
  def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])]
  /** Extract primitives contained in this value */
  def getPrimitives[Addr : Address, Abs : JoinLattice](x: L): Set[Primitive[Addr, Abs]]

  /** Injection of an integer */
  def inject(x: Int): L
  /** Injection of a float */
  def inject(x: Float): L
  /** Injection of a string */
  def inject(x: String): L
  /** Injection of a boolean */
  def inject(x: Boolean): L
  /** Injection of a character */
  def inject(x: Char): L
  /** Injection of a primitive function */
  def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): L
  /** Injection of a closure */
  def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L
  /** Injection of a symbol */
  def injectSymbol(x: String): L
  /** Creates a cons cell */
  def cons[Addr : Address](car: Addr, cdr: Addr): L
  /** Nil value */
  def nil: L

  /** Takes the car of a cons cell */
  def car[Addr : Address](x: L): Set[Addr]
  /** Takes the cdr of a cons cell */
  def cdr[Addr : Address](x: L): Set[Addr]
  /** Get a value from a vector. Returns the addresses where to look for the values */
  def vectorRef[Addr : Address](vector: L, index: L): MayFail[Set[Addr]]
  /** Changes a value inside a vector. The address given is an address where the
   * value can be stored if needed. Returns the vector value, as well as the
   * addresses to update in the store. The value stored is not passed to
   * vectorSet, but will be stored in the returned addresses. */
  def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])]
  /** Extract vector addresses contained in this value */
  def getVectors[Addr : Address](x: L): Set[Addr]
  /** Creates a vector of the given size, where the initial value lies at
   * address. Return the vector address wrapped in a lattice value, as well as
   * the vector value itsel */
  def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)]

}

trait SchemeLattice {
  type L
  val isSchemeLattice: IsSchemeLattice[L]
}
