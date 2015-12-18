import scalaz.Semigroup

/**
 * Each primitive has to implement this trait.
 */
trait Primitive[Addr, Abs] {
  /** The name of the primitive */
  val name: String
  /** Calls the primitive.
   * @param fexp: the expression with which the primitive has been called
   * @param args: the arguments with which the primitive has been called, both their expression and their value
   * @param store: the store
   * @return either an error, or the value returned by the primitive along with the updated store
   */
  def call[Exp : Expression, Time : Timestamp](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time): Either[String, (Abs, Store[Addr, Abs])]
}

/** These are the unary operations that should be supported by lattices */
object UnaryOperator extends Enumeration {
  type UnaryOperator = Value
  val IsNull, IsCons, IsChar, IsSymbol, IsString, IsInteger, IsFloat, IsBoolean, /* Checks the type of a value */
    Not, /* Negate a value */
    Ceiling, Log, Random /* Unary arithmetic operations */
  = Value
}
import UnaryOperator._

/** Binary operations thatt should be supported by lattices */
object BinaryOperator extends Enumeration {
  type BinaryOperator = Value
  val Plus, Minus, Times, Div, Modulo, /* Arithmetic operations */
    Lt, /* Arithmetic comparison */
    NumEq, Eq /* Equality checking (number equality, physical equality) */
  = Value
}
import BinaryOperator._

/**
 * Exception to be raised during injection if an element of the lattice is not
 * supported (e.g., a lattice that doesn't support primitives)
 */
object UnsupportedLatticeElement extends Exception

/**
 * Exception to be raised when two objects cannot be joined
 */
case class CannotJoin[Abs](x: Abs, y: Abs) extends Exception

/** Abstract values are abstract representations of the possible values of a variable */
trait AbstractValue[A] extends Semigroup[A] {
  /** Name of this lattice */
  def name: String

  /** Can this abstract value be considered true for conditionals? */
  def isTrue(x: A): Boolean
  /** Can this abstract value be considered false for conditionals? */
  def isFalse(x: A): Boolean
  /** Is this an erroneous value? (and only an erroneous value) */
  def isError(x: A): Boolean
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
  /** Returns the string representation of this value */
  def toString[Addr : Address](x: A, store: Store[Addr, A]): String

  /** Extract closures contained in this value */
  def getClosures[Exp : Expression, Addr : Address](x: A): Set[(Exp, Environment[Addr])]
  /** Extract primitives contained in this value */
  def getPrimitives[Addr : Address, Abs : AbstractValue](x: A): Set[Primitive[Addr, Abs]]
  /** Extract thread ids contained in this value */
  def getTids[TID : ThreadIdentifier](x: A): Set[TID]

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
  // /** Creates a vector */
  // def vector(size: A, init: A): A
  /** Nil value */
  def nil: A
}

/** This is where we define (Scheme) primitives */
class Primitives[Addr : Address, Abs : AbstractValue] {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]

  /** Some shortcuts */
  def isNull = abs.unaryOp(UnaryOperator.IsNull) _
  def isCons = abs.unaryOp(UnaryOperator.IsCons) _
  def isChar = abs.unaryOp(UnaryOperator.IsChar) _
  def isSymbol = abs.unaryOp(UnaryOperator.IsSymbol) _
  def isString = abs.unaryOp(UnaryOperator.IsString) _
  def isInteger = abs.unaryOp(UnaryOperator.IsInteger) _
  def isFloat = abs.unaryOp(UnaryOperator.IsFloat) _
  def isBoolean = abs.unaryOp(UnaryOperator.IsBoolean) _
  def ceiling = abs.unaryOp(UnaryOperator.Ceiling) _
  def log = abs.unaryOp(UnaryOperator.Log) _
  def not = abs.unaryOp(UnaryOperator.Not) _
  def random = abs.unaryOp(UnaryOperator.Random) _
  def plus = abs.binaryOp(BinaryOperator.Plus) _
  def minus = abs.binaryOp(BinaryOperator.Minus) _
  def times = abs.binaryOp(BinaryOperator.Times) _
  def div = abs.binaryOp(BinaryOperator.Div) _
  def modulo = abs.binaryOp(BinaryOperator.Modulo) _
  def lt = abs.binaryOp(BinaryOperator.Lt) _
  def numEq = abs.binaryOp(BinaryOperator.NumEq) _
  def eq = abs.binaryOp(BinaryOperator.Eq) _

  /** This is how a primitive is defined by extending Primitive */
  object Cons extends Primitive[Addr, Abs] {
    val name = "cons"
    def call[Exp : Expression, Time : Timestamp](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (carexp, car) :: (cdrexp, cdr) :: Nil => {
        val cara = addr.cell(carexp, t)
        val cdra = addr.cell(cdrexp, t)
        Right((abs.cons(cara, cdra), store.extend(cara, car).extend(cdra, cdr)))
      }
      case l => Left(s"cons: 2 operands expected, got ${l.size} instead")
    }
  }

  /* Other primitives are much simpler than cons and sometimes don't need access
   * to the store, or have a fixed amount of arguments, so we define a few
   * helper classes to define them */

  /** A primitive taking no argument, e.g., (newline) */
  class NullaryOperation(val name: String, f: => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case Nil => Right((f, store))
      case l => Left(s"${name}: no operand expected, got ${l.size} instead")
    }
  }
  object NullaryOperation {
    def apply(name: String, f: => Abs) = new NullaryOperation(name, f)
  }
  /** A primitive taking a single argument, e.g., (random 1) */
  case class UnaryOperation(name: String, f: Abs => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => Right((f(x), store))
      case l => Left(s"${name}: 1 operand expected, got ${l.size} instead")
    }
  }
  /** A primitive taking two arguments, e.g., (modulo 5 1) */
  case class BinaryOperation(name: String, f: (Abs, Abs) => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: (_, y) :: Nil => Right((f(x, y), store))
      case l => Left(s"${name}: 2 operands expected, got ${l.size} instead")
    }
  }

  /** A primitive taking a single argument and modifying the store */
  case class UnaryStoreOperation(name: String, f: (Abs, Store[Addr, Abs]) => (Abs, Store[Addr, Abs])) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => Right(f(x, store))
      case l => Left(s"${name}: 1 operand expected, got ${l.size} instead")
    }
  }

  /** A primitive taking two arguments and modifying the store */
  case class BinaryStoreOperation(name: String, f: (Abs, Abs, Store[Addr, Abs]) => (Abs, Store[Addr, Abs])) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: (_, y) :: Nil => Right(f(x, y, store))
      case l => Left(s"${name}: 2 operand expected, got ${l.size} instead")
    }
  }

  /** A primitive that doesn't modify the store but takes a variable amount of arguments */
  abstract class VariadicOperation extends Primitive[Addr, Abs] {
    def call(args: List[Abs]): Either[String, Abs]
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) =
      call(args.map({ case (_, v) => v })) match {
        case Right(v) => Right((v, store))
        case Left(err) => Left(err)
      }
  }

  /* Some primitives are defined by extending one of the helper class */

  object Plus extends VariadicOperation {
    val name = "+"
    def call(args: List[Abs]) = args match {
      case Nil => Right(abs.inject(0))
      case x :: rest => call(rest) match {
        case Right(y) => Right(plus(x, y))
        case Left(err) => Left(err)
      }
    }
  }
  object Minus extends VariadicOperation {
    val name = "-"
    def call(args: List[Abs]) = args match {
      case Nil => Left("-: at least 1 operand expected, got 0")
      case x :: Nil => Right(minus(abs.inject(0), x))
      case x :: rest => Plus.call(rest) match {
        case Right(y) => Right(minus(x, y))
        case Left(err) => Left(err)
      }
    }
  }
  object Times extends VariadicOperation {
    val name = "*"
    def call(args: List[Abs]) = args match {
      case Nil => Right(abs.inject(1))
      case x :: rest => call(rest) match {
        case Right(y) => Right(times(x, y))
        case Left(err) => Left(err)
      }
    }
  }
  object Div extends VariadicOperation {
    val name = "/"
    def call(args: List[Abs]) = args match {
      case Nil => Left("/: at least 1 operand expected, got 0")
      case x :: rest => Times.call(rest) match {
        case Right(y) => Right(div(x, y))
        case Left(err) => Left(err)
      }
    }
  }
  object Max extends VariadicOperation {
    /* TODO: In Scheme, max casts numbers to inexact as soon as one of them is inexact, but we don't support that */
    val name = "max"
    private def call(args: List[Abs], max: Abs): Abs = args match {
      case Nil => max
      case x :: rest => {
        val test = lt(max, x)
        if (abs.isError(test)) {
          test
        } else {
          val t = if (abs.isTrue(test)) { call(rest, x) } else { abs.bottom }
          val f = if (abs.isFalse(test)) { call(rest, max) } else { abs.bottom }
          abs.join(t, f)
        }
      }
    }
    def call(args: List[Abs]) = args match {
      case Nil => Left("max: at least 1 operand expected, got 0")
      case x :: rest => Right(call(rest, x))
    }
  }

  /* Some primitives can be defined as just a function that we pass to one of the helper class' constructor */

  private def newline: Abs = {
    println("")
    abs.bottom
  }
  private def display(v: Abs): Abs = { print(v); abs.bottom }

  private def car(v: Abs, store: Store[Addr, Abs]): Abs =
    abs.car(v).foldLeft(abs.bottom)((acc, a) => abs.join(acc, store.lookup(a)))

  private def cdr(v: Abs, store: Store[Addr, Abs]): Abs =
    abs.cdr(v).foldLeft(abs.bottom)((acc, a) => abs.join(acc, store.lookup(a)))

  private def abs(v: Abs): Abs = {
    val test = lt(v, abs.inject(0))
    if (abs.isError(test)) {
      test
    } else {
      val t = if (abs.isTrue(test)) { minus(abs.inject(0), v) } else { abs.bottom }
      val f = if (abs.isFalse(test)) { v } else { abs.bottom }
      abs.join(t, f)
    }
  }

  /* Among them, recursive primitives need to be defined as a fixpoint operation,
   * otherwise abstract values (e.g., gcd(Int, Int)) can lead to infinite
   * loops */

  /** (define (gcd a b) (if (= b 0) a (gcd b (modulo a b)))) */
  private def gcd(a: Abs, b: Abs, visited: Set[(Abs, Abs)]): Abs = {
    if (visited.contains(a, b)) {
      abs.bottom
    } else {
      val cond = numEq(b, abs.inject(0))
      if (abs.isError(cond)) {
        cond
      } else {
        val t = if (abs.isTrue(cond)) { a } else { abs.bottom }
        val f = if (abs.isFalse(cond)) { gcd(b, modulo(a, b), visited + ((a, b))) } else { abs.bottom }
        abs.join(t, f)
      }
    }
  }
  private def gcd(a: Abs, b: Abs): Abs = gcd(a, b, Set())

  /** (define (equal? a b) (or (eq? a b) (and (null? a) (null? b)) (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))) */
  private def equal(a: Abs, b: Abs, store: Store[Addr, Abs], visited: Set[(Abs, Abs)]): Abs = {
    if (visited.contains(a, b)) {
      abs.bottom
    } else {
      val visited2 = visited + ((a, b))
      abs.or(eq(a, b),
        abs.or(abs.and(isNull(a), isNull(b)),
          abs.and(isCons(a),
            abs.and(isCons(b),
              abs.and(equal(car(a, store), car(b, store), store, visited2),
                equal(cdr(a, store), cdr(b, store), store, visited2))))))
    }
  }
  private def equal(a: Abs, b: Abs, store: Store[Addr, Abs]): Abs = equal(a, b, store, Set())

  /** (define (length l) (if (pair? l) (+ 1 (length (cdr l))) (if (null? l) 0 (error "length called with a non-list")))) */
  private def length(l: Abs, store: Store[Addr, Abs], visited: Set[Abs]): Abs = {
    if (visited.contains(l)) {
      abs.bottom
    } else {
      val visited2 = visited + l
      val cond = isCons(l)
      val t = if (abs.isTrue(cond)) { plus(abs.inject(1), length(cdr(l, store), store, visited2)) } else { abs.bottom }
      val f = if (abs.isFalse(cond)) {
        val fcond = isNull(l)
        val ft = if (abs.isTrue(fcond)) { abs.inject(0) } else { abs.bottom }
        val ff = if (abs.isFalse(fcond)) { abs.error(abs.inject("length called with a non-list")) } else { abs.bottom }
        abs.join(ft, ff)
      } else {
        abs.bottom
      }
      abs.join(t, f)
    }
  }
  private def length(l: Abs, store: Store[Addr, Abs]): Abs = length(l, store, Set())

  /** Bundles all the primitives together */
  val all: List[Primitive[Addr, Abs]] = List(
    Plus, Minus, Times, Div, Max,
    BinaryOperation("quotient", div),
    BinaryOperation("<", lt), // TODO: <, <=, =, >, >= should accept any number of arguments
    BinaryOperation("<=", (x, y) => abs.or(lt(x, y), numEq(x, y))),
    BinaryOperation("=", numEq),
    BinaryOperation(">", (x, y) => abs.and(not(lt(x, y)), not(numEq(x, y)))),
    BinaryOperation(">=", (x, y) => not(lt(x, y))),
    BinaryOperation("modulo", modulo),
    BinaryOperation("gcd", gcd),
    UnaryOperation("not", not),
    UnaryOperation("random", random),
    UnaryOperation("ceiling", ceiling),
    UnaryOperation("log", log),
    UnaryOperation("zero?", (x) => numEq(abs.inject(0), x)), /* (define (zero? x) (= x 0)) */
    UnaryOperation("positive?", (x) => lt(abs.inject(0), x)), /* (define (positive? x) (< 0 x)) */
    UnaryOperation("negative?", (x) => lt(x, abs.inject(0))), /* (define (negative? x) (< x 0)) */
    UnaryOperation("odd?", (x) => numEq(abs.inject(1), modulo(x, abs.inject(2)))), /* (define (odd? x) (= 1 (modulo x 2))) */
    UnaryOperation("even?", (x) => numEq(abs.inject(0), modulo(x, abs.inject(2)))), /* (define (even? x) (= 0 (modulo x 2))) */
    UnaryOperation("display", display),
    NullaryOperation("newline", newline),
    Cons,
    UnaryStoreOperation("car", (v, store) => (car(v, store), store)),
    UnaryStoreOperation("cdr", (v, store) => (cdr(v, store), store)),
    UnaryStoreOperation("caar", (v, store) => (car(car(v, store), store), store)),
    UnaryStoreOperation("cadr", (v, store) => (car(cdr(v, store), store), store)),
    UnaryStoreOperation("cddr", (v, store) => (cdr(cdr(v, store), store), store)),
    UnaryStoreOperation("cdar", (v, store) => (cdr(car(v, store), store), store)),
    UnaryStoreOperation("caaar", (v, store) => (car(car(car(v, store), store), store), store)),
    UnaryStoreOperation("cdaar", (v, store) => (cdr(car(car(v, store), store), store), store)),
    UnaryStoreOperation("caadr", (v, store) => (car(car(cdr(v, store), store), store), store)),
    UnaryStoreOperation("cdadr", (v, store) => (cdr(car(cdr(v, store), store), store), store)),
    UnaryStoreOperation("caddr", (v, store) => (car(cdr(cdr(v, store), store), store), store)),
    UnaryStoreOperation("cdddr", (v, store) => (cdr(cdr(cdr(v, store), store), store), store)),
    UnaryStoreOperation("cadar", (v, store) => (car(cdr(car(v, store), store), store), store)),
    UnaryStoreOperation("cddar", (v, store) => (cdr(cdr(car(v, store), store), store), store)),
    BinaryStoreOperation("set-car!", (cell, v, store) =>
      (abs.bottom,
        abs.car(cell).foldLeft(store)((acc, a) => acc.update(a, v)))),
    BinaryStoreOperation("set-cdr!", (cell, v, store) =>
      (abs.bottom,
        abs.cdr(cell).foldLeft(store)((acc, a) => acc.update(a, v)))),
    UnaryOperation("error", abs.error),
    UnaryOperation("null?", isNull),
    UnaryOperation("pair?", isCons),
    UnaryOperation("char?", isChar),
    UnaryOperation("symbol?", isSymbol),
    UnaryOperation("string?", isString),
    UnaryOperation("integer?", isInteger),
    UnaryOperation("number?", x => abs.or(isInteger(x), isFloat(x))), /* TODO: complex numbers */
    UnaryOperation("real?", x => abs.or(isInteger(x), isFloat(x))),
    UnaryOperation("boolean?", isBoolean),
    BinaryOperation("eq?", eq),
    BinaryStoreOperation("equal?", (a, b, store) => (equal(a, b, store), store)),
    UnaryStoreOperation("length", (v, store) => (length(v, store), store)),
    UnaryOperation("abs", abs)
    // BinaryStoreOperation("make-vector", makeVector _),
    // UnaryOperation("vector?", isVector),
    // BinaryOperation(
  )

  private val allocated = all.map({ prim => (prim.name, addr.primitive(prim.name), abs.inject(prim)) })
  val forEnv: List[(String, Addr)] = allocated.map({ case (name, a, _) => (name, a) })
  val forStore: List[(Addr, Abs)] =  allocated.map({ case (_, a, v) => (a, v) })
}

object AbstractValue

trait Lattice {
  type L
  val isAbstractValue: AbstractValue[L]
}
