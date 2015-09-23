import scalaz.Semigroup

trait Primitive[Addr, Abs] {
  val name: String
  def call[Exp : Expression](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs]): Either[String, (Abs, Store[Addr, Abs])]
}

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
  /** x % y */
  def modulo(x: A, y: A): A
  /** Computes the ceiling of x */
  def ceiling(x: A): A
  /** Computes the log of x */
  def log(x: A): A
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
  /** Takes the car of a cons cell */
  def car[Addr : Address](x: A): Either[A, Addr]
  /** Takes the cdr of a cons cell */
  def cdr[Addr : Address](x: A): Either[A, Addr]

  /** Returns a random integer bounded by x*/
  def random(x: A): A

  def getKonts(x: A): Set[Kontinuation]
  def getClosures[Exp : Expression, Addr : Address](x: A): Set[(Exp, Environment[Addr])]
  def getPrimitive[Addr : Address](x: A): Option[Primitive[Addr, A]]
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
  def inject[Addr : Address](x: Primitive[Addr, A]): A
  /** Injection of a continuation */
  def inject[Kont <: Kontinuation](x: Kont): A
  /** Injection of a closure */
  def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): A
  /** Injection of a symbol */
  def injectSymbol(x: String): A
  /** Creates a cons cell */
  def cons[Addr : Address](car: Addr, cdr: Addr): A
}

class Primitives[Addr, Abs](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]) {
  class NullaryOperation(val name: String, f: => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs]) = args match {
      case Nil => Right((f, store))
      case l => Left(s"${name}: no operand expected, got ${l.size} instead")
    }
  }
  object NullaryOperation {
    def apply(name: String, f: => Abs) = new NullaryOperation(name, f)
  }
  case class UnaryOperation(name: String, f: Abs => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs]) = args match {
      case (_, x) :: Nil => Right((f(x), store))
      case l => Left(s"${name}: 1 operand expected, got ${l.size} instead")
    }
  }
  case class BinaryOperation(name: String, f: (Abs, Abs) => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs]) = args match {
      case (_, x) :: (_, y) :: Nil => Right((f(x, y), store))
      case l => Left(s"${name}: 2 operands expected, got ${l.size} instead")
    }
  }

  object Cons extends Primitive[Addr, Abs] {
    val name = "cons"
    def call[Exp : Expression](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs]) = args match {
      case (carexp, car) :: (cdrexp, cdr) :: Nil => {
        val cara = addri.cell(carexp)
        val cdra = addri.cell(cdrexp)
        Right((absi.cons(cara, cdra), store.extend(cara, car).extend(cdra, cdr)))
      }
      case l => Left(s"cons: 2 operands expected, got ${l.size} instead")
    }
  }

  case class UnaryStoreOperation(name: String, f: (Abs, Store[Addr, Abs]) => (Abs, Store[Addr, Abs])) extends Primitive[Addr, Abs] {
    def call[Exp : Expression](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs]) = args match {
      case (_, x) :: Nil => Right(f(x, store))
      case l => Left(s"${name}: 1 operand expected, got ${l.size} instead")
    }
  }

  case class BinaryStoreOperation(name: String, f: (Abs, Abs, Store[Addr, Abs]) => (Abs, Store[Addr, Abs])) extends Primitive[Addr, Abs] {
    def call[Exp : Expression](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs]) = args match {
      case (_, x) :: (_, y) :: Nil => Right(f(x, y, store))
      case l => Left(s"${name}: 2 operand expected, got ${l.size} instead")
    }
  }


  private def newline: Abs = {
    println("")
    absi.bottom
  }
  private def display(v: Abs): Abs = { print(v); absi.bottom }

  /* TODO: handle +, -, etc. with no fixed number of argument (e.g., (+ 1), (+ 1 2 3), etc.) */
  val all: List[Primitive[Addr, Abs]] = List(
    BinaryOperation("+", abs.plus),
    BinaryOperation("-", abs.minus),
    BinaryOperation("*", abs.times),
    BinaryOperation("/", abs.div),
    BinaryOperation("<", abs.lt),
    BinaryOperation("<=", (x, y) => abs.or(abs.lt(x, y), abs.numEq(x, y))),
    BinaryOperation("=", abs.numEq),
    BinaryOperation(">", (x, y) => abs.and(abs.not(abs.lt(x, y)), abs.not(abs.numEq(x, y)))),
    BinaryOperation(">=", (x, y) => abs.not(abs.lt(x, y))),
    BinaryOperation("modulo", abs.modulo),
    UnaryOperation("not", abs.not),
    UnaryOperation("random", abs.random),
    UnaryOperation("ceiling", abs.ceiling),
    UnaryOperation("log", abs.log),
    UnaryOperation("display", display),
    NullaryOperation("newline", newline),
    Cons,
    UnaryStoreOperation("car", (v, store) => (abs.car(v) match {
      case Left(v) => ??? /* TODO: should return an option instead of an Either */
      case Right(a) => store.lookup(a)
    }, store)),
    UnaryStoreOperation("cdr", (v, store) => (abs.cdr(v) match {
      case Left(v) => ???
      case Right(a) => store.lookup(a)
    }, store)),
    BinaryStoreOperation("set-car!", (cell, v, store) => abs.car(cell) match {
      case Left(v) => { println(cell); ??? }
      case Right(a) => (absi.bottom, store.extend(a, v))
    }),
    BinaryStoreOperation("set-cdr!", (cell, v, store) => abs.cdr(cell) match {
      case Left(v) => ???
      case Right(a) => (absi.bottom, store.extend(a, v))
    })
  )

  private val allocated = all.map({ prim => (prim.name, addri.primitive(prim.name), absi.inject(prim)) })
  val forEnv: List[(String, Addr)] = allocated.map({ case (name, a, _) => (name, a) })
  val forStore: List[(Addr, Abs)] = allocated.map({ case (_, a, v) => (a, v) })
}

object AbstractValue
