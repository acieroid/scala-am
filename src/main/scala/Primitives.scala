import UnaryOperator._
import BinaryOperator._
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
  def call[Exp : Expression, Time : Timestamp](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time): Either[String, (Abs, Store[Addr, Abs], Set[Effect[Addr, Abs]])]
}


/** This is where we define (Scheme) primitives */
class Primitives[Addr : Address, Abs : AbstractValue] {
  def traced(prim: Primitive[Addr, Abs]): Primitive[Addr, Abs] = new Primitive[Addr, Abs] {
    val name = prim.name
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = {
      val argsstr = args.map({ case (_, v) => abs.shows(v, store) }).mkString(" ")
      val res = prim.call(fexp, args, store, t)
      res match {
        case Left(err) => println(s"($name $argsstr) -> ERROR: $err")
        case Right((v, store, effs)) => println(s"($name $argsstr) -> ${abs.shows(v, store)}")
      }
      res
    }
  }
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
  def isVector = abs.unaryOp(UnaryOperator.IsVector) _
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
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (carexp, car) :: (cdrexp, cdr) :: Nil => {
        val cara = addr.cell(carexp, t)
        val cdra = addr.cell(cdrexp, t)
        Right((abs.cons(cara, cdra), store.extend(cara, car).extend(cdra, cdr), Set()))
      }
      case l => Left(s"cons: 2 operands expected, got ${l.size} instead")
    }
  }

  object MakeVector extends Primitive[Addr, Abs] {
    val name = "make-vector"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, size) :: (initexp, init) :: Nil => {
        val test = isInteger(size)
        if (abs.isError(test))
          Right((test, store, Set()))
        else if (abs.isTrue(test)) {
          val a = addr.cell(fexp, t)
          val initaddr = addr.cell(initexp, t)
          val (va, vector) = abs.vector(a, size, initaddr)
          Right((va, store.extend(a, vector).extend(initaddr, init), Set()))
        } else {
          Left(s"make-vector: first operand should be an integer (was $size)")
        }
      }
      case l => Left(s"make-vector: 2 operands expected, got ${l.size} instead")
    }
  }

  object VectorSet extends Primitive[Addr, Abs] {
    val name = "vector-set!"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, vector) :: (_, index) :: (exp, value) :: Nil => {
        Right(abs.getVectors(vector).foldLeft((abs.bottom, store, Set[Effect[Addr, Abs]]()))((acc, va) => {
          val oldvec = store.lookup(va)
          val targetaddr = addr.cell(exp, t)
          val (vec, addrs) = abs.vectorSet(oldvec, index, targetaddr)
          (abs.join(acc._1, vec),
            addrs.foldLeft(acc._2.update(va, vec))((st, a) => st.updateOrExtend(a, value)),
            acc._3 ++ addrs.map(a => EffectWriteVector(a)))
        }))
      }
      case l => Left(s"vector-set!: 3 operands expected, got ${l.size} instead")
    }
  }

  object Vector extends Primitive[Addr, Abs] {
    val name = "vector"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = {
      val a = addr.cell(fexp, t)
      val botaddr = addr.primitive("__bottom__")
      val (va, emptyVector) = abs.vector(a, abs.inject(args.size), botaddr)
      val (vector, store2) = args.zipWithIndex.foldLeft((emptyVector, store))((acc, arg) => arg match {
        case ((exp, value), index) =>
          val valaddr = addr.cell(exp, t)
          abs.vectorSet(acc._1, abs.inject(index), valaddr) match {
            case (vec, addrs) => (vec, addrs.foldLeft(acc._2)((st, a) => st.updateOrExtend(a, value)))
          }
      })
      Right((va, store2.extend(a, vector), Set()))
    }
  }

  object Lock extends Primitive[Addr, Abs] {
    val name = "new-lock"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case Nil =>
        val a = addr.cell(fexp, t)
        Right(abs.lock(a), store.extend(a, abs.unlockedValue), Set())
      case l => Left(s"lock: no operand expected, got ${l.size} instead")
    }
  }

  /* Other primitives are much simpler than cons and sometimes don't need access
   * to the store, or have a fixed amount of arguments, so we define a few
   * helper classes to define them */

  /** A primitive taking no argument, e.g., (newline) */
  class NullaryOperation(val name: String, f: => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case Nil => Right((f, store, Set()))
      case l => Left(s"${name}: no operand expected, got ${l.size} instead")
    }
  }
  object NullaryOperation {
    def apply(name: String, f: => Abs) = new NullaryOperation(name, f)
  }

  /** A primitive taking a single argument, e.g., (random 1) */
  case class UnaryOperation(name: String, f: Abs => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => Right((f(x), store, Set()))
      case l => Left(s"${name}: 1 operand expected, got ${l.size} instead")
    }
  }
  /** A primitive taking two arguments, e.g., (modulo 5 1) */
  case class BinaryOperation(name: String, f: (Abs, Abs) => Abs) extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: (_, y) :: Nil => Right((f(x, y), store, Set()))
      case l => Left(s"${name}: 2 operands expected, got ${l.size} instead")
    }
  }

  /** A primitive taking a single argument and modifying the store */
  case class UnaryStoreOperation(name: String, f: (Abs, Store[Addr, Abs]) => (Abs, Store[Addr, Abs], Set[Effect[Addr, Abs]]))
    extends Primitive[Addr, Abs] {
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, x) :: Nil => Right(f(x, store))
      case l => Left(s"${name}: 1 operand expected, got ${l.size} instead")
    }
  }

  /** A primitive taking two arguments and modifying the store */
  case class BinaryStoreOperation(name: String, f: (Abs, Abs, Store[Addr, Abs]) => (Abs, Store[Addr, Abs], Set[Effect[Addr, Abs]]))
      extends Primitive[Addr, Abs] {
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
        case Right(v) => Right((v, store, Set()))
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

  object Min extends VariadicOperation {
    /* TODO: same remark as max */
    val name = "min"
     private def call(args: List[Abs], min: Abs): Abs = args match {
      case Nil => min
      case x :: rest => {
        val test = lt(x, min)
        if (abs.isError(test)) {
          test
        } else {
          val t = if (abs.isTrue(test)) { call(rest, x) } else { abs.bottom }
          val f = if (abs.isFalse(test)) { call(rest, min) } else { abs.bottom }
          abs.join(t, f)
        }
      }
    }
    def call(args: List[Abs]) = args match {
      case Nil => Left("min: at least 1 operand expected, got 0")
      case x :: rest => Right(call(rest, x))
    }
  }

  /* Some primitives can be defined as just a function that we pass to one of the helper class' constructor */

  private def newline: Abs = {
    println("")
    abs.inject(false)
  }

  private def car(v: Abs, store: Store[Addr, Abs]): (Abs, Set[Effect[Addr, Abs]]) = {
    val (res, effs) = abs.car(v).foldLeft((abs.bottom, Set[Effect[Addr, Abs]]()))((acc, a) =>
      (abs.join(acc._1, store.lookup(a)), acc._2 + EffectReadConsCar(a)))
    if (res == abs.bottom) {
      (abs.error(abs.inject(s"Cannot take car of $v")), effs)
    } else {
      (res, effs)
    }
  }

  private def cdr(v: Abs, store: Store[Addr, Abs]): (Abs, Set[Effect[Addr, Abs]]) = {
    val (res, effs) = abs.cdr(v).foldLeft((abs.bottom, Set[Effect[Addr, Abs]]()))((acc, a) =>
      (abs.join(acc._1, store.lookup(a)), acc._2 + EffectReadConsCdr(a)))
    if (res == abs.bottom) {
      (abs.error(abs.inject(s"Cannot take cdr of $v")), effs)
    } else {
      (res, effs)
    }
  }

  private def vectorRef(v: Abs, index: Abs, store: Store[Addr, Abs]): (Abs, Set[Effect[Addr, Abs]]) = {
    val (res, effs) = abs.getVectors(v).foldLeft((abs.bottom, Set[Effect[Addr, Abs]]()))((acc, va) =>
      abs.vectorRef(store.lookup(va), index).foldLeft(acc)((acc, res) => res match {
        case Left(v) => (abs.join(acc._1, v), acc._2)
        case Right(a) => (abs.join(acc._1, store.lookup(a)), acc._2 + EffectReadVector(a))
      }))
    if (res == abs.bottom) {
      (abs.error(abs.inject(s"Cannot access vector $v")), effs)
    } else {
      (res, effs)
    }
  }

  private def vectorLength(v: Abs, store: Store[Addr, Abs]): (Abs, Set[Effect[Addr, Abs]]) = {
    val (res, effs) = abs.getVectors(v).foldLeft((abs.bottom, Set[Effect[Addr, Abs]]()))((acc, va) =>
      (abs.join(acc._1, abs.unaryOp(VectorLength)(store.lookup(va))),
        acc._2 + EffectReadVariable(va)))
    if (res == abs.bottom) {
      (abs.error(abs.inject(s"Cannot access vector $v")), effs)
    } else {
      (res, effs)
    }
  }

  /** (define (abs x) (if (< x 0) (- 0 x) x)) */
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

  /** (define (equal? a b)
        (or (eq? a b)
          (and (null? a) (null? b))
          (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
          (and (vector? a) (vector? b)
            (let ((n (vector-length a)))
              (and (= (vector-length b) n)
                (letrec ((loop (lambda (i)
                                 (or (= i n)
                                   (and (equal? (vector-ref a i) (vector-ref b i))
                                     (loop (+ i 1)))))))
                  (loop 0)))))))
   */
  private def equalVecLoop(a: Abs, b: Abs, i: Abs, n: Abs, store: Store[Addr, Abs], visitedEqual: Set[(Abs, Abs)], visited: Set[(Abs, Abs, Abs, Abs)]): (Abs, Set[Effect[Addr, Abs]]) =
    if (visited.contains((a, b, i, n)) || a == abs.bottom || b == abs.bottom || i == abs.bottom || n == abs.bottom) {
      (abs.bottom, Set[Effect[Addr, Abs]]())
    } else {
      val numtest = numEq(i, n)
      val t = if (abs.isTrue(numtest)) { abs.inject(true) } else { abs.bottom }
      val (f, effects) = if (abs.isFalse(numtest)) {
        val (vai, effects1) = vectorRef(a, i, store)
        val (vbi, effects2) = vectorRef(b, i, store)
        val (itemtest, effects3) = equal(vai, vbi, store, visitedEqual)
        val (tt, effects4) = if (abs.isTrue(itemtest)) {
          equalVecLoop(a, b, abs.binaryOp(BinaryOperator.Plus)(i, abs.inject(1)), n, store, visitedEqual, visited + ((a, b, i, n)))
        } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
        val tf = if (abs.isFalse(itemtest)) { abs.inject(false) } else { abs.bottom }
        (abs.join(tt, tf), effects1 ++ effects2 ++ effects3 ++ effects4)
      } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
      (abs.join(t, f), effects)
    }

  private def equal(a: Abs, b: Abs, store: Store[Addr, Abs], visited: Set[(Abs, Abs)]): (Abs, Set[Effect[Addr, Abs]]) = {
    if (visited.contains(a, b) || a == abs.bottom || b == abs.bottom) {
      (abs.bottom, Set[Effect[Addr, Abs]]())
    } else {
      val visited2 = visited + ((a, b))
      val eqtest = eq(a, b)
      val t = if (abs.isTrue(eqtest)) { eqtest } else { abs.bottom }
      val (f, effects1) = if (abs.isFalse(eqtest)) {
        val nulltest = abs.and(isNull(a), isNull(b))
        val ft = if (abs.isTrue(nulltest)) { nulltest } else { abs.bottom }
        val (ff, effects2) = if (abs.isFalse(nulltest)) {
          val constest = abs.and(isCons(a), isCons(b))
          val (fft, effects3) = if (abs.isTrue(constest)) {
            val (cara, effects4) = car(a, store)
            val (carb, effects5) = car(b, store)
            val (cartest, effects6) = equal(cara, carb, store, visited2)
            val (fftt, effects7) = if (abs.isTrue(cartest)) {
              val (cdra, effects8) = cdr(a, store)
              val (cdrb, effects9) = cdr(b, store)
              val (res, effects10) = equal(cdra, cdrb, store, visited2)
              (res, effects8 ++ effects9 ++ effects10)
            } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
            val fftf = if (abs.isFalse(cartest)) { abs.inject(false) } else { abs.bottom }
            (abs.join(fftt, fftf), effects4 ++ effects5 ++ effects6 ++ effects7)
          } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
          val (fff, effects11) = if (abs.isFalse(constest)) {
            val vectest = abs.and(isVector(a), isVector(b))
            val (ffft, effects12) = if (abs.isTrue(vectest)) {
              val (lengtha, effects13) = vectorLength(a, store)
              val (lengthb, effects14) = vectorLength(b, store)
              val lengthtest = numEq(lengtha, lengthb)
              val (ffftt, effects15) = if (abs.isTrue(lengthtest)) {
                equalVecLoop(a, b, abs.inject(0), lengtha, store, visited2, Set())
              } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
              val ffftf = if (abs.isFalse(lengthtest)) { abs.inject(false) } else { abs.bottom }
              (abs.join(ffftt, ffftf), effects13 ++ effects14 ++ effects15)
            } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
            val ffff = if (abs.isFalse(vectest)) { abs.inject(false) } else { abs.bottom }
            (abs.join(ffft, ffff), effects12)
          } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
          (abs.join(fft, fff), effects11)
        } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
        (abs.join(ft, ff), effects2)
      } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
      (abs.join(t, f), effects1)
    }
  }
  private def equal(a: Abs, b: Abs, store: Store[Addr, Abs]): (Abs, Set[Effect[Addr, Abs]]) = equal(a, b, store, Set())

  /** (define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l))) */
  private def listp(l: Abs, store: Store[Addr, Abs], visited: Set[Abs]): (Abs, Set[Effect[Addr, Abs]]) =
    if (visited.contains(l)) {
      /* R5RS: "all lists have finite length", and the cases where this is reached include circular lists */
      (abs.inject(false), Set[Effect[Addr, Abs]]())
    } else {
      val nulltest = isNull(l)
      val t = if (abs.isTrue(nulltest)) { nulltest } else { abs.bottom }
      val (f, effs) = if (abs.isFalse(nulltest)) {
        val constest = isCons(l)
        val (ft, effs) = if (abs.isTrue(constest)) {
          val (cdrl, effects1) = cdr(l, store)
          val (res, effects2) = listp(cdrl, store, visited + l)
          (res, effects1 ++ effects2)
        } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
        val ff = if (abs.isFalse(constest)) { abs.inject(false) } else { abs.bottom }
        (abs.join(ft, ff), effs)
      } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
      (abs.join(t, f), effs)
    }
  private def listp(l: Abs, store: Store[Addr, Abs]): (Abs, Set[Effect[Addr, Abs]]) = listp(l, store, Set())

  /** (define (length l) (if (pair? l) (+ 1 (length (cdr l))) (if (null? l) 0 (error "length called with a non-list")))) */
  private def length(l: Abs, store: Store[Addr, Abs], visited: Set[Abs]): (Abs, Set[Effect[Addr, Abs]]) =
    if (visited.contains(l)) {
      (abs.bottom, Set[Effect[Addr, Abs]]())
    } else {
      val visited2 = visited + l
      val cond = isCons(l)
      val (t, eff) = if (abs.isTrue(cond)) {
        val (cdrl, effects1) = cdr(l, store)
        val (lengthl, effects2) = length(cdrl, store, visited2)
        (plus(abs.inject(1), lengthl), effects1 ++ effects2)
      } else { (abs.bottom, Set[Effect[Addr, Abs]]()) }
      val f = if (abs.isFalse(cond)) {
        val fcond = isNull(l)
        val ft = if (abs.isTrue(fcond)) { abs.inject(0) } else { abs.bottom }
        val ff = if (abs.isFalse(fcond)) { abs.error(abs.inject("length called with a non-list")) } else { abs.bottom }
        abs.join(ft, ff)
      } else {
        abs.bottom
      }
      (abs.join(t, f), eff)
    }
  private def length(l: Abs, store: Store[Addr, Abs]): (Abs, Set[Effect[Addr, Abs]]) = length(l, store, Set())

  private def toPrim(x: (Abs, Set[Effect[Addr, Abs]]), store: Store[Addr, Abs]): (Abs, Store[Addr, Abs], Set[Effect[Addr, Abs]]) =
    (x._1, store, x._2)
  private def chain(v: Abs, fs: (Abs => (Abs, Set[Effect[Addr, Abs]]))*): (Abs, Set[Effect[Addr, Abs]]) =
    fs.foldLeft((v, Set[Effect[Addr, Abs]]()))((acc, f) => {
      val (res, effects) = f(acc._1)
      (res, acc._2 ++ effects)
    })


  /** Bundles all the primitives together */
  val all: List[Primitive[Addr, Abs]] = List(
    Plus, Minus, Times, Div, Max, Min,
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
    UnaryStoreOperation("display", (v, store) => {
      val str = abs.shows(v, store)
      print(if (str.startsWith("\"")) { str.substring(1, str.size-1) } else { str })
      toPrim((v, Set()), store)
    }),
    NullaryOperation("newline", newline),
    Cons,
    UnaryStoreOperation("car", (v, store) => toPrim(car(v, store), store)),
    UnaryStoreOperation("cdr", (v, store) => toPrim(cdr(v, store), store)),
    UnaryStoreOperation("caar", (v, store) => toPrim(chain(v, car(_, store), car(_, store)), store)),
    UnaryStoreOperation("cadr", (v, store) => toPrim(chain(v, cdr(_, store), car(_, store)), store)),
    UnaryStoreOperation("cddr", (v, store) => toPrim(chain(v, cdr(_, store), cdr(_, store)), store)),
    UnaryStoreOperation("cdar", (v, store) => toPrim(chain(v, car(_, store), cdr(_, store)), store)),
    UnaryStoreOperation("caaar", (v, store) => toPrim(chain(v, car(_, store), car(_, store), car(_, store)), store)),
    UnaryStoreOperation("cdaar", (v, store) => toPrim(chain(v, car(_, store), car(_, store), cdr(_, store)), store)),
    UnaryStoreOperation("caadr", (v, store) => toPrim(chain(v, cdr(_, store), car(_, store), car(_, store)), store)),
    UnaryStoreOperation("cdadr", (v, store) => toPrim(chain(v, cdr(_, store), car(_, store), cdr(_, store)), store)),
    UnaryStoreOperation("caddr", (v, store) => toPrim(chain(v, cdr(_, store), cdr(_, store), car(_, store)), store)),
    UnaryStoreOperation("cdddr", (v, store) => toPrim(chain(v, cdr(_, store), cdr(_, store), cdr(_, store)), store)),
    UnaryStoreOperation("cadar", (v, store) => toPrim(chain(v, car(_, store), cdr(_, store), car(_, store)), store)),
    UnaryStoreOperation("cddar", (v, store) => toPrim(chain(v, car(_, store), cdr(_, store), cdr(_, store)), store)),
    BinaryStoreOperation("set-car!", (cell, v, store) => {
      val (store2, effects) = abs.car(cell).foldLeft((store, Set[Effect[Addr, Abs]]()))((acc, a) =>
        (acc._1.update(a, v), acc._2 + EffectWriteConsCar(a)))
      (abs.inject(false), store2, effects)
    }),
    BinaryStoreOperation("set-cdr!", (cell, v, store) => {
      val (store2, effects) = abs.cdr(cell).foldLeft((store, Set[Effect[Addr, Abs]]()))((acc, a) =>
        (acc._1.update(a, v), acc._2 + EffectWriteConsCar(a)))
      (abs.inject(false), store2, effects)
    }),
    UnaryOperation("error", abs.error),
    UnaryOperation("null?", isNull),
    UnaryOperation("pair?", isCons),
    UnaryStoreOperation("list?", (v, store) => {
      val (res, effs) = listp(v, store)
      (res, store, effs)
    }),
    UnaryOperation("char?", isChar),
    UnaryOperation("symbol?", isSymbol),
    UnaryOperation("string?", isString),
    UnaryOperation("integer?", isInteger),
    UnaryOperation("number?", x => abs.or(isInteger(x), isFloat(x))), /* TODO: complex numbers */
    UnaryOperation("real?", x => abs.or(isInteger(x), isFloat(x))),
    UnaryOperation("boolean?", isBoolean),
    BinaryOperation("eq?", eq),
    BinaryStoreOperation("equal?", (a, b, store) => {
      val (res, eff) = equal(a, b, store)
      (res, store, eff)
    }),
    UnaryStoreOperation("length", (v, store) => {
      val (res, eff) = length(v, store)
      (res, store, eff)
    }),
    UnaryOperation("abs", abs),
    MakeVector, VectorSet, Vector,
    UnaryOperation("vector?", isVector),
    UnaryStoreOperation("vector-length", (v, store) => {
      val (res, eff) = vectorLength(v, store)
      (res, store, eff)
    }),
    BinaryStoreOperation("vector-ref", (v, i, store) => {
      val (res, effects) = vectorRef(v, i, store)
      (res, store, effects)
    }),
    Lock
  )

  private val allocated = ("bottom", addr.primitive("__bottom__"), abs.bottom) :: all.map({ prim => (prim.name, addr.primitive(prim.name), abs.inject(prim)) })
  val forEnv: List[(String, Addr)] = allocated.map({ case (name, a, _) => (name, a) })
  val forStore: List[(Addr, Abs)] =  allocated.map({ case (_, a, v) => (a, v) })
}
