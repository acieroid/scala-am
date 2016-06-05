import scalaz.{Plus => _, _}
import scalaz.Scalaz._

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
  def call[Exp : Expression, Time : Timestamp](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])]
}

abstract class Primitives[Addr : Address, Abs : JoinLattice] {
  def all: List[Primitive[Addr, Abs]]
  def toVal(prim: Primitive[Addr, Abs]): Abs

  val lat = implicitly[JoinLattice[Abs]]
  val addr = implicitly[Address[Addr]]

  /** Modify a primitive to trace it: output will be printed when the primitive is
    * called. This is for debugging purposes. */
  def traced(prim: Primitive[Addr, Abs]): Primitive[Addr, Abs] = new Primitive[Addr, Abs] {
    val name = prim.name
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = {
      val argsstr = args.map({ case (_, v) => v.toString }).mkString(" ")
      val res = prim.call(fexp, args, store, t)
      print("($name $argsstr) -> ")
      res match {
        case MayFailError(errs) => "ERROR: " + errs.mkString(" | ERROR: ")
        case MayFailSuccess((v, store, effs)) => v.toString
        case MayFailBoth((v, store, effs), errs) => v.toString + " | ERROR: " + errs.mkString(" | ERROR: ")
      }
      res
    }
  }

  lazy val bindings = ("bottom", addr.primitive("__bottom__"), lat.bottom) :: all.map({ prim => (prim.name, addr.primitive(prim.name), toVal(prim)) })
}

/** This is where we define Scheme primitives */
class SchemePrimitives[Addr : Address, Abs : AbstractValue] extends Primitives[Addr, Abs] {
  import SchemeOps._
  val abs = implicitly[AbstractValue[Abs]]

  def isNull = abs.unaryOp(SchemeOps.IsNull) _
  def isCons = abs.unaryOp(SchemeOps.IsCons) _
  def isChar = abs.unaryOp(SchemeOps.IsChar) _
  def isSymbol = abs.unaryOp(SchemeOps.IsSymbol) _
  def isString = abs.unaryOp(SchemeOps.IsString) _
  def isInteger = abs.unaryOp(SchemeOps.IsInteger) _
  def isFloat = abs.unaryOp(SchemeOps.IsFloat) _
  def isBoolean = abs.unaryOp(SchemeOps.IsBoolean) _
  def isVector = abs.unaryOp(SchemeOps.IsVector) _
  def ceiling = abs.unaryOp(SchemeOps.Ceiling) _
  def log = abs.unaryOp(SchemeOps.Log) _
  def not = abs.unaryOp(SchemeOps.Not) _
  def random = abs.unaryOp(SchemeOps.Random) _
  def plus = abs.binaryOp(SchemeOps.Plus) _
  def minus = abs.binaryOp(SchemeOps.Minus) _
  def times = abs.binaryOp(SchemeOps.Times) _
  def div = abs.binaryOp(SchemeOps.Div) _
  def modulo = abs.binaryOp(SchemeOps.Modulo) _
  def lt = abs.binaryOp(SchemeOps.Lt) _
  def numEq = abs.binaryOp(SchemeOps.NumEq) _
  def eq = abs.binaryOp(SchemeOps.Eq) _
  def vectorLength = abs.unaryOp(SchemeOps.VectorLength) _
  def stringAppend = abs.binaryOp(SchemeOps.StringAppend) _
  def stringLength = abs.unaryOp(SchemeOps.StringLength) _
  def numberToString = abs.unaryOp(SchemeOps.NumberToString) _

  abstract class NoStoreOperation(val name: String, val nargs: Option[Int] = None) extends Primitive[Addr, Abs] {
    def call(args: List[Abs]): MayFail[Abs] = MayFailError(List(ArityError(name, nargs.getOrElse(-1), args.length)))
    def call(arg1: Abs, arg2: Abs): MayFail[Abs] = call(List(arg1, arg2))
    def call[Exp : Expression](arg1: (Exp, Abs), arg2: (Exp, Abs)): MayFail[Abs] = call(arg1._2, arg2._2)
    def call[Exp : Expression](fexp: Exp, arg1: (Exp, Abs), arg2: (Exp, Abs)): MayFail[Abs] = call(arg1, arg2)
    def call(arg: Abs): MayFail[Abs] = call(List(arg))
    def call[Exp : Expression](arg: (Exp, Abs)): MayFail[Abs] = call(arg._2)
    def call[Exp : Expression](fexp: Exp, arg: (Exp, Abs)): MayFail[Abs] = call(arg)
    def call(): MayFail[Abs] = call(List())
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] = (args match {
      case Nil => call()
      case x :: Nil => call(fexp, x)
      case x :: y :: Nil => call(fexp, x, y)
      case l => call(args.map({ case (_, v) => v }))
    }).map(v => (v, store, Set()))
  }

  abstract class StoreOperation(val name: String, val nargs: Option[Int] = None) extends Primitive[Addr, Abs] {
    def call(args: List[Abs], store: Store[Addr, Abs]): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] = MayFailError(List(ArityError(name, nargs.getOrElse(-1), args.length)))
    def call(arg: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] =
      call(List(arg), store)
    def call(arg1: Abs, arg2: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] =
      call(List(arg1, arg2), store)
    def call[Exp : Expression](fexp: Exp, arg1: (Exp, Abs), arg2: (Exp, Abs), store: Store[Addr, Abs]): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] =
      call(arg1._2, arg2._2, store)
    def call[Exp : Expression](fexp: Exp, arg: (Exp, Abs), store: Store[Addr, Abs]): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] =
      call(arg._2, store)
    def call(store: Store[Addr, Abs]): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] = call(List(), store)
    def call[Exp : Expression, Time : Timestamp](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] = args match {
      case Nil => call(store)
      case x :: Nil => call(fexp, x, store)
      case x :: y :: Nil => call(fexp, x, y, store)
      case l => call(args.map({ case (_, v) => v }), store)
    }
  }

  object Plus extends NoStoreOperation("+") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailSuccess(abs.inject(0))
      case x :: rest => call(rest).bind(y => plus(x, y))
    }
  }
  object Minus extends NoStoreOperation("-") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailError(List(VariadicArityError(name, 1, 0)))
      case x :: Nil => minus(abs.inject(0), x)
      case x :: rest => Plus.call(rest).bind(y => minus(x, y))
    }
  }
  object Times extends NoStoreOperation("*") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailSuccess(abs.inject(1))
      case x :: rest => call(rest).bind(y => times(x, y))
    }
  }
  object Div extends NoStoreOperation("/") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailError(List(VariadicArityError(name, 1, 0)))
      case x :: rest => Times.call(rest).bind(y => div(x, y))
    }
  }
  object Quotient extends NoStoreOperation("quotient", Some(2)) {
    override def call(x: Abs, y: Abs) = div(x, y)
  }
  object LessThan extends NoStoreOperation("<", Some(2)) {
    override def call(x: Abs, y: Abs) = lt(x, y) /* TODO: < should accept any number of arguments (same for <= etc.) */
  }
  object LessOrEqual extends NoStoreOperation("<=", Some(2)) {
    override def call(x: Abs, y: Abs) = lt(x, y).bind(ltres => numEq(x, y).map(eqres => abs.or(ltres, eqres)))
  }
  object NumEq extends NoStoreOperation("=", Some(2)) {
    override def call(x: Abs, y: Abs) = numEq(x, y)
  }
  object GreaterThan extends NoStoreOperation(">", Some(2)) {
    override def call(x: Abs, y: Abs) = LessOrEqual.call(x, y).bind(leres => not(leres))
  }
  object GreaterOrEqual extends NoStoreOperation(">=", Some(2)) {
    override def call(x: Abs, y: Abs) = LessThan.call(x, y).bind(ltres => not(ltres))
  }
  object Modulo extends NoStoreOperation("modulo", Some(2)) {
    override def call(x: Abs, y: Abs) = modulo(x, y)
  }
  object Random extends NoStoreOperation("random", Some(1)) {
    override def call(x: Abs) = random(x)
  }
  object Ceiling extends NoStoreOperation("ceiling", Some(1)) {
    override def call(x: Abs) = ceiling(x)
  }
  object Log extends NoStoreOperation("log", Some(1)) {
    override def call(x: Abs) = log(x)
  }
  /** (define (zero? x) (= x 0)) */
  object Zerop extends NoStoreOperation("zero?", Some(1)) {
    override def call(x: Abs) = numEq(abs.inject(0), x)
  }
  /** (define (positive? x) (< x 0)) */
  object Positivep extends NoStoreOperation("positive?", Some(1)) {
    override def call(x: Abs) = lt(abs.inject(0), x)
  }
  /** (define (positive? x) (< 0 x)) */
  object Negativep extends NoStoreOperation("negative?", Some(1)) {
    override def call(x: Abs) = lt(x, abs.inject(0))
  }
  /** (define (odd? x) (= 1 (modulo x 2))) */
  object Oddp extends NoStoreOperation("odd?", Some(1)) {
    override def call(x: Abs) = modulo(x, abs.inject(2)).bind(mod => numEq(abs.inject(1), mod))
  }
  /** (define (even? x) (= 0 (modulo x 2))) */
  object Evenp extends NoStoreOperation("even?", Some(1)) {
    override def call(x: Abs) = modulo(x, abs.inject(2)).bind(mod => numEq(abs.inject(0), mod))
  }
  object Max extends NoStoreOperation("max") {
    /* TODO: In Scheme, max casts numbers to inexact as soon as one of them is inexact, but we don't support that */
    private def call(args: List[Abs], max: Abs): MayFail[Abs] = args match {
      case Nil => MayFailSuccess(max)
      case x :: rest => lt(max, x).bind(test => {
        val t = if (abs.isTrue(test)) { call(rest, x) } else { MayFailSuccess(abs.bottom) }
        val f = if (abs.isFalse(test)) { call(rest, max) } else { MayFailSuccess(abs.bottom) }
        MayFail.monoid[Abs].append(t, f)
      })
    }
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailError(List(VariadicArityError(name, 1, 0)))
      case x :: rest => call(rest, x)
    }
  }
  object Min extends NoStoreOperation("min") {
    /* TODO: same remark as max */
    private def call(args: List[Abs], min: Abs): MayFail[Abs] = args match {
      case Nil => MayFailSuccess(min)
      case x :: rest => lt(x, min).bind(test => {
        val t = if (abs.isTrue(test)) { call(rest, x) } else { MayFailSuccess(abs.bottom) }
        val f = if (abs.isFalse(test)) { call(rest, min) } else { MayFailSuccess(abs.bottom) }
        MayFail.monoid[Abs].append(t, f)
      })
    }
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailError(List(VariadicArityError(name, 1, 0)))
      case x :: rest => call(rest, x)
    }
  }
  /** (define (abs x) (if (< x 0) (- 0 x) x)) */
  object Abs extends NoStoreOperation("abs", Some(1)) {
    override def call(x: Abs) = lt(x, abs.inject(0)).bind(ltres => {
      val t = if (abs.isTrue(ltres)) { minus(abs.inject(0), x) } else { MayFailSuccess(abs.bottom) }
      val f = if (abs.isFalse(ltres)) { MayFailSuccess(x) } else { MayFailSuccess(abs.bottom) }
      MayFail.monoid[Abs].append(t, f)
    })
  }
  /** (define (gcd a b) (if (= b 0) a (gcd b (modulo a b)))) */
  object Gcd extends NoStoreOperation("gcd", Some(2)) {
    private def gcd(a: Abs, b: Abs, visited: Set[(Abs, Abs)]): MayFail[Abs] = {
      if (visited.contains((a, b))) {
        MayFailSuccess(abs.bottom)
      } else {
        numEq(b, abs.inject(0)).bind(cond => {
          val t = if (abs.isTrue(cond)) { MayFailSuccess(a) } else { MayFailSuccess(abs.bottom) }
          val f = if (abs.isFalse(cond)) {
            modulo(a, b).bind(amodb => gcd(b, amodb, visited + ((a, b))))
          } else {
            MayFailSuccess(abs.bottom)
          }
          MayFail.monoid[Abs].append(t, f)
        })
      }
    }
    override def call(x: Abs, y: Abs) = gcd(x, y, Set())
  }

  object Nullp extends NoStoreOperation("null?", Some(1)) {
    override def call(x: Abs) = isNull(x)
  }
  object Pairp extends NoStoreOperation("pair?", Some(1)) {
    override def call(x: Abs) = isCons(x)
  }
  object Charp extends NoStoreOperation("char?", Some(1)) {
    override def call(x: Abs) = isChar(x)
  }
  object Symbolp extends NoStoreOperation("symbol?", Some(1)) {
    override def call(x: Abs) = isSymbol(x)
  }
  object Stringp extends NoStoreOperation("string?", Some(1)) {
    override def call(x: Abs) = isString(x)
  }
  object Integerp extends NoStoreOperation("integer?", Some(1)) {
    override def call(x: Abs) = isInteger(x)
  }
  object Realp extends NoStoreOperation("real?", Some(1)) {
    override def call(x: Abs) = isInteger(x).bind(isint => isFloat(x).map(isfloat => abs.or(isint, isfloat)))
  }
  object Numberp extends NoStoreOperation("number?", Some(1)) {
    override def call(x: Abs) = Realp.call(x) /* No support for complex number, so number? is equivalent as real? */
  }
  object Booleanp extends NoStoreOperation("boolean?", Some(1)) {
    override def call(x: Abs) = isBoolean(x)
  }
  object Vectorp extends NoStoreOperation("vector?", Some(1)) {
    override def call(x: Abs) = isVector(x)
  }
  object Eq extends NoStoreOperation("eq?", Some(2)) {
    override def call(x: Abs, y: Abs) = abs.binaryOp(SchemeOps.Eq)(x, y)
  }
  object Not extends NoStoreOperation("not", Some(1)) {
    override def call(x: Abs) = not(x)
  }
  object NumberToString extends NoStoreOperation("number->string", Some(1)) {
    override def call(x: Abs) = numberToString(x)
  }
  object StringAppend extends NoStoreOperation("string-append") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailSuccess(abs.inject(""))
      case x :: rest => call(rest).bind(y => stringAppend(x, y))
    }
  }
  object StringLength extends NoStoreOperation("string-length", Some(1)) {
    override def call(x: Abs) = stringLength(x)
  }
  object Newline extends NoStoreOperation("newline", Some(0)) {
    override def call() = { println(""); MayFailSuccess(abs.inject(false)) }
  }
  object Display extends NoStoreOperation("display", Some(1)) {
    override def call(x: Abs) = {
      val str = x.toString
      print(if (str.startsWith("\"")) { str.substring(1, str.size-1) } else { str })
      MayFailSuccess(x) /* Undefined behavior in R5RS */
    }
  }
  object Error extends NoStoreOperation("error", Some(1)) {
    override def call[Exp : Expression](fexp: Exp, x: (Exp, Abs)) =
      MayFailError(List(UserError(x._2.toString, implicitly[Expression[Exp]].pos(fexp))))
  }

  val mfmon = MayFail.monoid[(Abs, Set[Effect[Addr]])]
  def err(e: SchemeError): MayFail[(Abs, Set[Effect[Addr]])] = MayFailError(List(e))
  def success(v: Abs): MayFail[(Abs, Set[Effect[Addr]])] = MayFailSuccess((v, Set()))
  object Cons extends Primitive[Addr, Abs] {
    val name = "cons"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (carexp, car) :: (cdrexp, cdr) :: Nil => {
        val cara = addr.cell(carexp, t)
        val cdra = addr.cell(cdrexp, t)
        MayFailSuccess((abs.cons(cara, cdra), store.extend(cara, car).extend(cdra, cdr), Set()))
      }
      case l => MayFailError(List(ArityError(name, 2, l.size)))
    }
  }
  private def car(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
    val addrs = abs.car(v)
    if (addrs.isEmpty) {
      err(CannotAccessCar(v.toString))
    } else {
      addrs.foldLeft(mfmon.zero)((acc, a) =>
        mfmon.append(acc, store.lookup(a) match {
          case Some(v) => MayFailSuccess((v, Set(EffectReadConsCar(a))))
          case None => err(UnboundAddress(a.toString))
        }))
    }
  }
  private def cdr(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
    val addrs = abs.cdr(v)
    if (addrs.isEmpty) {
      err(CannotAccessCdr(v.toString))
    } else {
      addrs.foldLeft(mfmon.zero)((acc, a) =>
        MayFail.monoid[(Abs, Set[Effect[Addr]])].append(acc, store.lookup(a) match {
          case Some(v) => MayFailSuccess((v, Set(EffectReadConsCdr(a))))
          case None => err(UnboundAddress(a.toString))
        }))
    }
  }

  class CarCdrOperation(override val name: String) extends StoreOperation(name, Some(1)) {
    trait Spec
    case object Car extends Spec
    case object Cdr extends Spec
    val spec: List[Spec] = name.drop(1).take(name.length - 2).toList.reverse.map(c =>
      if (c == 'a') { Car }
      else if (c == 'd') { Cdr }
      else { throw new Exception("Incorrect car/cdr operation: $name") })
    override def call(v: Abs, store: Store[Addr, Abs]) =
      spec.foldLeft(success(v))((acc, op) =>
        acc.bind({ case (v, effs) => op match {
          case Car => car(v, store)
          case Cdr => cdr(v, store)
        }})).map({ case (v, effs) => (v, store, effs) })
  }
  object Car extends CarCdrOperation("car")
  object Cdr extends CarCdrOperation("cdr")
  object Caar extends CarCdrOperation("caar")
  object Cadr extends CarCdrOperation("cadr")
  object Cdar extends CarCdrOperation("cdar")
  object Cddr extends CarCdrOperation("cddr")
  object Caaar extends CarCdrOperation("caaar")
  object Caadr extends CarCdrOperation("caadr")
  object Cadar extends CarCdrOperation("cadar")
  object Caddr extends CarCdrOperation("caddr")
  object Cdaar extends CarCdrOperation("cdaar")
  object Cdadr extends CarCdrOperation("cdadr")
  object Cddar extends CarCdrOperation("cddar")
  object Cdddr extends CarCdrOperation("cdddr")
  object Caaaar extends CarCdrOperation("caaaar")
  object Caaadr extends CarCdrOperation("caaadr")
  object Caadar extends CarCdrOperation("caadar")
  object Caaddr extends CarCdrOperation("caaddr")
  object Cadaar extends CarCdrOperation("cadaar")
  object Cadadr extends CarCdrOperation("cadadr")
  object Caddar extends CarCdrOperation("caddar")
  object Cadddr extends CarCdrOperation("cadddr")
  object Cdaaar extends CarCdrOperation("cdaaar")
  object Cdaadr extends CarCdrOperation("cdaadr")
  object Cdadar extends CarCdrOperation("cdadar")
  object Cdaddr extends CarCdrOperation("cdaddr")
  object Cddaar extends CarCdrOperation("cddaar")
  object Cddadr extends CarCdrOperation("cddadr")
  object Cdddar extends CarCdrOperation("cdddar")
  object Cddddr extends CarCdrOperation("cddddr")

  object SetCar extends StoreOperation("set-car!", Some(2)) {
    override def call(cell: Abs, value: Abs, store: Store[Addr, Abs]) = {
      val addrs = abs.car(cell)
      if (addrs.isEmpty) {
         MayFailError(List(CannotAccessCar(cell.toString)))
      } else {
        val (store2, effects) = addrs.foldLeft((store, Set[Effect[Addr]]()))((acc, a) =>
          (acc._1.update(a, value), acc._2 + EffectWriteConsCar(a)))
        MayFailSuccess((abs.inject(false) /* undefined */, store2, effects))
      }
    }
  }
  object SetCdr extends StoreOperation("set-cdr!", Some(2)) {
    override def call(cell: Abs, value: Abs, store: Store[Addr, Abs]) = {
      val addrs = abs.cdr(cell)
      if (addrs.isEmpty) {
        MayFailError(List(CannotAccessCdr(cell.toString)))
      } else {
        val (store2, effects) = addrs.foldLeft((store, Set[Effect[Addr]]()))((acc, a) =>
          (acc._1.update(a, value), acc._2 + EffectWriteConsCdr(a)))
        MayFailSuccess((abs.inject(false) /* undefined */, store2, effects))
      }
    }
  }
  object Length extends StoreOperation("length", Some(1)) {
    override def call(l: Abs, store: Store[Addr, Abs]) = {
      def length(l: Abs, visited: Set[Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
        if (visited.contains(l)) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          isCons(l).bind(cond => {
            val t = if (abs.isTrue(cond)) {
              cdr(l, store).bind({ case (cdrl, effects1) =>
                length(cdrl, visited + l).bind( { case (lengthcdrl, effects2) =>
                  plus(abs.inject(1), lengthcdrl).map(lengthl =>
                    (lengthl, effects1 ++ effects2))
                })
              })
            } else { mfmon.zero }
            val f = if (abs.isFalse(cond)) {
              isNull(l).bind(fcond => {
              val ft = if (abs.isTrue(fcond)) { MayFailSuccess((abs.inject(0), Set[Effect[Addr]]())) } else { mfmon.zero }
                val ff = if (abs.isFalse(fcond)) { err(TypeError("length", "first operand", "list", "non-list")) } else { mfmon.zero }
                mfmon.append(ft, ff)
              })
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            mfmon.append(t, f)
          })
        }
      }
      length(l, Set()).map({ case (v, effs) => (v, store, effs) })
    }
  }
  /** (define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l))) */
  object Listp extends StoreOperation("list?", Some(1)) {
    override def call(l: Abs, store: Store[Addr, Abs]) = {
      def listp(l: Abs, visited: Set[Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
        if (visited.contains(l)) {
          /* R5RS: "all lists have finite length", and the cases where this is reached include circular lists */
          MayFailSuccess(abs.inject(false), Set[Effect[Addr]]())
        } else {
          isNull(l).bind(nulltest => {
            val t = if (abs.isTrue(nulltest)) { MayFailSuccess((nulltest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(nulltest)) {
              isCons(l).bind(constest => {
                val ft = if (abs.isTrue(constest)) {
                  cdr(l, store).bind({ case (cdrl, effects1) =>
                    listp(cdrl, visited + l).map({ case (listpl, effects2) =>
                      (listpl, effects1 ++ effects2)
                    })
                  })
                } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                val ff = if (abs.isFalse(constest)) {
                  MayFailSuccess((abs.inject(false), Set[Effect[Addr]]()))
                } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              })
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
          })
        }
      }
      listp(l, Set()).map({ case (v, effs) => (v, store, effs) })
    }
  }

  object MakeVector extends Primitive[Addr, Abs] {
    val name = "make-vector"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, size) :: (initexp, init) :: Nil =>
        isInteger(size).bind(isint =>
          if (abs.isTrue(isint)) {
            val a = addr.cell(fexp, t)
            val initaddr = addr.cell(initexp, t)
            abs.vector(a, size, initaddr).map({ case (va, vector) =>
              (va, store.extend(a, vector).extend(initaddr, init), Set())})
          } else {
            MayFailError(List(TypeError("make-vector", "first operand", "integer", size.toString)))
          })
      case l => MayFailError(List(ArityError(name, 2, l.size)))
    }
  }
  object VectorSet extends Primitive[Addr, Abs] {
    val name = "vector-set!"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (_, vector) :: (_, index) :: (exp, value) :: Nil => {
        val addrs = abs.getVectors(vector)
        if (addrs.isEmpty) {
          MayFailError(List(CannotAccessVector(vector.toString)))
        } else {
          val init: MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] = MayFailSuccess((abs.bottom, store, Set[Effect[Addr]]()))
          addrs.foldLeft(init)((acc, va) => {
            store.lookup(va) match {
              case Some(oldvec) => {
                acc.bind({ case (oldval, store, effects) =>
                  val targetaddr = addr.cell(exp, t)
                  abs.vectorSet(oldvec, index, targetaddr).map({ case (vec, addrs) =>
                    val store2 = addrs.foldLeft(store.update(va, vec))((st, a) => st.updateOrExtend(a, value))
                    val effects2 = addrs.map(a => EffectWriteVector(a))
                    (abs.join(oldval, vec), store2, effects ++ effects2)
                  })
                })
              }
              case None => acc.addError(UnboundAddress(va.toString))
            }
          })
        }
      }
      case l => MayFailError(List((ArityError(name, 3, l.size))))
    }
  }
  object Vector extends Primitive[Addr, Abs] {
    val name = "vector"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = {
      val a = addr.cell(fexp, t)
      val botaddr = addr.primitive("__bottom__")
      abs.vector(a, abs.inject(args.size), botaddr).bind({ case (va, emptyVector) =>
        /* No tracked effects because we only perform atomic updates at allocation time */
        val init: MayFail[(Abs, Store[Addr, Abs])] = MayFailSuccess((emptyVector, store))
        args.zipWithIndex.foldLeft(init)((acc, arg) => acc.bind({ case (vec, store) =>
          arg match {
            case ((exp, value), index) =>
              val valaddr = addr.cell(exp, t)
              abs.vectorSet(vec, abs.inject(index), valaddr).map({
                case (vec, addrs) => (vec, addrs.foldLeft(store)((st, a) => st.updateOrExtend(a, value)))
              })
          }})).map({ case (vector, store) => (va, store.extend(a, vector), Set[Effect[Addr]]()) })
      })
    }
  }
  object VectorLength extends StoreOperation("vector-length", Some(1)) {
    def length(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      val addrs = abs.getVectors(v)
      if (addrs.isEmpty) {
        MayFailError(List(CannotAccessVector(v.toString)))
      } else {
        addrs.foldLeft(mfmon.zero)((acc, va) =>
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(acc, /* TODO: that's basically a foldMap over a monoid... */
            store.lookup(va) match {
              case Some(v) => vectorLength(v).map(vl => (vl, Set(EffectReadVariable(va))))
              case None => MayFailError(List(UnboundAddress(va.toString)))
            }))
      }
    }
    override def call(v: Abs, store: Store[Addr, Abs]) = {
      length(v, store).map({ case (v, effs) => (v, store, effs) })
    }
  }
  object VectorRef extends StoreOperation("vector-ref", Some(2)) {
    def vectorRef(v: Abs, index: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      val addrs = abs.getVectors(v)
      if (addrs.isEmpty) {
        MayFailError(List(CannotAccessVector(v.toString)))
      } else {
        addrs.foldLeft(mfmon.zero)((acc, va) =>
          store.lookup(va) match {
            case Some(v) => abs.vectorRef(v, index).bind(vs =>
              vs.foldLeft(acc)((acc, a) =>
                mfmon.append(acc,
                  store.lookup(a) match {
                    case Some(value) => MayFailSuccess((value, Set(EffectReadVector(a))))
                    case None => err(UnboundAddress(a.toString))
                  })))
            case None => acc.addError(UnboundAddress(va.toString))
          })
      }
    }
    override def call(v: Abs, index: Abs, store: Store[Addr, Abs]) =
      vectorRef(v, index, store).map({ case (v, effs) => (v, store, effs) })
  }

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
  object Equal extends StoreOperation("equal?", Some(2)) {
    override def call(a: Abs, b: Abs, store: Store[Addr, Abs]) = {
      def equalVec(a: Abs, b: Abs, i: Abs, n: Abs, visitedEqual: Set[(Abs, Abs)], visited: Set[(Abs, Abs, Abs, Abs)]): MayFail[(Abs, Set[Effect[Addr]])] = {
        println(s"equalVec($a, $b, $i, $n)")
        if (visited.contains((a, b, i, n)) || a == abs.bottom || b == abs.bottom || i == abs.bottom || n == abs.bottom) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          numEq(i, n).bind(numtest => {
            val t = if (abs.isTrue(numtest)) { MayFailSuccess((abs.inject(true), Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(numtest)) {
              VectorRef.vectorRef(a, i, store).bind({ case (vai, effects1) =>
                VectorRef.vectorRef(b, i, store).bind({ case (vbi, effects2) =>
                  equalp(vai, vbi, visitedEqual).bind({ case (itemtest, effects3) => {
                    val tt = if (abs.isTrue(itemtest)) {
                      plus(i, abs.inject(1)).bind(iplus1 =>
                        equalVec(a, b, iplus1, n, visitedEqual, visited + ((a, b, i, n))).map({ case (eqvec, effects4) =>
                        (eqvec, effects1 ++ effects2 ++ effects3 ++ effects4)}))
                    } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                    val tf = if (abs.isFalse(itemtest)) { MayFailSuccess((abs.inject(false), effects1 ++ effects2 ++ effects3)) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                    MayFail.monoid[(Abs, Set[Effect[Addr]])].append(tt, tf)
                }})
              })})
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
          })
        }
      }
      def equalp(a: Abs, b: Abs, visited: Set[(Abs, Abs)]): MayFail[(Abs, Set[Effect[Addr]])] = {
        println(s"equal($a, $b)")
        if (visited.contains((a, b)) || a == abs.bottom || b == abs.bottom) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          val visited2 = visited + ((a, b))
          abs.binaryOp(SchemeOps.Eq)(a, b).bind(eqtest => {
            val t = if (abs.isTrue(eqtest)) { MayFailSuccess((eqtest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(eqtest)) {
              isNull(a).bind(anull => isNull(b).bind(bnull => {
                val nulltest = abs.and(anull, bnull)
                val ft = if (abs.isTrue(nulltest)) { MayFailSuccess((eqtest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                val ff = if (abs.isFalse(nulltest)) {
                  isCons(a).bind(acons => isCons(b).bind(bcons => {
                    val constest = abs.and(acons, bcons)
                    val fft = if (abs.isTrue(constest)) {
                      car(a, store).bind({ case (acar, effects1) =>
                        car(b, store).bind({ case (bcar, effects2) =>
                          equalp(acar, bcar, visited2).bind({ case (cartest, effects3) => {
                            val fftt = if (abs.isTrue(cartest)) {
                              cdr(a, store).bind({ case (acdr, effects4) =>
                                cdr(b, store).bind({ case (bcdr, effects5) =>
                                  equalp(acdr, bcdr, visited2).map({ case (eqtest, effects6) =>
                                    (eqtest, effects1 ++ effects2 ++ effects3 ++ effects4 ++ effects5 ++ effects6)
                                  })
                                })
                              })
                            } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                            val fftf = if (abs.isFalse(cartest)) { MayFailSuccess((abs.inject(true)), effects1 ++ effects2 ++ effects3) } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(fftt, fftf)
                          }})})})} else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                    val fff = if (abs.isFalse(constest)) {
                      isVector(a).bind(avec =>
                        isVector(b).bind(bvec => {
                          val vectest = abs.and(avec, bvec)
                          val ffft = if (abs.isTrue(vectest)) {
                            VectorLength.length(a, store).bind({ case (alength, effects1) =>
                              VectorLength.length(b, store).bind({ case (blength, effects2) =>
                                numEq(alength, blength).bind(lengthtest => {
                                  val ffftt = if (abs.isTrue(lengthtest)) {
                                    equalVec(a, b, abs.inject(0), alength, visited2, Set()).map({ case (eqvec, effects3) =>
                                      (eqvec, effects1 ++ effects2 ++ effects3)})
                                  } else  { MayFailSuccess((abs.bottom, effects1 ++ effects2)) }
                                  val ffftf = if (abs.isFalse(lengthtest)) { MayFailSuccess((abs.inject(false), effects1 ++ effects2)) } else { MayFailSuccess((abs.bottom, effects1 ++ effects2)) }
                                  MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ffftt, ffftf)
                                })
                              })
                            })
                          } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                          val ffff = if (abs.isFalse(vectest)) { MayFailSuccess((abs.inject(false), Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ffft, ffff)
                        }))
                    } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                    MayFail.monoid[(Abs, Set[Effect[Addr]])].append(fft, fff)
                  }))
                } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              }))
            } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
          })
        }
      }
      equalp(a, b, Set()).map({ case (v, effs) => (v, store, effs) })
    }
  }

  object Lock extends Primitive[Addr, Abs] {
    val name = "new-lock"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case Nil =>
        val a = addr.cell(fexp, t)
        MayFailSuccess((abs.lock(a), store.extend(a, abs.unlockedValue), Set()))
      case l => MayFailError(List(ArityError("lock", 0, l.size)))
    }
  }

  /** Bundles all the primitives together */
  def all: List[Primitive[Addr, Abs]] = List(
    Plus, Minus, Times, Div, Quotient, LessThan, LessOrEqual, NumEq, GreaterThan, GreaterOrEqual,
    Modulo, Random, Ceiling, Log, Zerop, Positivep, Negativep, Oddp, Evenp, Max, Min, Abs, Gcd,
    Nullp, Pairp, Charp, Symbolp, Stringp, Integerp, Realp, Numberp, Booleanp, Vectorp, Eq,
    NumberToString, StringAppend, StringLength, Newline, Display, Error, Not,
    Cons, Car, Cdr, Caar, Cadr, Cdar, Cddr, Caaar, Caadr, Cadar, Caddr, Cdaar, Cdadr, Cddar,
    Cdddr, Caaaar, Caaadr, Caadar, Caaddr, Cadaar, Cadadr, Caddar, Cadddr, Cdaaar, Cdaadr, Cdadar,
    Cdaddr, Cddaar, Cddadr, Cdddar, Cddddr, SetCar, SetCdr, Length, Listp,
    MakeVector, VectorSet, Vector, VectorLength, VectorRef,
    Equal,
    Lock)
  def toVal(prim: Primitive[Addr, Abs]): Abs = abs.inject(prim)
}
