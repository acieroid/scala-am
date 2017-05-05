import scalaz._
import scalaz.Scalaz._

/** This is where we define Scheme primitives */
class SchemePrimitives[Addr : Address, Abs : IsSchemeLattice] extends Primitives[Addr, Abs] {
  import SchemeOps._
  val abs = implicitly[IsSchemeLattice[Abs]]

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
  def floor = abs.unaryOp(UnaryOperator.Floor) _
  def round = abs.unaryOp(UnaryOperator.Round) _
  def log = abs.unaryOp(UnaryOperator.Log) _
  def not = abs.unaryOp(UnaryOperator.Not) _
  def random = abs.unaryOp(UnaryOperator.Random) _
  def sin = abs.unaryOp(UnaryOperator.Sin) _
  def asin = abs.unaryOp(UnaryOperator.ASin) _
  def cos = abs.unaryOp(UnaryOperator.Cos) _
  def acos = abs.unaryOp(UnaryOperator.ACos) _
  def tan = abs.unaryOp(UnaryOperator.Tan) _
  def atan = abs.unaryOp(UnaryOperator.ATan) _
  def sqrt = abs.unaryOp(UnaryOperator.Sqrt) _
  def vectorLength = abs.unaryOp(UnaryOperator.VectorLength) _
  def stringLength = abs.unaryOp(UnaryOperator.StringLength) _
  def numberToString = abs.unaryOp(UnaryOperator.NumberToString) _
  def symbolToString = abs.unaryOp(UnaryOperator.SymbolToString) _
  def inexactToExact = abs.unaryOp(UnaryOperator.InexactToExact) _
  def exactToInexact = abs.unaryOp(UnaryOperator.ExactToInexact) _

  def plus = abs.binaryOp(BinaryOperator.Plus) _
  def minus = abs.binaryOp(BinaryOperator.Minus) _
  def times = abs.binaryOp(BinaryOperator.Times) _
  def div = abs.binaryOp(BinaryOperator.Div) _
  def quotient = abs.binaryOp(BinaryOperator.Quotient) _
  def modulo = abs.binaryOp(BinaryOperator.Modulo) _
  def remainder = abs.binaryOp(BinaryOperator.Remainder) _
  def lt = abs.binaryOp(BinaryOperator.Lt) _
  def numEq = abs.binaryOp(BinaryOperator.NumEq) _
  def eqq = abs.binaryOp(BinaryOperator.Eq) _
  def stringAppend = abs.binaryOp(BinaryOperator.StringAppend) _
  def stringLt = abs.binaryOp(BinaryOperator.StringLt) _

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

  object BoolTop extends NoStoreOperation("bool-top") {
    override def call(args: List[Abs]) = abs.boolTop
  }
  object IntTop extends NoStoreOperation("int-top") {
    override def call(args: List[Abs]) = abs.intTop
  }
  object Plus extends NoStoreOperation("+") {
    override def call(args: List[Abs]) = args match {
      case Nil => abs.inject(0)
      case x :: rest => call(rest) >>= (plus(x, _))
    }
  }
  object Minus extends NoStoreOperation("-") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailError(List(VariadicArityError(name, 1, 0)))
      case x :: Nil => minus(abs.inject(0), x)
      case x :: rest => Plus.call(rest) >>= (minus(x, _))
    }
  }
  object Times extends NoStoreOperation("*") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailSuccess(abs.inject(1))
      case x :: rest => call(rest) >>= (times(x, _))
    }
  }
  object Div extends NoStoreOperation("/") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailError(List(VariadicArityError(name, 1, 0)))
      case x :: rest => for {
        multrest <- Times.call(rest)
        r <- div(x, multrest)
        fl <- floor(r)
        isexact <- eqq(r, fl)
        xisint <- isInteger(x)
        multrestisint <- isInteger(multrest)
        convert = abs.and(isexact, abs.and(xisint, multrestisint))
        exr <- inexactToExact(r)
      } yield {
        val t = if (abs.isTrue(convert)) { exr } else { abs.bottom }
        val f = if (abs.isFalse(convert)) { r } else { abs.bottom }
        abs.join(t, f)
      }
    }
  }
  object Quotient extends NoStoreOperation("quotient", Some(2)) {
    override def call(x: Abs, y: Abs) = quotient(x, y)
  }
  object LessThan extends NoStoreOperation("<", Some(2)) {
    override def call(x: Abs, y: Abs) = lt(x, y) /* TODO: < should accept any number of arguments (same for <= etc.) */
  }
  object LessOrEqual extends NoStoreOperation("<=", Some(2)) {
    override def call(x: Abs, y: Abs) = for {
      ltres <- lt(x, y)
      eqres <- numEq(x, y)
    } yield abs.or(ltres, eqres)
  }
  object NumEq extends NoStoreOperation("=", Some(2)) {
    def eq(first: Abs, l: List[Abs]): MayFail[Abs] = l match {
      case Nil => abs.inject(true)
      case x :: rest => numEq(first, x) >>= (feqx => {
        for {
          t <- if (abs.isTrue(feqx)) { eq(first, rest) } else { abs.bottom.point[MayFail] }
          f = if (abs.isFalse(feqx)) { abs.inject(false) } else { abs.bottom }
        } yield abs.join(t, f)
      })
    }
    override def call(args: List[Abs]) = args match {
      case Nil => abs.inject(true)
      case x :: rest => eq(x, rest)
    }
  }
  object GreaterThan extends NoStoreOperation(">", Some(2)) {
    override def call(x: Abs, y: Abs) = LessOrEqual.call(x, y) >>= not
  }
  object GreaterOrEqual extends NoStoreOperation(">=", Some(2)) {
    override def call(x: Abs, y: Abs) = LessThan.call(x, y) >>= not
  }
  object Modulo extends NoStoreOperation("modulo", Some(2)) {
    override def call(x: Abs, y: Abs) = modulo(x, y)
  }
  object Remainder extends NoStoreOperation("remainder", Some(2)) {
    override def call(x: Abs, y: Abs) = remainder(x, y)
  }
  object Random extends NoStoreOperation("random", Some(1)) {
    override def call(x: Abs) = random(x)
  }
  object Ceiling extends NoStoreOperation("ceiling", Some(1)) {
    override def call(x: Abs) = ceiling(x)
  }
  object Floor extends NoStoreOperation("floor", Some(1)) {
    override def call(x: Abs) = floor(x)
  }
  object Round extends NoStoreOperation("round", Some(1)) {
    override def call(x: Abs) = round(x)
  }
  object Log extends NoStoreOperation("log", Some(1)) {
    override def call(x: Abs) = log(x)
  }
  object Sin extends NoStoreOperation("sin", Some(1)) {
    override def call(x: Abs) = sin(x)
  }
  object ASin extends NoStoreOperation("asin", Some(1)) {
    override def call(x: Abs) = asin(x)
  }
  object Cos extends NoStoreOperation("cos", Some(1)) {
    override def call(x: Abs) = cos(x)
  }
  object ACos extends NoStoreOperation("acos", Some(1)) {
    override def call(x: Abs) = acos(x)
  }
  object Tan extends NoStoreOperation("tan", Some(1)) {
    override def call(x: Abs) = tan(x)
  }
  object ATan extends NoStoreOperation("atan", Some(1)) {
    override def call(x: Abs) = atan(x)
  }
  object Sqrt extends NoStoreOperation("sqrt", Some(1)) {
    override def call(x: Abs) = lt(x, abs.inject(0)) >>= { signtest =>
      val t: MayFail[Abs] = if (abs.isFalse(signtest) /* n >= 0 */) {
        for {
          r <- sqrt(x)
          fl <- floor(r)
          argisexact <- isInteger(x)
          resisexact <- eqq(r, fl)
          convert = abs.and(argisexact, resisexact)
          exr <- inexactToExact(r)
        } yield {
          val tt = if (abs.isTrue(convert)) { exr } else { abs.bottom }
          val tf = if (abs.isFalse(convert)) { r } else { abs.bottom }
          abs.join(tt, tf)
        }
      } else { abs.bottom.point[MayFail] }
      val f: MayFail[Abs] = if (abs.isTrue(signtest) /* n < 0 */ ) { MayFailError(List(OperatorNotApplicable("sqrt", List(x.toString)))) } else { MayFailSuccess(abs.bottom) }
      MayFail.monoid[Abs].append(t, f)
    }
  }
  object ExactToInexact extends NoStoreOperation("exact->inexact", Some(1)) {
    override def call(x: Abs) = exactToInexact(x)
  }
  object InexactToExact extends NoStoreOperation("inexact->exact", Some(1)) {
    override def call(x: Abs) = inexactToExact(x)
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
    override def call(x: Abs) = modulo(x, abs.inject(2)) >>= (numEq(abs.inject(1), _))
  }
  /** (define (even? x) (= 0 (modulo x 2))) */
  object Evenp extends NoStoreOperation("even?", Some(1)) {
    override def call(x: Abs) = modulo(x, abs.inject(2)) >>= (numEq(abs.inject(0), _))
  }
  object Max extends NoStoreOperation("max") {
    /* TODO: In Scheme, max casts numbers to inexact as soon as one of them is inexact, but we don't support that */
    private def call(args: List[Abs], max: Abs): MayFail[Abs] = args match {
      case Nil => MayFailSuccess(max)
      case x :: rest => for {
        test <- lt(max, x)
        t <- if (abs.isTrue(test)) { call(rest, x) } else { abs.bottom.point[MayFail] }
        f <- if (abs.isFalse(test)) { call(rest, max) } else { abs.bottom.point[MayFail] }
      } yield abs.join(t, f)
    }
    override def call(args: List[Abs]) = args match {
      case Nil => VariadicArityError(name, 1, 0)
      case x :: rest => call(rest, x)
    }
  }
  object Min extends NoStoreOperation("min") {
    /* TODO: same remark as max */
    private def call(args: List[Abs], min: Abs): MayFail[Abs] = args match {
      case Nil => MayFailSuccess(min)
      case x :: rest => for {
        test <- lt(x, min)
        t <- if (abs.isTrue(test)) { call(rest, x) } else { MayFailSuccess(abs.bottom) }
        f <- if (abs.isFalse(test)) { call(rest, min) } else { MayFailSuccess(abs.bottom) }
      } yield abs.join(t, f)
    }
    override def call(args: List[Abs]) = args match {
      case Nil => VariadicArityError(name, 1, 0)
      case x :: rest => call(rest, x)
    }
  }
  /** (define (abs x) (if (< x 0) (- 0 x) x)) */
  object Abs extends NoStoreOperation("abs", Some(1)) {
    override def call(x: Abs) = for {
      test <- lt(x, abs.inject(0))
      t <- if (abs.isTrue(test)) { minus(abs.inject(0), x) } else { abs.bottom.point[MayFail] }
      f <- if (abs.isFalse(test)) { x.point[MayFail] } else { abs.bottom.point[MayFail] }
    } yield abs.join(t, f)
  }
  /** (define (gcd a b) (if (= b 0) a (gcd b (modulo a b)))) */
  object Gcd extends NoStoreOperation("gcd", Some(2)) {
    private def gcd(a: Abs, b: Abs, visited: Set[(Abs, Abs)]): MayFail[Abs] = {
      if (visited.contains((a, b))) {
        abs.bottom.point[MayFail]
      } else {
        for {
          test <- numEq(b, abs.inject(0))
          t <- if (abs.isTrue(test)) { a.point[MayFail] } else { abs.bottom.point[MayFail] }
          f <- if (abs.isFalse(test)) { modulo(a, b) >>= (amodb => gcd(b, amodb, visited + ((a, b)))) } else { abs.bottom.point[MayFail] }
        } yield abs.join(t, f)
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
    override def call(x: Abs) = for {
      isint <- isInteger(x)
      isfloat <- isFloat(x)
    } yield abs.or(isint, isfloat)
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
    override def call(x: Abs, y: Abs) = eqq(x, y)
  }
  object Not extends NoStoreOperation("not", Some(1)) {
    override def call(x: Abs) = not(x)
  }
  object NumberToString extends NoStoreOperation("number->string", Some(1)) {
    override def call(x: Abs) = numberToString(x)
  }
  object SymbolToString extends NoStoreOperation("symbol->string", Some(1)) {
    override def call(x: Abs) = symbolToString(x)
  }
  object StringAppend extends NoStoreOperation("string-append") {
    override def call(args: List[Abs]) = args match {
      case Nil => MayFailSuccess(abs.inject(""))
      case x :: rest => call(rest) >>= (stringAppend(x, _))
    }
  }
  object StringLt extends NoStoreOperation("string<?", Some(2)) {
    override def call(x: Abs, y: Abs) = stringLt(x, y)
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
      MayFailError(List(UserError(x._2.toString, Expression[Exp].pos(fexp))))
  }

  val mfmon = MayFail.monoid[(Abs, Set[Effect[Addr]])]
  def err(e: SemanticError): MayFail[(Abs, Set[Effect[Addr]])] = e
  def success(v: Abs): MayFail[(Abs, Set[Effect[Addr]])] = (v, Set[Effect[Addr]]()).point[MayFail]
  object Cons extends Primitive[Addr, Abs] {
    val name = "cons"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case (carexp, car) :: (cdrexp, cdr) :: Nil => {
        val cara = Address[Addr].cell(carexp, t)
        val cdra = Address[Addr].cell(cdrexp, t)
        MayFailSuccess((abs.cons(cara, cdra), store.extend(cara, car).extend(cdra, cdr), Set()))
      }
      case l => MayFailError(List(ArityError(name, 2, l.size)))
    }
  }
  private def car(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
    val addrs = abs.car(v)
    if (addrs.isEmpty) {
      CannotAccessCar(v.toString)
    } else {
      addrs.foldLeft(mfmon.zero)((acc, a) =>
        acc |+| (store.lookup(a) match {
          case Some(v) => (v, Set[Effect[Addr]](EffectReadConsCar(a))).point[MayFail]
          case None => UnboundAddress(a.toString)
        }))
    }
  }
  private def cdr(v: Abs, store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
    val addrs = abs.cdr(v)
    if (addrs.isEmpty) {
      err(CannotAccessCdr(v.toString))
    } else {
      addrs.foldLeft(mfmon.zero)((acc, a) =>
        mfmon.append(acc, store.lookup(a) match {
          case Some(v) => MayFailSuccess((v, Set(EffectReadConsCdr(a))))
          case None => err(UnboundAddress(a.toString))
        }))
    }
  }

  class CarCdrOperation(override val name: String) extends StoreOperation(name, Some(1)) {
    trait Spec
    case object Car extends Spec
    case object Cdr extends Spec
    val spec: List[Spec] = name.drop(1).take(name.length - 2).toList.reverseMap(c =>
      if (c == 'a') { Car }
      else if (c == 'd') { Cdr }
      else { throw new Exception("Incorrect car/cdr operation: $name") })
    override def call(v: Abs, store: Store[Addr, Abs]) =
      for { (v, effs) <- spec.foldLeft(success(v))((acc, op) => for {
        (v, effs) <- acc
        (vcxr, effs2) <- op match {
          case Car => car(v, store)
          case Cdr => cdr(v, store)
        }
      } yield (vcxr, effs ++ effs2)) } yield (v, store, effs)
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
          isCons(l) >>= (cond => {
            val t = if (abs.isTrue(cond)) {
              cdr(l, store) >>= ({ case (cdrl, effects1) =>
                length(cdrl, visited + l) >>= ({ case (lengthcdrl, effects2) =>
                  plus(abs.inject(1), lengthcdrl).map(lengthl =>
                    (lengthl, effects1 ++ effects2))
                })
              })
            } else { mfmon.zero }
            val f = if (abs.isFalse(cond)) {
              isNull(l) >>= (fcond => {
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

  object ListPrim extends StoreOperation("list", None) {
    override def call[Exp: Expression, Time: Timestamp](fexp: Exp,
                                                        args: List[(Exp, Abs)],
                                                        store: Store[Addr, Abs],
                                                        t: Time): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])] = {
      val pos = implicitly[Expression[Exp]].pos(fexp)

      val nilv = abs.nil
      val nila = Address[Addr].variable(Identifier("_nil_", pos), abs.bottom, t) // Hack to make sure addresses use the position of fexp
      val init: (Abs, Addr, Store[Addr, Abs]) = (nilv, nila, store)
      /*
       * If args is empty, the store should not be extended, so we allocate an address, but only forward it to
       * the next iteration, so that this next iteration (if there is one) can use it to extend the store.
       */
      val result = args.zipWithIndex.reverse.foldLeft(init)({
        case ((cdrv, cdra, store), ((argExp, argv), index)) =>
          val cara = Address[Addr].cell(argExp, t)
          val cons = abs.cons(cara, cdra)
          val newStore = store.extend(cdra, cdrv).extend(cara, argv)
          val paira = Address[Addr].variable(Identifier(s"_cons_${index}", pos), abs.bottom, t) // Hack to make sure addresses use the position of fexp
          (abs.cons(cara, cdra), paira, newStore)
      })
      MayFailSuccess((result._1, result._3, Set()))
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
          isNull(l) >>= (nulltest => {
            val t = if (abs.isTrue(nulltest)) { MayFailSuccess((nulltest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(nulltest)) {
              isCons(l) >>= (constest => {
                val ft = if (abs.isTrue(constest)) {
                  cdr(l, store) >>= ({ case (cdrl, effects1) =>
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
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = {
      def createVec(size: Abs, init: Abs, initaddr: Addr): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])]  = {
        isInteger(size) >>= (isint =>
          if (abs.isTrue(isint)) {
            val vaddr = Address[Addr].cell(fexp, t)
            abs.vector(vaddr, size, initaddr).map({ case (va, vector) =>
              (va, store.extend(vaddr, vector).extend(initaddr, init), Set.empty)})
          } else {
            MayFailError(List(TypeError("make-vector", "first operand", "integer", size.toString)))
          })
      }
      args match {
        case (_, size) :: Nil => createVec(size, abs.inject(false), Address[Addr].primitive("__undef-vec-element__"))
        case (_, size) :: (initexp, init) :: Nil => createVec(size, init, Address[Addr].cell(initexp, t))
        case l => MayFailError(List(ArityError(name, 1, l.size)))
      }
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
                acc >>= ({ case (oldval, store, effects) =>
                  val targetaddr = Address[Addr].cell(exp, t)
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
      val a = Address[Addr].cell(fexp, t)
      val botaddr = Address[Addr].primitive("__bottom__")
      abs.vector(a, abs.inject(args.size), botaddr) >>= ({ case (va, emptyVector) =>
        /* No tracked effects because we only perform atomic updates at allocation time */
        val init: MayFail[(Abs, Store[Addr, Abs])] = MayFailSuccess((emptyVector, store))
        args.zipWithIndex.foldLeft(init)((acc, arg) => acc >>= ({ case (vec, store) =>
          arg match {
            case ((exp, value), index) =>
              val valaddr = Address[Addr].cell(exp, t)
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
            case Some(v) => abs.vectorRef(v, index) >>= (vs =>
              vs.foldLeft(acc)((acc, a) =>
                mfmon.append(acc,
                  store.lookup(a) match {
                    case Some(value) => MayFailSuccess((value, Set(EffectReadVector(a))))
                    case None => err(UnboundAddress(a.toString))
                  })))
            case None => mfmon.append(acc, MayFailError(List(UnboundAddress(va.toString))))
          })
      }
    }
    override def call(v: Abs, index: Abs, store: Store[Addr, Abs]) =
      vectorRef(v, index, store).map({ case (v, effs) => (v, store, effs) })
  }

  /** (define (list-ref l index)
        (if (pair? l)
          (if (= index 0)
            (car l)
            (list-ref (cdr l) (- index 1)))
          (error "list-ref applied to a non-list"))) */
  object ListRef extends StoreOperation("list-ref", Some(2)) {
    def listRef(l: Abs, index: Abs, visited: Set[(Abs, Abs)], store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      if (visited.contains((l, index))) {
        (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
      } else {
        isCons(l) >>= { constest => {
          val t: MayFail[(Abs, Set[Effect[Addr]])] = if (abs.isTrue(constest)) {
            numEq(index, abs.inject(0)) >>= { indextest =>
              val tt = if (abs.isTrue(indextest)) {
                car(l, store)
              } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
              val tf = if (abs.isFalse(indextest)) {
                minus(index, abs.inject(1)) >>= { index2 => cdr(l, store) >>= {
                  case (cdrl, effcdr) => listRef(cdrl, index2, visited + ((l, index)), store).map({ case (v, effs) => (v, effs ++ effcdr) }) } }
              } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
              MayFail.monoid[(Abs, Set[Effect[Addr]])].append(tt, tf)
            }
          } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
          val f: MayFail[(Abs, Set[Effect[Addr]])] = if (abs.isFalse(constest)) {
            MayFailError(List(OperatorNotApplicable("list-ref: first argument not a list or index out of bounds", List(l.toString, index.toString))))
          } else { (abs.bottom, Set.empty[Effect[Addr]]).point[MayFail] }
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
        }}
      }
    }
    override def call(l: Abs, index: Abs, store: Store[Addr, Abs]) =
      listRef(l, index, Set.empty, store).map({ case (v, effs) => (v, store, effs) })
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
        if (visited.contains((a, b, i, n)) || a == abs.bottom || b == abs.bottom || i == abs.bottom || n == abs.bottom) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          numEq(i, n) >>= (numtest => {
            val t = if (abs.isTrue(numtest)) { MayFailSuccess((abs.inject(true), Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(numtest)) {
              VectorRef.vectorRef(a, i, store) >>= ({ case (vai, effects1) =>
                VectorRef.vectorRef(b, i, store) >>= ({ case (vbi, effects2) =>
                  equalp(vai, vbi, visitedEqual) >>= ({ case (itemtest, effects3) => {
                    val tt = if (abs.isTrue(itemtest)) {
                      plus(i, abs.inject(1)) >>= (iplus1 =>
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
        if (visited.contains((a, b)) || a == abs.bottom || b == abs.bottom) {
          MayFailSuccess((abs.bottom, Set[Effect[Addr]]()))
        } else {
          val visited2 = visited + ((a, b))
          eqq(a, b) >>= (eqtest => {
            val t = if (abs.isTrue(eqtest)) { MayFailSuccess((eqtest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
            val f = if (abs.isFalse(eqtest)) {
              isNull(a) >>= (anull => isNull(b) >>= (bnull => {
                val nulltest = abs.and(anull, bnull)
                val ft = if (abs.isTrue(nulltest)) { MayFailSuccess((eqtest, Set[Effect[Addr]]())) } else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                val ff = if (abs.isFalse(nulltest)) {
                  isCons(a) >>= (acons => isCons(b) >>= (bcons => {
                    val constest = abs.and(acons, bcons)
                    val fft = if (abs.isTrue(constest)) {
                      car(a, store) >>= ({ case (acar, effects1) =>
                        car(b, store) >>= ({ case (bcar, effects2) =>
                          equalp(acar, bcar, visited2) >>= ({ case (cartest, effects3) => {
                            val fftt = if (abs.isTrue(cartest)) {
                              cdr(a, store) >>= ({ case (acdr, effects4) =>
                                cdr(b, store) >>= ({ case (bcdr, effects5) =>
                                  equalp(acdr, bcdr, visited2).map({ case (eqtest, effects6) =>
                                    (eqtest, effects1 ++ effects2 ++ effects3 ++ effects4 ++ effects5 ++ effects6)
                                  })
                                })
                              })
                            } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                            val fftf = if (abs.isFalse(cartest)) { MayFailSuccess((abs.inject(false)), effects1 ++ effects2 ++ effects3) } else { MayFailSuccess((abs.bottom, effects1 ++ effects2 ++ effects3)) }
                            MayFail.monoid[(Abs, Set[Effect[Addr]])].append(fftt, fftf)
                          }})})})} else { MayFailSuccess((abs.bottom, Set[Effect[Addr]]())) }
                    val fff = if (abs.isFalse(constest)) {
                      isVector(a) >>= (avec =>
                        isVector(b) >>= (bvec => {
                          val vectest = abs.and(avec, bvec)
                          val ffft = if (abs.isTrue(vectest)) {
                            VectorLength.length(a, store) >>= ({ case (alength, effects1) =>
                              VectorLength.length(b, store) >>= ({ case (blength, effects2) =>
                                numEq(alength, blength) >>= (lengthtest => {
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

  /** (define (member e l) ; member, memq and memv are similar, the difference lies in the comparison function used
       (if (null? l)
         #f
         (if (equal? (car l) e)
           l
           (member e (cdr l))))) */
  abstract class MemberLike(val n: String, eqFn: (Abs, Abs, Store[Addr, Abs]) => MayFail[(Abs, Set[Effect[Addr]])]) extends StoreOperation(n, Some(2)) {
    def mem(e: Abs, l: Abs, visited: Set[Abs], store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      if (visited.contains(l)) {
        (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
      } else {
        isNull(l) >>= { nulltest => {
          val t = if (abs.isTrue(nulltest)) {
            (abs.inject(false), Set[Effect[Addr]]()).point[MayFail]
          } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
          val f = if (abs.isFalse(nulltest)) {
            car(l, store) >>= { case (carl, careff) =>
              eqFn(e, carl, store) >>= { case (equaltest, equaleff) =>
                val ft = if (abs.isTrue(equaltest)) {
                  (l, careff ++ equaleff).point[MayFail]
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                val ff = if (abs.isFalse(equaltest)) {
                  cdr(l, store) >>= { case (cdrl, cdreff) =>
                    mem(e, cdrl, visited + l, store).map({ case (res, effs) => (res, effs ++ careff ++ cdreff) })
                  }
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              }
            }
          } else {
            (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
          }
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
        }}
      }
    }
    override def call(e: Abs, l: Abs, store: Store[Addr, Abs]) =
      mem(e, l, Set.empty, store).map({ case (v, effs) => (v, store, effs) })
  }

  object Member extends MemberLike("member",
    (x: Abs, y: Abs, store: Store[Addr, Abs]) => Equal.call(x, y, store).map({ case (res, _ /* unchanged store */, eff) => (res, eff) }))
  object Memq extends MemberLike("memq",
    (x: Abs, y: Abs, store: Store[Addr, Abs]) => Eq.call(x, y).map(res => (res, Set.empty)))

  abstract class AssocLike(val n: String, eqFn: (Abs, Abs, Store[Addr, Abs]) => MayFail[(Abs, Set[Effect[Addr]])]) extends StoreOperation(n, Some(2)) {
    def assoc(e: Abs, l: Abs, visited: Set[Abs], store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = {
      if (visited.contains(l)) {
        (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
      } else {
        isNull(l) >>= { nulltest => {
          val t = if (abs.isTrue(nulltest)) {
            (abs.inject(false), Set[Effect[Addr]]()).point[MayFail]
          } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
          val f = if (abs.isFalse(nulltest)) {
            car(l, store) >>= { case (carl, careff) =>
              isCons(carl) >>= { constest =>
                val ft = if (abs.isTrue(constest)) {
                  car(carl, store) >>= { case (caarl, caareff) =>
                    eqFn(e, caarl, store) >>= { case (equaltest, equaleff) =>
                      val ftt = if (abs.isTrue(equaltest)) {
                        (carl, careff ++ caareff ++ equaleff).point[MayFail]
                      } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                      val ftf = if (abs.isFalse(equaltest)) {
                        cdr(l, store) >>= { case (cdrl, cdreff) =>
                          assoc(e, cdrl, visited + l, store).map({ case (res, effs) => (res, effs ++ careff ++ cdreff ++ equaleff) })
                        }
                      } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                      MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ftt, ftf)
                    }
                  }
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                val ff = if (abs.isFalse(constest)) {
                  (abs.inject(false), Set[Effect[Addr]]()).point[MayFail] /* or error? because it's not an alist */
                } else { (abs.bottom, Set[Effect[Addr]]()).point[MayFail] }
                MayFail.monoid[(Abs, Set[Effect[Addr]])].append(ft, ff)
              }
            }
          } else {
            (abs.bottom, Set[Effect[Addr]]()).point[MayFail]
          }
          MayFail.monoid[(Abs, Set[Effect[Addr]])].append(t, f)
        }}
      }
    }
    override def call(e: Abs, l: Abs, store: Store[Addr, Abs]) =
      assoc(e, l, Set.empty, store).map({ case (v, effs) => (v, store, effs) })
  }

  object Assoc extends AssocLike("assoc",
    (x: Abs, y: Abs, store: Store[Addr, Abs]) => Equal.call(x, y, store).map({ case (res, _ /* unchanged store */, eff) => (res, eff) }))
  object Assq extends AssocLike("assq",
    (x: Abs, y: Abs, store: Store[Addr, Abs]) => Eq.call(x, y).map(res => (res, Set.empty)))


  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def all: List[Primitive[Addr, Abs]] = List(
    Times,          /* [vv] *: Arithmetic */
    Plus,           /* [vv] +: Arithmetic */
    Minus,          /* [vv] -: Arithmetic */
    Div,            /* [vx] /: Arithmetic (no support for fractions) */
    Abs,            /* [vv] abs: Arithmetic */
    ACos,           /* [vv] acos: Scientific */
                    /* [x]  angle: Complex */
                    /* [x]  append: Append/Reverse */
                    /* [x]  apply: Fly Evaluation */
                    /* [x]  apply: Fly Evaluation */
    ASin,           /* [vv] asin: Scientific */
    Assoc,          /* [vv] assoc: Retrieving Alist Entries */
    Assq,           /* [vv] assq: Retrieving Alist Entries */
                    /* [x]  assv: Retrieving Alist Entries */
    ATan,           /* [vv] atan: Scientific [easy] */
    Booleanp,       /* [vv] boolean?: Booleans */
                    /* [x]  call-with-current-continuation: Continuations */
                    /* [x]  call-with-input-file: File Ports */
                    /* [x]  call-with-output-file: File Ports */
                    /* [x]  call-with-values: Multiple Values */
    Car,            /* [vv] car: Pairs */
    Cdr,            /* [vv] cdr: Pairs */
    Ceiling,        /* [vv] ceiling: Arithmetic */
                    /* [x]  char->integer: Characters */
                    /* [x]  char-alphabetic?: Characters */
                    /* [x]  char-ci<=?: Characters */
                    /* [x]  char-ci<?: Characters */
                    /* [x]  char-ci=?: Characters */
                    /* [x]  char-ci>=?: Characters */
                    /* [x]  char-ci>?: Characters */
                    /* [x]  char-downcase: Characters */
                    /* [x]  char-lower-case?: Characters */
                    /* [x]  char-numeric?: Characters */
                    /* [x]  char-ready?: Reading */
                    /* [x]  char-upcase: Characters */
                    /* [x]  char-upper-case?: Characters */
                    /* [x]  char-whitespace?: Characters */
                    /* [x]  char<=?: Characters */
                    /* [x]  char<?: Characters */
                    /* [x]  char=?: Characters */
                    /* [x]  char>=?: Characters */
                    /* [x]  char>?: Characters */
    Charp,          /* [vv] char?: Characters */
                    /* [x]  close-input-port: Closing */
                    /* [x]  close-output-port: Closing */
                    /* [x]  complex?: Complex Numbers */
    Cons,           /* [vv] cons: Pairs */
    Cos,            /* [vv] cos: Scientific */
                    /* [x]  current-input-port: Default Ports */
                    /* [x]  current-output-port: Default Ports */
    Display,        /* [v]  display: Writing */
                    /* [x]  dynamic-wind: Dynamic Wind */
                    /* [x]  eof-object?: Reading */
    Eq,             /* [vv] eq?: Equality */
    Equal,          /* [vv] equal?: Equality */
                    /* [x]  eqv?: Equality */
                    /* [x]  eval: Fly Evaluation */
    Evenp,          /* [v]  even?: Integer Operations */
    ExactToInexact, /* [vv] exact->inexact: Exactness */
                    /* [x]  exact?: Exactness */
                    /* [x]  exp: Scientific */
                    /* [x]  expt: Scientific */
    Floor,          /* [vv] floor: Arithmetic */
                    /* [x]  for-each: List Mapping */
                    /* [x]  force: Delayed Evaluation */
    Gcd,            /* [vx] gcd: Integer Operations */
                    /* [x]  imag-part: Complex */
    InexactToExact, /* [vv] inexact->exact: Exactness */
                    /* [x]  inexact?: Exactness */
                    /* [x]  input-port?: Ports */
                    /* [x]  integer->char: Characters */
    Integerp,       /* [vv] integer?: Integers */
                    /* [x]  interaction-environment: Fly Evaluation */
                    /* [x]  lcm: Integer Operations */
    Length,         /* [vv] length: List Selection */
    ListPrim,       /* [vv] list: List Constructors */
                    /* [x]  list->string: String Constructors */
                    /* [x]  list->vector: Vector Creation */
    ListRef,        /* [vv] list-ref: List Selection */
                    /* [x]  list-tail: List Selection */
    Listp,          /* [vv] list?: List Predicates */
                    /* [x]  load: Loading */
    Log,            /* [vv] log: Scientific */
                    /* [x]  magnitude: Complex */
                    /* [x]  make-polar: Complex */
                    /* [x]  make-rectangular: Complex */
                    /* [x]  make-string: String Constructors */
    MakeVector,     /* [vv] make-vector: Vector Creation */
                    /* [x]  map: List Mapping */
    Max,            /* [vv] max: Arithmetic */
    Member,         /* [vv] member: List Searching */
    Memq,           /* [v]  memq: List Searching */
                    /* [x]  memv: List Searching */
    Min,            /* [vv] min: Arithmetic */
    Modulo,         /* [vv] modulo: Integer Operations */
    Negativep,      /* [vv] negative?: Comparison */
    Newline,        /* [v]  newline: Writing */
    Not,            /* [vv] not: Booleans */
    Nullp,          /* [vv] null?: List Predicates */
    NumberToString, /* [vx] number->string: Conversion: does not support two arguments */
    Numberp,        /* [vv] number?: Numerical Tower */
    Oddp,           /* [vv] odd?: Integer Operations */
                    /* [x]  open-input-file: File Ports */
                    /* [x]  open-output-file: File Ports */
                    /* [x]  output-port?: Ports */
    Pairp,          /* [vv] pair?: Pairs */
                    /* [x]  peek-char?: Reading */
    Positivep,      /* [vv] positive?: Comparison */
                    /* [x]  procedure?: Procedure Properties */
    Quotient,       /* [vv] quotient: Integer Operations */
                    /* [x]  rational?: Reals and Rationals */
                    /* [x]  read: Scheme Read */
                    /* [x]  read-char?: Reading */
                    /* [x]  real-part: Complex */
    Realp,          /* [vv] real?: Reals and Rationals */
    Remainder,      /* [vv] remainder: Integer Operations */
                    /* [x]  reverse: Append/Reverse */
    Round,          /* [vv] round: Arithmetic */
    SetCar,         /* [vv] set-car!: Pairs */
    SetCdr,         /* [vv] set-cdr!: Pairs */
    Sin,            /* [vv] sin: Scientific */
    Sqrt,           /* [vv] sqrt: Scientific */
                    /* [x]  string: String Constructors */
                    /* [x]  string->list: List/String Conversion */
                    /* [x]  string->number: Conversion */
                    /* [x]  string->symbol: Symbol Primitives */
    StringAppend,   /* [vx] string-append: Appending Strings: only two arguments supported */
                    /* [x]  string-ci<: String Comparison */
                    /* [x]  string-ci=?: String Comparison */
                    /* [x]  string-ci>=?: String Comparison */
                    /* [x]  string-ci>?: String Comparison */
                    /* [x]  string-copy: String Selection */
                    /* [x]  string-fill!: String Modification */
    StringLength,   /* [vv] string-length: String Selection */
                    /* [x]  string-ref: String Selection */
                    /* [x]  string-set!: String Modification */
                    /* [x]  string<=?: String Comparison */
    StringLt,       /* [vv]  string<?: String Comparison */
                    /* [x]  string=?: String Comparison */
                    /* [x]  string>=?: String Comparison */
                    /* [x]  string>?: String Comparison */
    Stringp,        /* [x]  string?: String Predicates */
                    /* [x]  substring: String Selection */
    SymbolToString, /* [vv] symbol->string: Symbol Primitives */
    Symbolp,        /* [vv] symbol?: Symbol Primitives */
    Tan,            /* [vv] tan: Scientific */
                    /* [x]  truncate: Arithmetic */
                    /* [x]  values: Multiple Values */
    Vector,         /* [vv] vector: Vector Creation */
                    /* [x]  vector->list: Vector Creation */
                    /* [x]  vector-fill!: Vector Accessors */
    VectorLength,   /* [vv] vector-length: Vector Accessors */
    VectorRef,      /* [vv] vector-ref: Vector Accessors */
    VectorSet,      /* [vv] vector-set!: Vector Accessors */
    Vectorp,        /* [vv] vector?: Vector Creation */
                    /* [x]  with-input-from-file: File Ports */
                    /* [x]  with-output-to-file: File Ports */
                    /* [x]  write-char: Writing */
    Zerop,          /* [vv] zero?: Comparison */
    LessThan,       /* [v]  < */
    LessOrEqual,    /* [v]  <= */
    NumEq,          /* [v]  = */
    GreaterThan,    /* [v]  > */
    GreaterOrEqual, /* [v]  >= */
                    /* [x]  numerator */
                    /* [x]  denominator */
                    /* [x]  rationalize-string */
                    /* [x]  scheme-report-environment */
                    /* [x]  null-environment */
                    /* [x]  write transcript-on */
                    /* [x]  transcript-off */
    Caar, Cadr,     /* [v]  caar etc. */
    Cddr, Caaar, Caadr, Cadar, Caddr, Cdaar, Cdadr, Cddar, Cdddr, Caaaar,
    Caaadr, Caadar, Caaddr, Cadaar, Cadadr, Caddar, Cadddr, Cdaaar,
    Cdaadr, Cdadar, Cdaddr, Cddaar, Cddadr, Cdddar, Cddddr,
    /* Other primitives that are not R5RS */
    Random, Error, BoolTop, IntTop)

  def toVal(prim: Primitive[Addr, Abs]): Abs = abs.inject(prim)
}
