package scalaam.language.scheme

import scalaam.core._

/**
  * TODO: Many primitives use non-tail recursion and could profit from being written in the same style as the Equalp primitive
  */
trait SchemePrimitives[A <: Address, V, T, C] extends SchemeSemantics[A, V, T, C] {
  implicit val timestamp: Timestamp[T, C]
  implicit val schemeLattice: SchemeLattice[V, SchemeExp, A]

  case class PrimitiveArityError(name: String, expected: Int, got: Int)                extends Error
  case class PrimitiveVariadicArityError(name: String, expectedAtLeast: Int, got: Int) extends Error
  case class PrimitiveNotApplicable(name: String, args: List[V])                       extends Error
  case class UserError(message: String)                                                extends Error
  trait Primitive {
    def name: String
    def callAction(fexp: SchemeExp,
                   args: List[(SchemeExp, V)],
                   store: Store[A, V],
                   t: T): Set[Action.A] =
      call(fexp, args, store, t)
        .mapSet[Action.A](vstore => Action.Value(vstore._1, vstore._2))(err => Action.Err(err))
    def call(fexp: SchemeExp,
             args: List[(SchemeExp, V)],
             store: Store[A, V],
             t: T): MayFail[(V, Store[A, V]), Error]
  }

  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def allPrimitives: List[Primitive] = {
    import PrimitiveDefs._
    List(
      Times, /* [vv] *: Arithmetic */
      Plus, /* [vv] +: Arithmetic */
      Minus, /* [vv] -: Arithmetic */
      Div, /* [vx] /: Arithmetic (no support for fractions) */
      Abs, /* [vv] abs: Arithmetic */
      ACos, /* [vv] acos: Scientific */
      /* [x]  angle: Complex */
      /* [x]  append: Append/Reverse */
      /* [x]  apply: Fly Evaluation */
      ASin, /* [vv] asin: Scientific */
      Assoc, /* [vv] assoc: Retrieving Alist Entries */
      Assq, /* [vv] assq: Retrieving Alist Entries */
      /* [x]  assv: Retrieving Alist Entries */
      ATan, /* [vv] atan: Scientific */
      Booleanp, /* [vv] boolean?: Booleans */
      /* [x]  call-with-current-continuation: Continuations */
      /* [x]  call-with-input-file: File Ports */
      /* [x]  call-with-output-file: File Ports */
      /* [x]  call-with-values: Multiple Values */
      Car, /* [vv] car: Pairs */
      Cdr, /* [vv] cdr: Pairs */
      Ceiling, /* [vv] ceiling: Arithmetic */
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
      Charp, /* [vv] char?: Characters */
      /* [x]  close-input-port: Closing */
      /* [x]  close-output-port: Closing */
      /* [x]  complex?: Complex Numbers */
      Cons, /* [vv] cons: Pairs */
      Cos, /* [vv] cos: Scientific */
      /* [x]  current-input-port: Default Ports */
      /* [x]  current-output-port: Default Ports */
      Display, /* [v]  display: Writing */
      /* [x]  dynamic-wind: Dynamic Wind */
      /* [x]  eof-object?: Reading */
      Eq, /* [vv] eq?: Equality */
      Equal, /* [vx] equal?: Equality */
      /* [x]  eqv?: Equality */
      /* [x]  eval: Fly Evaluation */
      Evenp, /* [vv] even?: Integer Operations */
      ExactToInexact, /* [vv] exact->inexact: Exactness */
      /* [x]  exact?: Exactness */
      /* [x]  exp: Scientific */
      Expt, /* [vv] expt: Scientific */
      Floor, /* [vv] floor: Arithmetic */
      /* [x]  for-each: List Mapping */
      /* [x]  force: Delayed Evaluation */
      Gcd, /* [vx] gcd: Integer Operations */
      /* [x]  imag-part: Complex */
      InexactToExact, /* [vv] inexact->exact: Exactness */
      /* [x]  inexact?: Exactness */
      /* [x]  input-port?: Ports */
      /* [x]  integer->char: Characters */
      Integerp, /* [vv] integer?: Integers */
      /* [x]  interaction-environment: Fly Evaluation */
      /* [x]  lcm: Integer Operations */
      Length, /* [vv] length: List Selection */
      ListPrim, /* [vv] list: List Constructors */
      /* [x]  list->string: String Constructors */
      /* [x]  list->vector: Vector Creation */
      ListRef, /* [vv] list-ref: List Selection */
      /* [x]  list-tail: List Selection */
      Listp, /* [vv] list?: List Predicates */
      /* [x]  load: Loading */
      Log, /* [vv] log: Scientific */
      /* [x]  magnitude: Complex */
      /* [x]  make-polar: Complex */
      /* [x]  make-rectangular: Complex */
      /* [x]  make-string: String Constructors */
      /* [x]  map: List Mapping */
      Max, /* [vv] max: Arithmetic */
      Member, /* [vv] member: List Searching */
      Memq, /* [v]  memq: List Searching */
      /* [x]  memv: List Searching */
      Min, /* [vv] min: Arithmetic */
      Modulo, /* [vv] modulo: Integer Operations */
      Negativep, /* [vv] negative?: Comparison */
      Newline, /* [v]  newline: Writing */
      Not, /* [vv] not: Booleans */
      Nullp, /* [vv] null?: List Predicates */
      NumberToString, /* [vx] number->string: Conversion: does not support two arguments */
      Numberp, /* [vv] number?: Numerical Tower */
      Oddp, /* [vv] odd?: Integer Operations */
      /* [x]  open-input-file: File Ports */
      /* [x]  open-output-file: File Ports */
      /* [x]  output-port?: Ports */
      Pairp, /* [vv] pair?: Pairs */
      /* [x]  peek-char?: Reading */
      Positivep, /* [vv] positive?: Comparison */
      /* [x]  procedure?: Procedure Properties */
      Quotient, /* [vv] quotient: Integer Operations */
      /* [x]  rational?: Reals and Rationals */
      /* [x]  read: Scheme Read */
      /* [x]  read-char?: Reading */
      /* [x]  real-part: Complex */
      Realp, /* [vv] real?: Reals and Rationals */
      Remainder, /* [vv] remainder: Integer Operations */
      /* [x]  reverse: Append/Reverse */
      Round, /* [vv] round: Arithmetic */
      SetCar, /* [vv] set-car!: Pairs */
      SetCdr, /* [vv] set-cdr!: Pairs */
      Sin, /* [vv] sin: Scientific */
      Sqrt, /* [vv] sqrt: Scientific */
      /* [x]  string: String Constructors */
      /* [x]  string->list: List/String Conversion */
      /* [x]  string->number: Conversion */
      StringToSymbol, /* [vv] string->symbol: Symbol Primitives */
      StringAppend, /* [vx] string-append: Appending Strings: only two arguments supported */
      /* [x]  string-ci<: String Comparison */
      /* [x]  string-ci=?: String Comparison */
      /* [x]  string-ci>=?: String Comparison */
      /* [x]  string-ci>?: String Comparison */
      /* [x]  string-copy: String Selection */
      /* [x]  string-fill!: String Modification */
      StringLength, /* [vv] string-length: String Selection */
      StringRef, /* [x]  string-ref: String Selection */
      /* [x]  string-set!: String Modification */
      /* [x]  string<=?: String Comparison */
      StringLt, /* [vv]  string<?: String Comparison */
      /* [x]  string=?: String Comparison */
      /* [x]  string>=?: String Comparison */
      /* [x]  string>?: String Comparison */
      Stringp, /* [vv]  string?: String Predicates */
      /* [x]  substring: String Selection */
      SymbolToString, /* [vv] symbol->string: Symbol Primitives */
      Symbolp, /* [vv] symbol?: Symbol Primitives */
      Tan, /* [vv] tan: Scientific */
      /* [x]  truncate: Arithmetic */
      /* [x]  values: Multiple Values */
      // TODO MakeVector,     /* [vv] make-vector: Vector Creation */
      // TODO Vector,         /* [vv] vector: Vector Creation */
      /* [x]  vector->list: Vector Creation */
      /* [x]  vector-fill!: Vector Accessors */
      // TODO VectorLength,   /* [vv] vector-length: Vector Accessors */
      // TODO VectorRef,      /* [vv] vector-ref: Vector Accessors */
      // TODO VectorSet,      /* [vv] vector-set!: Vector Accessors */
      // TODO Vectorp,        /* [vv] vector?: Vector Creation */
      /* [x]  with-input-from-file: File Ports */
      /* [x]  with-output-to-file: File Ports */
      /* [x]  write-char: Writing */
      Zerop, /* [vv] zero?: Comparison */
      LessThan, /* [vv]  < */
      LessOrEqual, /* [vv]  <= */
      NumEq, /* [vv]  = */
      GreaterThan, /* [vv]  > */
      GreaterOrEqual, /* [vv]  >= */
      /* [x]  numerator */
      /* [x]  denominator */
      /* [x]  rationalize-string */
      /* [x]  scheme-report-environment */
      /* [x]  null-environment */
      /* [x]  write transcript-on */
      /* [x]  transcript-off */
      Caar,
      Cadr, /* [v]  caar etc. */
      Cdar,
      Cddr,
      Caaar,
      Caadr,
      Cadar,
      Caddr,
      Cdaar,
      Cdadr,
      Cddar,
      Cdddr,
      Caaaar,
      Caaadr,
      Caadar,
      Caaddr,
      Cadaar,
      Cadadr,
      Caddar,
      Cadddr,
      Cdaaar,
      Cdaadr,
      Cdadar,
      Cdaddr,
      Cddaar,
      Cddadr,
      Cdddar,
      Cddddr,
      /* Other primitives that are not R5RS */
      Random,
      Error,
    )
  }

  object PrimitiveDefs {

    /** Helper for defining operations that do not modify the store */
    abstract class NoStoreOperation(val name: String, val nargs: Option[Int] = None)
        extends Primitive {
      def call(args: List[V]): MayFail[V, Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call(arg1: V, arg2: V): MayFail[V, Error] = call(List(arg1, arg2))
      def call(arg1: (SchemeExp, V), arg2: (SchemeExp, V)): MayFail[V, Error] =
        call(arg1._2, arg2._2)
      def call(fexp: SchemeExp, arg1: (SchemeExp, V), arg2: (SchemeExp, V)): MayFail[V, Error] =
        call(arg1, arg2)
      def call(arg: V): MayFail[V, Error]                               = call(List(arg))
      def call(arg: (SchemeExp, V)): MayFail[V, Error]                  = call(arg._2)
      def call(fexp: SchemeExp, arg: (SchemeExp, V)): MayFail[V, Error] = call(arg)
      def call(): MayFail[V, Error]                                     = call(List())
      def call(fexp: SchemeExp,
               args: List[(SchemeExp, V)],
               store: Store[A, V],
               t: T): MayFail[(V, Store[A, V]), Error] =
        (args match {
          case Nil           => call()
          case x :: Nil      => call(fexp, x)
          case x :: y :: Nil => call(fexp, x, y)
          case _             => call(args.map({ case (_, v) => v }))
        }).map(v => (v, store))
    }

    abstract class StoreOperation(val name: String, val nargs: Option[Int] = None)
        extends Primitive {
      def call(args: List[V], store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call(arg: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg), store)
      def call(arg1: V, arg2: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg1, arg2), store)
      def call(fexp: SchemeExp,
               arg1: (SchemeExp, V),
               arg2: (SchemeExp, V),
               store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(arg1._2, arg2._2, store)
      def call(fexp: SchemeExp,
               arg: (SchemeExp, V),
               store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(arg._2, store)
      def call(store: Store[A, V]): MayFail[(V, Store[A, V]), Error] = call(List(), store)
      def call(fexp: SchemeExp,
               args: List[(SchemeExp, V)],
               store: Store[A, V],
               t: T): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil           => call(store)
        case x :: Nil      => call(fexp, x, store)
        case x :: y :: Nil => call(fexp, x, y, store)
        case _             => call(args.map({ case (_, v) => v }), store)
      }
    }

    import schemeLattice._
    import scala.util.control.TailCalls._
    def liftTailRec(x: MayFail[TailRec[MayFail[V, Error]], Error]): TailRec[MayFail[V, Error]] = x match {
      case MayFailSuccess(v) => tailcall(v)
      case MayFailError(err) => done(MayFailError(err))
      case MayFailBoth(v, err) => tailcall(v).map(_.addErrors(err))
    }

    /** Dereferences a pointer x (which may point to multiple addresses) and applies a function to its value, joining everything together */
    def dereferencePointer(x: V, store: Store[A, V])(f: V => MayFail[V, Error]): MayFail[V, Error] =
      getPointerAddresses(x).foldLeft(MayFail.success[V, Error](bottom))(
        (acc: MayFail[V, Error], a: A) =>
          for {
            v    <- store.lookupMF(a)
            res  <- f(v)
            accv <- acc
          } yield join(accv, res))
    def dereferencePointerTR(x: V, store: Store[A, V])(f: V => TailRec[MayFail[V, Error]]): TailRec[MayFail[V, Error]] =
      getPointerAddresses(x).foldLeft(done(MayFail.success[V, Error](bottom)))(
        (acc: TailRec[MayFail[V, Error]], a: A) =>
          acc.flatMap(accv =>
            liftTailRec(store.lookupMF(a).map(f)).flatMap(fv =>
              done(fv.flatMap(res => accv.flatMap(accvv => join(accvv, res)))))))

    /* TODO[medium] improve these implicit classes to be able to write primitives more clearly */
    implicit class V1Ops(f: V => MayFail[V, Error]) {
      def apply(arg: MayFail[V, Error]): MayFail[V, Error] = arg >>= f
    }
    implicit class V2Ops(f: (V, => V) => V) {
      def apply(arg1: MayFail[V, Error], arg2: MayFail[V, Error]) =
        for {
          v1  <- arg1
          v2  <- arg2
          res <- f(v1, v2)
        } yield res
    }
    /*
    def app(f: V => MayFail[V, Error])(arg: MayFail[V, Error]): MayFail[V, Error] =
      arg >>= f
    def app2(f: (V, V) => MayFail[V, Error])(arg1: MayFail[V, Error], arg2: MayFail[V, Error]): MayFail[V, Error] =
      for {
        v1 <- arg1
        v2 <- arg2
        res <- f(v1, v2)
      } yield res */
    def ifThenElse(cond: MayFail[V, Error])(thenBranch: => MayFail[V, Error])(
        elseBranch: => MayFail[V, Error]): MayFail[V, Error] = {
      val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
      val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)
      cond >>= { condv =>
        val t = if (isTrue(condv)) { thenBranch } else { MayFail.success[V, Error](latMon.zero) }
        val f = if (isFalse(condv)) { elseBranch } else { MayFail.success[V, Error](latMon.zero) }
        mfMon.append(t, f)
      }
    }

    def ifThenElseTR(cond: MayFail[V, Error])(thenBranch: => TailRec[MayFail[V, Error]])(
      elseBranch: => TailRec[MayFail[V, Error]]): TailRec[MayFail[V, Error]] = {
      val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
      val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)
      liftTailRec(cond >>= { condv =>
        val t = if (isTrue(condv)) { thenBranch } else { done(MayFail.success[V, Error](latMon.zero)) }
        val f = if (isFalse(condv)) { elseBranch } else { done(MayFail.success[V, Error](latMon.zero)) }
        t.flatMap(tval =>
          f.map(fval =>
            mfMon.append(tval, fval)))
      })
    }

    import scala.language.implicitConversions
    implicit def fromMF[X](x: X): MayFail[X, Error] = MayFail.success(x)

    /* Simpler names for lattice operations */
    def isNull         = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsNull) _
    def isCons         = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsCons) _
    def isPointer      = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsPointer) _
    def isChar         = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsChar) _
    def isSymbol       = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsSymbol) _
    def isString       = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsString) _
    def isInteger      = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsInteger) _
    def isReal         = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsReal) _
    def isBoolean      = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsBoolean) _
    def isVector       = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsVector) _
    def ceiling        = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Ceiling) _
    def floor          = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Floor) _
    def round          = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Round) _
    def log            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Log) _
    def not            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Not) _
    def random         = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Random) _
    def sin            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Sin) _
    def asin           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ASin) _
    def cos            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Cos) _
    def acos           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ACos) _
    def tan            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Tan) _
    def atan           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ATan) _
    def sqrt           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Sqrt) _
    def vectorLength   = schemeLattice.unaryOp(SchemeOps.UnaryOperator.VectorLength) _
    def stringLength   = schemeLattice.unaryOp(SchemeOps.UnaryOperator.StringLength) _
    def numberToString = schemeLattice.unaryOp(SchemeOps.UnaryOperator.NumberToString) _
    def symbolToString = schemeLattice.unaryOp(SchemeOps.UnaryOperator.SymbolToString) _
    def stringToSymbol = schemeLattice.unaryOp(SchemeOps.UnaryOperator.StringToSymbol) _
    def inexactToExact = schemeLattice.unaryOp(SchemeOps.UnaryOperator.InexactToExact) _
    def exactToInexact = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ExactToInexact) _

    def plus         = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Plus) _
    def minus        = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Minus) _
    def times        = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Times) _
    def div          = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Div) _
    def quotient     = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Quotient) _
    def modulo       = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Modulo) _
    def remainder    = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Remainder) _
    def lt           = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Lt) _
    def numEq        = schemeLattice.binaryOp(SchemeOps.BinaryOperator.NumEq) _
    def eqq          = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Eq) _
    def stringAppend = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringAppend) _
    def stringRef    = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringRef) _
    def stringLt     = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringLt) _

    object Plus extends NoStoreOperation("+") {
      override def call(args: List[V]) = args match {
        case Nil       => number(0)
        case x :: rest => call(rest) >>= (plus(x, _))
      }
    }
    object Minus extends NoStoreOperation("-") {
      override def call(args: List[V]) = args match {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: Nil  => minus(number(0), x)
        case x :: rest => Plus.call(rest) >>= (minus(x, _))
      }
    }
    object Times extends NoStoreOperation("*") {
      override def call(args: List[V]) = args match {
        case Nil       => number(1)
        case x :: rest => call(rest) >>= (times(x, _))
      }
    }
    object Div extends NoStoreOperation("/") {
      override def call(args: List[V]) = args match {
        case Nil => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: rest =>
          for {
            multrest      <- Times.call(rest)
            r             <- div(x, multrest)
            fl            <- floor(r)
            isexact       <- eqq(r, fl)
            xisint        <- isInteger(x)
            multrestisint <- isInteger(multrest)
            convert       <- and(isexact, and(xisint, multrestisint))
            exr           <- inexactToExact(r)
            res           <- ifThenElse(convert) { exr } { r }
          } yield {
            res
          }
      }
    }
    object Quotient extends NoStoreOperation("quotient", Some(2)) {
      override def call(x: V, y: V) = quotient(x, y)
    }

    object Expt extends NoStoreOperation("expt") {
      def expt(x: V, y: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
        if (visited.contains(y) || x == bottom || y == bottom) {
          done(bottom)
        } else {
          ifThenElseTR(numEq(y, number(0))) {
            done(number(1))
          } {
            liftTailRec(minus(y, number(1)).flatMap(y1 =>
              tailcall(expt(x, y1, visited + y)).flatMap(exptrest =>
                done(exptrest.flatMap(y => times(x, y))))))
          }
        }
      }
      override def call(x: V, y: V) = expt(x, y, Set()).result
    }

    object LessThan extends NoStoreOperation("<", Some(2)) {
      override def call(x: V, y: V) =
        lt(x, y) /* TODO[easy]: < should accept any number of arguments (same for <= etc.) */
    }
    object LessOrEqual extends NoStoreOperation("<=", Some(2)) {
      override def call(x: V, y: V) = (or _)(lt(x, y), numEq(x, y)) /*for {
        ltres <- lt(x, y)
        eqres <- numEq(x, y)
      } yield or(ltres, eqres) */
    }
    object NumEq extends NoStoreOperation("=", Some(2)) {
      def eq(first: V, l: List[V]): MayFail[V, Error] = l match {
        case Nil => bool(true)
        case x :: rest =>
          ifThenElse(numEq(first, x)) {
            eq(first, rest)
          } {
            bool(false)
          }
      }
      override def call(args: List[V]) = args match {
        case Nil       => bool(true)
        case x :: rest => eq(x, rest)
      }
    }
    object GreaterThan extends NoStoreOperation(">", Some(2)) {
      override def call(x: V, y: V) = not(LessOrEqual.call(x, y))
    }
    object GreaterOrEqual extends NoStoreOperation(">=", Some(2)) {
      override def call(x: V, y: V) = not(LessThan.call(x, y))
    }
    object Modulo extends NoStoreOperation("modulo", Some(2)) {
      override def call(x: V, y: V) = modulo(x, y)
    }
    object Remainder extends NoStoreOperation("remainder", Some(2)) {
      override def call(x: V, y: V) = remainder(x, y)
    }
    object Random extends NoStoreOperation("random", Some(1)) {
      override def call(x: V) = random(x)
    }
    object Ceiling extends NoStoreOperation("ceiling", Some(1)) {
      override def call(x: V) = ceiling(x)
    }
    object Floor extends NoStoreOperation("floor", Some(1)) {
      override def call(x: V) = floor(x)
    }
    object Round extends NoStoreOperation("round", Some(1)) {
      override def call(x: V) = round(x)
    }
    object Log extends NoStoreOperation("log", Some(1)) {
      override def call(x: V) = log(x)
    }
    object Sin extends NoStoreOperation("sin", Some(1)) {
      override def call(x: V) = sin(x)
    }
    object ASin extends NoStoreOperation("asin", Some(1)) {
      override def call(x: V) = asin(x)
    }
    object Cos extends NoStoreOperation("cos", Some(1)) {
      override def call(x: V) = cos(x)
    }
    object ACos extends NoStoreOperation("acos", Some(1)) {
      override def call(x: V) = acos(x)
    }
    object Tan extends NoStoreOperation("tan", Some(1)) {
      override def call(x: V) = tan(x)
    }
    object ATan extends NoStoreOperation("atan", Some(1)) {
      override def call(x: V) = atan(x)
    }
    object Sqrt extends NoStoreOperation("sqrt", Some(1)) {
      override def call(x: V) =
        ifThenElse(LessOrEqual.call(number(0), x)) {
          /* n >= 0 */
          for {
            r          <- sqrt(x)
            fl         <- floor(r)
            argisexact <- isInteger(x)
            resisexact <- eqq(r, fl)
            convert    <- and(argisexact, resisexact)
            exr        <- inexactToExact(r)
            res        <- ifThenElse(convert) { exr } { r }
          } yield {
            res
          }
        } {
          /* n < 0 */
          MayFail.failure(PrimitiveNotApplicable("sqrt", List(x)))
        }
    }
    object ExactToInexact extends NoStoreOperation("exact->inexact", Some(1)) {
      override def call(x: V) = exactToInexact(x)
    }
    object InexactToExact extends NoStoreOperation("inexact->exact", Some(1)) {
      override def call(x: V) = inexactToExact(x)
    }

    /** (define (zero? x) (= x 0)) */
    object Zerop extends NoStoreOperation("zero?", Some(1)) {
      override def call(x: V) = numEq(number(0), x)
    }

    /** (define (positive? x) (< x 0)) */
    object Positivep extends NoStoreOperation("positive?", Some(1)) {
      override def call(x: V) = lt(number(0), x)
    }

    /** (define (positive? x) (< 0 x)) */
    object Negativep extends NoStoreOperation("negative?", Some(1)) {
      override def call(x: V) = lt(x, number(0))
    }

    /** (define (odd? x) (= 1 (modulo x 2))) */
    object Oddp extends NoStoreOperation("odd?", Some(1)) {
      override def call(x: V) = modulo(x, number(2)) >>= (numEq(number(1), _))
    }

    /** (define (even? x) (= 0 (modulo x 2))) */
    object Evenp extends NoStoreOperation("even?", Some(1)) {
      override def call(x: V) = modulo(x, number(2)) >>= (numEq(number(0), _))
    }
    object Max extends NoStoreOperation("max") {
      /* TODO: In Scheme, max casts numbers to inexact as soon as one of them is inexact, but we don't support that */
      private def call(args: List[V], max: V): MayFail[V, Error] = args match {
        case Nil => max
        case x :: rest =>
          ifThenElse(lt(max, x)) {
            call(rest, x)
          } {
            call(rest, max)
          }
      }
      override def call(args: List[V]) = args match {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: rest => call(rest, x)
      }
    }
    object Min extends NoStoreOperation("min") {
      /* TODO: same remark as max */
      private def call(args: List[V], min: V): MayFail[V, Error] = args match {
        case Nil => min
        case x :: rest =>
          ifThenElse(lt(x, min)) {
            call(rest, x)
          } {
            call(rest, min)
          }
      }
      override def call(args: List[V]) = args match {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: rest => call(rest, x)
      }
    }

    /** (define (abs x) (if (< x 0) (- 0 x) x)) */
    object Abs extends NoStoreOperation("abs", Some(1)) {
      override def call(x: V) =
        ifThenElse(lt(x, number(0))) {
          minus(number(0), x)
        } {
          x
        }
    }

    /** (define (gcd a b) (if (= b 0) a (gcd b (modulo a b)))) */
    object Gcd extends NoStoreOperation("gcd", Some(2)) {
      private def gcd(a: V, b: V, visited: Set[(V, V)]): TailRec[MayFail[V, Error]] = {
        if (visited.contains((a, b)) || a == bottom || b == bottom) {
          done(bottom)
        } else {
          ifThenElseTR(numEq(b, number(0))) {
            done(a)
          } {
            liftTailRec(modulo(a, b) >>= (amodb =>
              tailcall(gcd(b, amodb, visited + ((a, b))))))
          }
        }
      }
      override def call(x: V, y: V) = gcd(x, y, Set()).result
    }

    object Nullp extends NoStoreOperation("null?", Some(1)) {
      override def call(x: V) = isNull(x)
    }
    object Pairp extends NoStoreOperation("pair?", Some(1)) {
      /* TODO[easy]: this should be a store operation that dereferences the pointer to check if it is indeed a cons */
      override def call(x: V) = (or _)(isCons(x), isPointer(x))
    }
    object Charp extends NoStoreOperation("char?", Some(1)) {
      override def call(x: V) = isChar(x)
    }
    object Symbolp extends NoStoreOperation("symbol?", Some(1)) {
      override def call(x: V) = isSymbol(x)
    }
    object Stringp extends NoStoreOperation("string?", Some(1)) {
      override def call(x: V) = isString(x)
    }
    object Integerp extends NoStoreOperation("integer?", Some(1)) {
      override def call(x: V) = isInteger(x)
    }
    object Realp extends NoStoreOperation("real?", Some(1)) {
      override def call(x: V) =
        for {
          isint  <- isInteger(x)
          isreal <- isReal(x)
        } yield or(isint, isreal)
    }
    object Numberp extends NoStoreOperation("number?", Some(1)) {
      override def call(x: V) =
        Realp.call(x) /* No support for complex number, so number? is equivalent as real? */
    }
    object Booleanp extends NoStoreOperation("boolean?", Some(1)) {
      override def call(x: V) = isBoolean(x)
    }
    object Vectorp extends NoStoreOperation("vector?", Some(1)) {
      override def call(x: V) = isVector(x)
    }
    object Eq extends NoStoreOperation("eq?", Some(2)) {
      override def call(x: V, y: V) = eqq(x, y)
    }
    object Not extends NoStoreOperation("not", Some(1)) {
      override def call(x: V) = not(x)
    }
    object NumberToString extends NoStoreOperation("number->string", Some(1)) {
      override def call(x: V) = numberToString(x)
    }
    object SymbolToString extends NoStoreOperation("symbol->string", Some(1)) {
      override def call(x: V) = symbolToString(x)
    }
    object StringToSymbol extends NoStoreOperation("string->symbol", Some(1)) {
      override def call(x: V) = stringToSymbol(x)
    }
    object StringAppend extends NoStoreOperation("string-append") {
      override def call(args: List[V]) = args match {
        case Nil       => string("")
        case x :: rest => call(rest) >>= (stringAppend(x, _))
      }
    }
    object StringRef extends NoStoreOperation("string-ref") {
      override def call(args: List[V]) = args match {
        case s :: n :: Nil => stringRef(s, n)
        case l => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }
    object StringLt extends NoStoreOperation("string<?", Some(2)) {
      override def call(x: V, y: V) = stringLt(x, y)
    }
    object StringLength extends NoStoreOperation("string-length", Some(1)) {
      override def call(x: V) = stringLength(x)
    }
    object Newline extends NoStoreOperation("newline", Some(0)) {
      override def call() = { println(""); MayFailSuccess(bool(false)) }
    }
    object Display extends NoStoreOperation("display", Some(1)) {
      override def call(x: V) = {
        val str = x.toString
        print(if (str.startsWith("\"")) { str.substring(1, str.size - 1) } else { str })
        x /* Undefined behavior in R5RS, but we return the printed value */
      }
    }
    object Error extends NoStoreOperation("error", Some(1)) {
      override def call(fexp: SchemeExp, x: (SchemeExp, V)) =
        MayFail.failure(UserError(x._2.toString))
    }

    object Cons extends Primitive {
      val name = "cons"
      def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T) = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val consa = allocator.pointer(fexp, t)
          (pointer(consa), store.extend(consa, cons(car, cdr)))
        case l => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }

    class CarCdrOperation(override val name: String) extends StoreOperation(name, Some(1)) {
      trait Spec
      case object Car extends Spec
      case object Cdr extends Spec
      val spec: List[Spec] = name
        .drop(1)
        .take(name.length - 2)
        .toList
        .reverseMap(c =>
          if (c == 'a') { Car } else if (c == 'd') { Cdr } else {
            throw new Exception(s"Incorrect car/cdr operation: $name")
        })
      override def call(v: V, store: Store[A, V]) =
        for {
          v <- spec.foldLeft(MayFail.success[V, Error](v))((acc, op) =>
            for {
              v <- acc
              res <- dereferencePointer(v, store) { consv =>
                op match {
                  case Car => car(consv)
                  case Cdr => cdr(consv)
                }
              }
            } yield res)
        } yield (v, store)
    }

    object Car    extends CarCdrOperation("car")
    object Cdr    extends CarCdrOperation("cdr")
    object Caar   extends CarCdrOperation("caar")
    object Cadr   extends CarCdrOperation("cadr")
    object Cdar   extends CarCdrOperation("cdar")
    object Cddr   extends CarCdrOperation("cddr")
    object Caaar  extends CarCdrOperation("caaar")
    object Caadr  extends CarCdrOperation("caadr")
    object Cadar  extends CarCdrOperation("cadar")
    object Caddr  extends CarCdrOperation("caddr")
    object Cdaar  extends CarCdrOperation("cdaar")
    object Cdadr  extends CarCdrOperation("cdadr")
    object Cddar  extends CarCdrOperation("cddar")
    object Cdddr  extends CarCdrOperation("cdddr")
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
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))((acc, a) =>
            for {
              consv <- store.lookupMF(a) /* look up in old store */
              st    <- acc /* updated store */
              v1 = value /* update car */
              v2 <- cdr(consv) /* preserves cdr */
            } yield st.update(a, cons(v1, v2)))
          .map(store => (bool(false) /* undefined */, store))
    }
    object SetCdr extends StoreOperation("set-cdr!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))((acc, a) =>
            for {
              consv <- store.lookupMF(a) /* look up in old store */
              st    <- acc /* updated store */
              v1    <- car(consv) /* preserves car */
              v2 = value /* update cdr */
            } yield st.update(a, cons(v1, v2)))
          .map(store => (bool(false) /* undefined */, store))
    }

    object Length extends StoreOperation("length", Some(1)) {
      override def call(l: V, store: Store[A, V]) = {
        def length(l: V, visited: Set[V]): TailRec[MayFail[V, Error]] =
          if (visited.contains(l)) {
            done(bottom)
          } else {
            ifThenElseTR(isPointer(l)) {
              /* dereferences the pointer and applies length to the result */
              dereferencePointerTR(l, store) { consv =>
                tailcall(length(consv, visited + l))
              }
            } {
              ifThenElseTR(isCons(l)) {
                /* length of the list is length of its cdr plus one */
                liftTailRec(cdr(l) >>= (cdr =>
                  tailcall(length(cdr, visited + l)).flatMap(lengthcdr =>
                    liftTailRec(lengthcdr.flatMap(l => done(plus(number(1), l)))))))
              } {
                ifThenElseTR(isNull(l)) {
                  /* length of null is 0 */
                  done(number(0))
                } {
                  /* not a list */
                  done(MayFail.failure(PrimitiveNotApplicable("length", List(l))))
                }
              }
            }
          }
        length(l, Set()).result.map(v => (v, store))
      }
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

    // TODO: this is without vectors
    object Equal extends StoreOperation("equal?", Some(2)) {
      override def call(a: V, b: V, store: Store[A, V]) = {
        def equalp(a: V, b: V, visited: Set[(V, V)]): TailRec[MayFail[V, Error]] = {
          if (visited.contains((a, b)) || a == bottom || b == bottom) {
            done(bottom)
          } else {
            val visited2 = visited + ((a, b))
            ifThenElseTR(eqq(a, b)) {
              /* If a and b are eq?, then they are equal? */
              done(bool(true))
            } {
              ifThenElseTR((and _)(isNull(a), isNull(b))) {
                /* If both a and b are null, then they are equal? */
                done(bool(true))
              } {
                ifThenElseTR((and _)(isCons(a), isCons(b))) {
                  /* If both cons, check car and cdr */
                  liftTailRec(car(a).flatMap(cara =>
                    car(b).flatMap(carb =>
                      cdr(a).flatMap(cdra =>
                        cdr(b).flatMap(cdrb =>
                            tailcall(equalp(cara, carb, visited2)).flatMap(eqcar =>
                              tailcall(equalp(cdra, cdrb, visited2)).map(eqcdr =>
                                for {
                                  x <- eqcar
                                  y <- eqcdr
                                } yield and(x, y))))))))
                } {
                  ifThenElseTR((and _)(isPointer(a), isPointer(b))) {
                    /* both pointers, then look up their values */
                    dereferencePointerTR(a, store) { consa =>
                      dereferencePointerTR(b, store) { consb =>
                        tailcall(equalp(consa, consb, visited2))
                      }
                    }
                  } {
                    /* otherwise, there is no equality possible */
                    done(bool(false))
                  }
                }
              }
            }
          }
        }
        equalp(a, b, Set()).result.map(v => (v, store))
      }
    }

    /** (define list (lambda args
          (if (null? args)
            '()
            (if (pair? args)
              (cons (car args) (apply list (cdr args)))
              args))))
      */
    object ListPrim extends StoreOperation("list", None) {
      override def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T) =
        args match {
          case Nil => (nil, store)
          case (exp, v) :: rest =>
            for {
              (restv, store2) <- call(fexp, rest, store, t)
              consv  = cons(v, restv)
              consa  = allocator.pointer(exp, t)
              store3 = store2.extend(consa, consv)
            } yield (pointer(consa), store3)
        }
    }

    /** (define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l))) */
    object Listp extends StoreOperation("list?", Some(1)) {
      override def call(l: V, store: Store[A, V]) = {
        def listp(l: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
          if (visited.contains(l) || l == bottom) {
            /* R5RS: "all lists have finite length", and the cases where this is reached
             * include circular lists. If an abstract list reaches this point, it
             * may be also finite but will reach a true branch somewhere else, and
             * both booleans will get joined */
            done(bool(false))
          } else {
            ifThenElseTR(isNull(l)) {
              done(bool(true))
            } {
              ifThenElseTR(isCons(l)) {
                /* This is a cons, check that the cdr itself is a list */
                liftTailRec(cdr(l) >>= (cdrl => tailcall(listp(cdrl, visited + l))))
              } {
                ifThenElseTR(isPointer(l)) {
                  /* This is a pointer, dereference it and check if it is itself a list */
                  dereferencePointerTR(l, store) { consv =>
                    tailcall(listp(consv, visited + l))
                  }
                } {
                  /* Otherwise, not a list */
                  done(bool(false))
                }
              }
            }
          }
        }
        listp(l, Set()).result.map(v => (v, store))
      }
    }

    /** (define (list-ref l index)
          (if (pair? l)
            (if (= index 0)
              (car l)
              (list-ref (cdr l) (- index 1)))
            (error "list-ref applied to a non-list"))) */
    object ListRef extends StoreOperation("list-ref", Some(2)) {
      override def call(l: V, index: V, store: Store[A, V]) = {
        def listRef(l: V, index: V, visited: Set[(V, V)]): TailRec[MayFail[V, Error]] = {
          if (visited.contains((l, index)) || l == bottom || index == bottom) {
            done(bottom)
          } else {
            ifThenElseTR(isPointer(l)) {
              /* dereferences the pointer and list-ref that */
              dereferencePointerTR(l, store) { consv =>
                tailcall(listRef(consv, index, visited + ((l, index))))
              }
            } {
              ifThenElseTR(isCons(l)) {
                ifThenElseTR(numEq(index, number(0))) {
                  /* index is 0, return car */
                  done(car(l))
                } {
                  /* index is >0, decrease it and continue looking into the cdr */
                  liftTailRec(cdr(l) >>= (cdrl =>
                    minus(index, number(1)) >>= (index2 =>
                      tailcall(listRef(cdrl, index2, visited + ((l, index)))))))
                }
              } {
                /* not a list */
                done(MayFail.failure(PrimitiveNotApplicable("list-ref", List(l, index))))
              }
            }
          }
        }
        listRef(l, index, Set.empty).result.map(v => (v, store))
      }
    }

    /** (define (member e l) ; member, memq and memv are similar, the difference lies in the comparison function used
          (if (null? l)
            #f
            (if (equal? (car l) e)
              l
              (member e (cdr l))))) */
    abstract class MemberLike(override val name: String,
                              eqFn: (V, V, Store[A, V]) => MayFail[V, Error])
        extends StoreOperation(name, Some(2)) {
      override def call(e: V, l: V, store: Store[A, V]) = {
        def mem(e: V, l: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
          if (visited.contains(l) || e == bottom || l == bottom) {
            done(bottom)
          } else {
            ifThenElseTR(isNull(l)) {
              /* list is empty, return false */
              done(bool(false))
            } {
              ifThenElseTR(isPointer(l)) {
                dereferencePointerTR(l, store) { lv =>
                  liftTailRec(car(lv) >>= (carl =>
                    ifThenElseTR(eqFn(e, carl, store)) {
                      /* (car l) and e are equal, return l */
                      done(l)
                    } {
                      liftTailRec(cdr(lv) >>= (cdrl =>
                        tailcall(mem(e, cdrl, visited + l))))
                    }))
                }
              } {
                /* not a list. Note: it may be a cons, but cons shouldn't come from the outside
                 * as they are wrapped in pointers, so it shouldn't happen that
                 * l is a cons at this point */
                done(MayFail.failure(PrimitiveNotApplicable(name, List(e, l))))
              }
            }
          }
        }
        mem(e, l, Set.empty).result.map(v => (v, store))
      }
    }

    object Member
        extends MemberLike("member",
                           (x: V, y: V, store: Store[A, V]) => Equal.call(x, y, store).map(_._1))
    object Memq extends MemberLike("memq", (x: V, y: V, store: Store[A, V]) => Eq.call(x, y))

    abstract class AssocLike(override val name: String,
                             eqFn: (V, V, Store[A, V]) => MayFail[V, Error])
        extends StoreOperation(name, Some(2)) {
      override def call(e: V, l: V, store: Store[A, V]) = {
        def assoc(e: V, l: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
          if (visited.contains(l) || e == bottom || l == bottom) {
            done(bottom)
          } else {
            ifThenElseTR(isNull(l)) {
              done(bool(false))
            } {
              ifThenElseTR(isPointer(l)) {
                dereferencePointerTR(l, store) { lv =>
                  liftTailRec(car(lv) >>= (carl =>
                    ifThenElseTR(isPointer(carl)) {
                      dereferencePointerTR(carl, store) { carlv =>
                        liftTailRec(car(carlv) >>= (caarl =>
                          ifThenElseTR(eqFn(e, caarl, store)) {
                            done(carl)
                          } {
                            liftTailRec(cdr(lv) >>= (cdrl => tailcall(assoc(e, cdrl, visited + l))))
                          }))
                      }
                    } {
                      done(MayFail.failure(PrimitiveNotApplicable(name, List(e, l))))
                    }))
                }
              } {
                done(MayFail.failure(PrimitiveNotApplicable(name, List(e, l))))
              }
            }
          }
        }
        assoc(e, l, Set.empty).result.map(v => (v, store))
      }
    }
    object Assoc
        extends AssocLike("assoc",
                          (x: V, y: V, store: Store[A, V]) => Equal.call(x, y, store).map(_._1))
    object Assq extends AssocLike("assq", (x: V, y: V, store: Store[A, V]) => Eq.call(x, y))
  }
}
