package scalaam.language.scheme

import scalaam.core._

trait SchemeAllocator[A] {
  def pointer[C](pos2: (Identity.Position, Identity.Position), c: C): A
  def pointer(pos2: (Identity.Position, Identity.Position)): A = pointer(pos2,())
}

trait SchemePrimitive[V, A <: Address] extends Primitive {
  def call(fpos: Identity.Position,
           args: List[(Identity.Position, V)],
           store: Store[A, V],
           alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = call(fpos, fpos, args, store, alloc)
  def call(fpos: Identity.Position,
           cpos: Identity.Position,
           args: List[(Identity.Position, V)],
           store: Store[A, V],
    alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = call(fpos, args, store, alloc)
}

class MinimalSchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) {

  case class PrimitiveArityError(name: String, expected: Int, got: Int)                extends Error
  case class PrimitiveVariadicArityError(name: String, expectedAtLeast: Int, got: Int) extends Error
  case class PrimitiveNotApplicable(name: String, args: List[V])                       extends Error
  case class UserError(message: String)                                                extends Error

  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def allPrimitives: List[SchemePrimitive[V,A]] = {
    import PrimitiveDefs._
    List(
      Modulo,
      Times, /* [vv] *: Arithmetic */
      Plus, /* [vv] +: Arithmetic */
      Minus, /* [vv] -: Arithmetic */
      Div, /* [vx] /: Arithmetic (no support for fractions) */
      ACos, /* [vv] acos: Scientific */
      /* [x]  angle: Complex */
      /* [x]  apply: Fly Evaluation */
      ASin, /* [vv] asin: Scientific */
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
      CharacterToInteger, /* [x]  char->integer: Characters */
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
      /* [x]  dynamic-wind: Dynamic Wind */
      /* [x]  eof-object?: Reading */
      Eq, /* [vv] eq?: Equality */
      /* [x]  eqv?: Equality */
      /* [x]  eval: Fly Evaluation */
      ExactToInexact, /* [vv] exact->inexact: Exactness */
      /* [x]  exact?: Exactness */
      /* [x]  exp: Scientific */
      Expt, /* [vv] expt: Scientific */
      Floor, /* [vv] floor: Arithmetic */
      /* [x]  for-each: List Mapping */
      /* [x]  force: Delayed Evaluation */
      /* [x]  imag-part: Complex */
      InexactToExact, /* [vv] inexact->exact: Exactness */
      /* [x]  inexact?: Exactness */
      /* [x]  input-port?: Ports */
      /* [x]  integer->char: Characters */
      Integerp, /* [vv] integer?: Integers */
      /* [x]  interaction-environment: Fly Evaluation */
      /* [x]  lcm: Integer Operations */
      ListPrim, /* [vv] list: List Constructors */
      /* [x]  list->string: String Constructors */
      /* [x]  list->vector: Vector Creation */
      /* [x]  list-tail: List Selection */
      /* [x]  load: Loading */
      Log, /* [vv] log: Scientific */
      /* [x]  magnitude: Complex */
      /* [x]  make-polar: Complex */
      /* [x]  make-rectangular: Complex */
      /* [x]  make-string: String Constructors */
      /* [x]  map: List Mapping */
      /* [x]  memv: List Searching */
      Nullp, /* [vv] null?: List Predicates */
      NumberToString, /* [vx] number->string: Conversion: does not support two arguments */
      Numberp, /* [vv] number?: Numerical Tower */
//      Oddp, /* [vv] odd?: Integer Operations */
      /* [x]  open-input-file: File Ports */
      /* [x]  open-output-file: File Ports */
      /* [x]  output-port?: Ports */
      Pairp, /* [vv] pair?: Pairs */
      /* [x]  peek-char?: Reading */
//      Positivep, /* [vv] positive?: Comparison */
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
      MakeVector, /* [vv] make-vector: Vector Creation */
      Vector, /* [vv] vector: Vector Creation */
      /* [x]  vector->list: Vector Creation */
      /* [x]  vector-fill!: Vector Accessors */
      VectorLength, /* [vv] vector-length: Vector Accessors */
      VectorRef, /* [vv] vector-ref: Vector Accessors */
      VectorSet, /* [vv] vector-set!: Vector Accessors */
      Vectorp, /* [vv] vector?: Vector Creation */
      /* [x]  with-input-from-file: File Ports */
      /* [x]  with-output-to-file: File Ports */
      /* [x]  write-char: Writing */
      LessThan, /* [vv]  < */
      NumEq, /* [vv]  = */
      GreaterThan, /* [vv]  > */
      /* [x]  numerator */
      /* [x]  denominator */
      /* [x]  rationalize-string */
      /* [x]  scheme-report-environment */
      /* [x]  null-environment */
      /* [x]  write transcript-on */
      /* [x]  transcript-off */
      /* Other primitives that are not R5RS */
      Random,
      Error
    )
  }

  object PrimitiveDefs {

    lazy val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
    lazy val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)

    /** Helper for defining operations that do not modify the store */
    abstract class NoStoreOperation(val name: String, val nargs: Option[Int] = None)
        extends SchemePrimitive[V,A] {
      def call(args: List[V]): MayFail[V, Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call2(arg1: V, arg2: V): MayFail[V, Error] = call(List(arg1, arg2))
      def call2(arg1: (Identity.Position, V), arg2: (Identity.Position, V)): MayFail[V, Error] =
        call2(arg1._2, arg2._2)
      def call2pos(fpos: Identity.Position, arg1: (Identity.Position, V), arg2: (Identity.Position, V)): MayFail[V, Error] =
        call2(arg1, arg2)
      def call(arg: V): MayFail[V, Error]                               = call(List(arg))
      def call(arg: (Identity.Position, V)): MayFail[V, Error]                  = call(arg._2)
      def call1pos(fpos: Identity.Position, arg: (Identity.Position, V)): MayFail[V, Error] = call(arg)
      def call(): MayFail[V, Error]                                     = call(List())
      override def call(fpos: Identity.Position,
               args: List[(Identity.Position, V)],
               store: Store[A, V],
               alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] =
        (args match {
          case Nil           => call()
          case x :: Nil      => call1pos(fpos, x)
          case x :: y :: Nil => call2pos(fpos, x, y)
          case _             => call(args.map({ case (_, v) => v }))
        }).map(v => (v, store))
    }

    abstract class StoreOperation(val name: String, val nargs: Option[Int] = None)
        extends SchemePrimitive[V,A] {
      def call(args: List[V], store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call(arg: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg), store)
      def call(arg1: V, arg2: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg1, arg2), store)
      def call( fpos: Identity.Position,
                arg1: (Identity.Position, V),
                arg2: (Identity.Position, V),
                store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg1._2, arg2._2, store)
      def call( pos: Identity.Position,
                arg: (Identity.Position, V),
                store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg._2, store)
      def call(store: Store[A, V]): MayFail[(V, Store[A, V]), Error] = call(List(), store)
      override def call(fpos: Identity.Position,
               args: List[(Identity.Position, V)],
               store: Store[A, V],
               alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil           => call(store)
        case x :: Nil      => call(fpos, x, store)
        case x :: y :: Nil => call(fpos, x, y, store)
        case _             => call(args.map({ case (_, v) => v }), store)
      }
    }

    import schemeLattice._
    import scala.util.control.TailCalls._

    def liftTailRec(x: MayFail[TailRec[MayFail[V, Error]], Error]): TailRec[MayFail[V, Error]] =
      x match {
        case MayFailSuccess(v)   => tailcall(v)
        case MayFailError(err)   => done(MayFailError(err))
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
          } yield join(accv, res)
      )
    def dereferencePointerGetAddressReturnStore(x: V, store: Store[A, V])(
        f: (A, V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]
    ): MayFail[(V, Store[A, V]), Error] =
      getPointerAddresses(x).foldLeft(MayFail.success[(V, Store[A, V]), Error]((bottom, store)))(
        (acc: MayFail[(V, Store[A, V]), Error], a: A) =>
          acc >>= ({
            case (accv, updatedStore) =>
              /* We use the old store because the new added information can only negatively influence precision (as it didn't hold at the point of the function call */
              store.lookupMF(a) >>= (
                  v =>
                    /* But we pass the updated store around as it should reflect all updates */
                    f(a, v, updatedStore) >>= ({
                      case (res, newStore) =>
                        MayFail.success((join(accv, res), newStore))
                    })
                )
          })
      )

    def dereferencePointerTR(x: V, store: Store[A, V])(
        f: V => TailRec[MayFail[V, Error]]
    ): TailRec[MayFail[V, Error]] =
      getPointerAddresses(x).foldLeft(done(MayFail.success[V, Error](bottom)))(
        (acc: TailRec[MayFail[V, Error]], a: A) =>
          acc.flatMap(
            accv =>
              liftTailRec(store.lookupMF(a).map(f))
                .flatMap(fv => done(fv.flatMap(res => accv.flatMap(accvv => join(accvv, res)))))
          )
      )

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

    def ifThenElse(
        cond: MayFail[V, Error]
    )(thenBranch: => MayFail[V, Error])(elseBranch: => MayFail[V, Error]): MayFail[V, Error] = {
      cond >>= { condv =>
        val t = if (isTrue(condv)) {
          thenBranch
        } else {
          MayFail.success[V, Error](latMon.zero)
        }
        val f = if (isFalse(condv)) {
          elseBranch
        } else {
          MayFail.success[V, Error](latMon.zero)
        }
        mfMon.append(t, f)
      }
    }

    private trait Clause {
      def otherwise(act: => MayFail[V,Error]): MayFail[V,Error]
      def otherwise(cls: Clause): Clause = otherwise_(cls, this)
      def otherwise_(cls: Clause, parent: Clause) = new Clause {
        def otherwise(alt: => MayFail[V,Error]) = parent.otherwise(cls.otherwise(alt))
      }
    }

    private def ifV(prd: => MayFail[V,Error])(csq: => MayFail[V,Error]) = new Clause {
      def otherwise(alt: => MayFail[V,Error]) = ifThenElse(prd) { csq } { alt }
    }

    def ifThenElseTR(cond: MayFail[V, Error])(
        thenBranch: => TailRec[MayFail[V, Error]]
    )(elseBranch: => TailRec[MayFail[V, Error]]): TailRec[MayFail[V, Error]] = {
      val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
      val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)
      liftTailRec(cond >>= { condv =>
        val t = if (isTrue(condv)) {
          thenBranch
        } else {
          done(MayFail.success[V, Error](latMon.zero))
        }
        val f = if (isFalse(condv)) {
          elseBranch
        } else {
          done(MayFail.success[V, Error](latMon.zero))
        }
        t.flatMap(tval => f.map(fval => mfMon.append(tval, fval)))
      })
    }

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
    def characterToInt = schemeLattice.unaryOp(SchemeOps.UnaryOperator.CharacterToInteger) _

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
      override def call2(x: V, y: V) = quotient(x, y)
    }

    object Expt extends NoStoreOperation("expt") {
      def expt(x: V, y: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
        if (visited.contains(y) || x == bottom || y == bottom) {
          done(bottom)
        } else {
          ifThenElseTR(numEq(y, number(0))) {
            done(number(1))
          } {
            liftTailRec(
              minus(y, number(1)).flatMap(
                y1 =>
                  tailcall(expt(x, y1, visited + y))
                    .flatMap(exptrest => done(exptrest.flatMap(y => times(x, y))))
              )
            )
          }
        }
      }
      override def call2(x: V, y: V) = expt(x, y, Set()).result
    }

    object LessThan extends NoStoreOperation("<", Some(2)) {
      override def call2(x: V, y: V) =
        lt(x, y) /* TODO[easy]: < should accept any number of arguments (same for <= etc.) */
    }
    object LessOrEqual extends NoStoreOperation("<=", Some(2)) {
      override def call2(x: V, y: V) = (or _)(lt(x, y), numEq(x, y)) /*for {
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
      override def call2(x: V, y: V) = not(LessOrEqual.call2(x, y))
    }
    object GreaterOrEqual extends NoStoreOperation(">=", Some(2)) {
      override def call2(x: V, y: V) = not(LessThan.call2(x, y))
    }
    object Modulo extends NoStoreOperation("modulo", Some(2)) {
      override def call2(x: V, y: V) = modulo(x, y)
    }
    object Remainder extends NoStoreOperation("remainder", Some(2)) {
      override def call2(x: V, y: V) = remainder(x, y)
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
        ifThenElse(LessOrEqual.call2(number(0), x)) {
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
    object CharacterToInteger extends NoStoreOperation("char->integer", Some(1)) {
      override def call(x: V) = characterToInt(x)
    }

    object Nullp extends NoStoreOperation("null?", Some(1)) {
      override def call(x: V) = isNull(x)
    }
    object Pairp extends StoreOperation("pair?", Some(1)) {
      /* TODO[easy]: this should be a store operation that dereferences the pointer to check if it is indeed a cons */
      override def call(x: V, store: Store[A, V]) =
        (ifThenElse(isPointer(x)) {
          dereferencePointer(x, store) { v =>
            isCons(v)
          }
        } {
          bool(false)
        }).map(v => (v, store))
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
    object Vectorp extends StoreOperation("vector?", Some(1)) {
      override def call(x: V, store: Store[A, V]) =
        for {
          ispointer <- isPointer(x)
          isvector <- dereferencePointer(x, store) { v =>
            isVector(v)
          }
        } yield (and(ispointer, isvector), store)
    }
    object Eq extends NoStoreOperation("eq?", Some(2)) {
      override def call2(x: V, y: V) = eqq(x, y)
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
        case l             => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }
    object StringLt extends NoStoreOperation("string<?", Some(2)) {
      override def call2(x: V, y: V) = stringLt(x, y)
    }
    object StringLength extends NoStoreOperation("string-length", Some(1)) {
      override def call(x: V) = stringLength(x)
    }
    object Newline extends NoStoreOperation("newline", Some(0)) {
      override def call() = { /* disabled println(""); */ MayFailSuccess(bool(false)) }
    }
    object Error extends NoStoreOperation("error", Some(1)) {
      override def call1pos(fpos: Identity.Position, x: (Identity.Position, V)) =
        MayFail.failure(UserError(x._2.toString))
    }

    object Cons extends SchemePrimitive[V,A] {
      val name = "cons"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val consa = alloc.pointer((fpos, cpos))
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
        .reverseIterator
        .map(
          c =>
            if (c == 'a') {
              Car
            } else if (c == 'd') {
              Cdr
            } else {
              throw new Exception(s"Incorrect car/cdr operation: $name")
            }
        ).toList
      override def call(v: V, store: Store[A, V]) =
        for {
          v <- spec.foldLeft(MayFail.success[V, Error](v))(
            (acc, op) =>
              for {
                v <- acc
                res <- dereferencePointer(v, store) { consv =>
                  op match {
                    case Car => car(consv)
                    case Cdr => cdr(consv)
                  }
                }
              } yield res
          )
        } yield (v, store)
    }

    object Car    extends CarCdrOperation("car")
    object Cdr    extends CarCdrOperation("cdr")

    object SetCar extends StoreOperation("set-car!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1 = value /* update car */
                v2 <- cdr(consv) /* preserves cdr */
              } yield st.update(a, cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
    }
    object SetCdr extends StoreOperation("set-cdr!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1    <- car(consv) /* preserves car */
                v2 = value /* update cdr */
              } yield st.update(a, cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
    }

    /** (define list (lambda args
          (if (null? args)
            '()
            (if (pair? args)
              (cons (car args) (apply list (cdr args)))
              args))))
      */
    object ListPrim extends StoreOperation("list", None) {
      override def call(fpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) =
        args match {
          case Nil => (nil, store)
          case (exp, v) :: rest =>
            for {
              (restv, store2) <- call(fpos, rest, store, alloc)
              consv  = cons(v, restv)
              consa  = alloc.pointer((exp, fpos))
              store3 = store2.extend(consa, consv)
            } yield (pointer(consa), store3)
        }
    }

    object MakeVector extends SchemePrimitive[V,A] {
      val name = "make-vector"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        def createVec(size: V, init: V): MayFail[(V, Store[A, V]), Error] = {
          isInteger(size) >>= (
              isint =>
                if (isTrue(isint)) {
                  val veca = alloc.pointer((fpos, cpos))
                  vector(size, init) >>= (vec => (pointer(veca), store.extend(veca, vec)))
                } else {
                  MayFail.failure(PrimitiveNotApplicable(name, args.map(_._2)))
                }
            )
        }
        args match {
          case (_, size) :: Nil              => createVec(size, /* XXX: unspecified */ bool(false))
          case (_, size) :: (_, init) :: Nil => createVec(size, init)
          case l                             => MayFail.failure(PrimitiveVariadicArityError(name, 1, l.size))
        }
      }
    }

    object Vector extends SchemePrimitive[V,A] {
      val name = "vector"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        val veca = alloc.pointer((fpos, cpos))
        vector(number(args.size), bottom) >>= (
            emptyvec =>
              args.zipWithIndex.foldLeft(MayFail.success[V, Error](emptyvec))(
                (acc, arg) =>
                  acc >>= (
                      vec =>
                        arg match {
                          case ((_, value), index) =>
                            vectorSet(vec, number(index), value)
                        }
                    )
              )
          ) >>= (vec => (pointer(veca), store.extend(veca, vec)))
      }
    }

    object VectorLength extends StoreOperation("vector-length", Some(1)) {
      def length(v: V, store: Store[A, V]): MayFail[V, Error] = {
        dereferencePointer(v, store) { vec =>
          ifThenElse(isVector(vec)) {
            vectorLength(vec)
          } {
            MayFail.failure(PrimitiveNotApplicable(name, List(v)))
          }
        }
      }
      override def call(v: V, store: Store[A, V]) = {
        length(v, store).map(v => (v, store))
      }
    }

    object VectorRef extends StoreOperation("vector-ref", Some(2)) {
      def vectorRef(v: V, index: V, store: Store[A, V]): MayFail[V, Error] = {
        dereferencePointer(v, store) { vec =>
          ifThenElse(isVector(vec)) {
            schemeLattice.vectorRef(vec, index)
          } {
            MayFail.failure(PrimitiveNotApplicable(name, List(v, index)))
          }
        }
      }
      override def call(v: V, index: V, store: Store[A, V]) =
        vectorRef(v, index, store).map(v => (v, store))
    }

    object VectorSet extends StoreOperation("vector-set!", Some(3)) {
      def vectorSet(
          v: V,
          index: V,
          newval: V,
          store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] = {
        dereferencePointerGetAddressReturnStore(v, store) {
          case (veca, vec, store) =>
            isVector(vec) >>= (test => {
              val t: MayFail[(V, Option[(A, V)]), Error] =
                if (isTrue(test)) {
                  schemeLattice.vectorSet(vec, index, newval) >>= (
                      newvec =>
                        MayFail.success(( /* unspecified */ bool(false), Some((veca, newvec))))
                    )
                } else {
                  MayFail.success((bottom, None))
                }
              val f: MayFail[V, Error] =
                if (isFalse(test)) {
                  MayFail.failure(PrimitiveNotApplicable(name, List(v, index, newval)))
                } else {
                  MayFail.success(bottom)
                }
              t >>= ({
                case (tv, None) => f.join(MayFail.success(tv), join).map(v => (v, store))
                case (tv, Some((a, va))) =>
                  f.join(MayFail.success(tv), join).map(v => (v, store.update(a, va)))
              })
            })
        }
      }
      override def call(args: List[V], store: Store[A, V]) = args match {
        case v :: index :: newval :: Nil => vectorSet(v, index, newval, store)
        case _                           => MayFail.failure(PrimitiveArityError(name, 3, args.size))
      }
    }
  }
}

class ManualSchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) {

  case class PrimitiveArityError(name: String, expected: Int, got: Int)                extends Error
  case class PrimitiveVariadicArityError(name: String, expectedAtLeast: Int, got: Int) extends Error
  case class PrimitiveNotApplicable(name: String, args: List[V])                       extends Error
  case class UserError(message: String)                                                extends Error

  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def allPrimitives: List[SchemePrimitive[V,A]] = {
    import PrimitiveDefs._
    List(
      Times, /* [vv] *: Arithmetic */
      Plus, /* [vv] +: Arithmetic */
      Minus, /* [vv] -: Arithmetic */
      Div, /* [vx] /: Arithmetic (no support for fractions) */
      Abs, /* [vv] abs: Arithmetic */
      ACos, /* [vv] acos: Scientific */
      /* [x]  angle: Complex */
      Append, /* [x]  append: Append/Reverse */
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
      CharacterToInteger, /* [x]  char->integer: Characters */
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
      MakeVector, /* [vv] make-vector: Vector Creation */
      Vector, /* [vv] vector: Vector Creation */
      /* [x]  vector->list: Vector Creation */
      /* [x]  vector-fill!: Vector Accessors */
      VectorLength, /* [vv] vector-length: Vector Accessors */
      VectorRef, /* [vv] vector-ref: Vector Accessors */
      VectorSet, /* [vv] vector-set!: Vector Accessors */
      Vectorp, /* [vv] vector?: Vector Creation */
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
      Error
    )
  }

  object PrimitiveDefs {

    lazy val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
    lazy val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)

    /** Helper for defining operations that do not modify the store */
    abstract class NoStoreOperation(val name: String, val nargs: Option[Int] = None)
        extends SchemePrimitive[V,A] {
      def call(args: List[V]): MayFail[V, Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call2(arg1: V, arg2: V): MayFail[V, Error] = call(List(arg1, arg2))
      def call2(arg1: (Identity.Position, V), arg2: (Identity.Position, V)): MayFail[V, Error] =
        call2(arg1._2, arg2._2)
      def call2pos(fpos: Identity.Position, arg1: (Identity.Position, V), arg2: (Identity.Position, V)): MayFail[V, Error] =
        call2(arg1, arg2)
      def call(arg: V): MayFail[V, Error]                               = call(List(arg))
      def call(arg: (Identity.Position, V)): MayFail[V, Error]                  = call(arg._2)
      def call1pos(fpos: Identity.Position, arg: (Identity.Position, V)): MayFail[V, Error] = call(arg)
      def call(): MayFail[V, Error]                                     = call(List())
      override def call(fpos: Identity.Position,
               args: List[(Identity.Position, V)],
               store: Store[A, V],
               alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] =
        (args match {
          case Nil           => call()
          case x :: Nil      => call1pos(fpos, x)
          case x :: y :: Nil => call2pos(fpos, x, y)
          case _             => call(args.map({ case (_, v) => v }))
        }).map(v => (v, store))
    }

    abstract class StoreOperation(val name: String, val nargs: Option[Int] = None)
        extends SchemePrimitive[V,A] {
      def call(args: List[V], store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call(arg: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg), store)
      def call(arg1: V, arg2: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg1, arg2), store)
      def call( fpos: Identity.Position,
                arg1: (Identity.Position, V),
                arg2: (Identity.Position, V),
                store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg1._2, arg2._2, store)
      def call( pos: Identity.Position,
                arg: (Identity.Position, V),
                store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg._2, store)
      def call(store: Store[A, V]): MayFail[(V, Store[A, V]), Error] = call(List(), store)
      override def call(fpos: Identity.Position,
               args: List[(Identity.Position, V)],
               store: Store[A, V],
               alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil           => call(store)
        case x :: Nil      => call(fpos, x, store)
        case x :: y :: Nil => call(fpos, x, y, store)
        case _             => call(args.map({ case (_, v) => v }), store)
      }
    }

    import schemeLattice._
    import scala.util.control.TailCalls._

    abstract class FixpointPrimitiveUsingStore(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {

      // parameterized by
      // - the arguments to the recursive call
      type Args
      // - the representation of 'call components' (tuning precision)
      type Call
      // - a mapping from arguments to call components
      def callFor(args: Args): Call
      // - a function to compute the initial arguments from the primitive input
      def initialArgs(fpos: Identity.Position, argsWithExps: List[(Identity.Position, V)]): Option[Args]
      // - a function for updating the arguments when upon a new call to a 'call'
      def updateArgs(oldArgs: Args, newArgs: Args): Args
      // - (optional) a function for updating the result of a function call
      def updateResult(oldResult: MayFail[V,Error], newResult: MayFail[V,Error]): MayFail[V, Error] = mfMon.append(oldResult, newResult)
      // - a function to execute a single 'call' with given arguments
      def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[V,Error]

      override def call(fpos: Identity.Position,
               argsWithExps: List[(Identity.Position, V)],
               store: Store[A,V],
               alloc: SchemeAllocator[A]): MayFail[(V,Store[A,V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = initialArgs(fpos, argsWithExps) match {
          case Some(args) =>
            args
          case None =>
            return MayFail.failure(PrimitiveArityError(name,arity.getOrElse(-1),argsWithExps.length))
        }
        val initCall = callFor(initArgs)
        // for every call, keep track of the arguments
        var callArgs = Map[Call,Args]((initCall -> initArgs))
        // keep track of results for "visited" arguments
        var cache = Map[Call,MayFail[V,Error]]().withDefaultValue(mfMon.zero)
        // keep track of which calls depend on which other calls
        var deps = Map[Call,Set[Call]]().withDefaultValue(Set())
        // standard worklist algorithm
        var worklist = Set(initCall)
        while (worklist.nonEmpty) {
          // take the next arguments from the worklist
          val nextCall = worklist.head
          worklist = worklist - nextCall
          // call with the next arguments
          val nextArgs = callArgs(nextCall)
          val res = callWithArgs(nextArgs)(alloc, store, args => {
            val call = callFor(args)
            deps += (call -> (deps(call) + nextCall))
            callArgs.get(call) match {
              case None => // first time calling this 'call'
                worklist = worklist + call
                callArgs += (call -> args)
              case Some(oldArgs) => // call was already called
                val updatedArgs = updateArgs(oldArgs,args)
                if (updatedArgs != oldArgs) {
                  worklist = worklist + call
                  callArgs += (call -> updatedArgs)
                }
              }
            cache(call)
          })
          // update the cache and worklist
          val oldValue = cache(nextCall)
          val updatedValue = updateResult(oldValue, res)
          if (updatedValue != oldValue) {
            cache += (nextCall -> updatedValue)
            worklist ++= deps(nextCall)
          }
        }
        cache(initCall).map(v => (v,store))
      }
    }

    abstract class SimpleFixpointPrimitive(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {
      type Args = List[V]

      // Executes a single call with given arguments.
      def callWithArgs(args: Args, cache: Args => MayFail[V,Error]): MayFail[V,Error]

      override def call(fpos: Identity.Position,
               argsWithExps: List[(Identity.Position, V)],
               store: Store[A,V],
               alloc: SchemeAllocator[A]): MayFail[(V,Store[A,V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = arity match {
          case Some(a) if argsWithExps.length == a =>
            argsWithExps.map(_._2)
          case None =>
            return MayFail.failure(PrimitiveArityError(name,arity.getOrElse(-1),argsWithExps.length))
        }
        // for every call, keep track of the arguments
        // keep track of results for "visited" arguments
        var cache = Map[Args,MayFail[V,Error]]().withDefaultValue(mfMon.zero)
        // keep track of which calls depend on which other calls
        var deps = Map[Args,Set[Args]]().withDefaultValue(Set())
        // standard worklist algorithm
        var worklist = Set(initArgs)
        while (worklist.nonEmpty) {
          // take the next arguments from the worklist
          val nextArgs = worklist.head
          worklist = worklist - nextArgs
          // call with the next arguments
          val res = callWithArgs(nextArgs, args => {
            deps += (args -> (deps(args) + nextArgs))
            if (cache.get(args).isEmpty) worklist = worklist + args
            cache(args)
          })
          // update the cache and worklist
          val oldValue = cache(nextArgs)
          val updatedValue = mfMon.append(oldValue, res)
          if (updatedValue != oldValue) {
            cache += (nextArgs -> updatedValue)
            worklist ++= deps(nextArgs)
          }
        }
        cache(initArgs).map((_, store))
      }
    }

    // Simpler than FixpointPrimitiveUsingStore BUT allows callWithArgs to return a modified store...
    abstract class SimpleFixpointPrimitiveUsingStore(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {
      type Args = List[V]

      // Executes a single call with given arguments.
      //def callWithArgs(prim: Identity.Position, args: Args, store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[(V, Store[A,V]),Error] // MUTABLE STORE (REPLACED)
      def callWithArgs(prim: Identity.Position, args: Args, store: Store[A,V], cache: Args => MayFail[V,Error], alloc: SchemeAllocator[A]): MayFail[V,Error]

      override def call(fpos: Identity.Position,
               argsWithExps: List[(Identity.Position, V)],
               store: Store[A,V],
               alloc: SchemeAllocator[A]): MayFail[(V,Store[A,V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = arity match {
          case Some(a) if argsWithExps.length == a => argsWithExps.map(_._2)
          case None => return MayFail.failure(PrimitiveArityError(name, arity.getOrElse(-1), argsWithExps.length))
        }
        // for every call, keep track of the arguments
        // keep track of results for "visited" arguments
        var cache = Map[Args,MayFail[V,Error]]().withDefaultValue(mfMon.zero)
        // keep track of which calls depend on which other calls, independently of the position of the calls!
        var deps = Map[Args,Set[Args]]().withDefaultValue(Set())
        // standard worklist algorithm
        var worklist = Set(initArgs)
        //var curStore = store // MUTABLE STORE (UNNECESSARY)
        while (worklist.nonEmpty) {
          // take the next arguments from the worklist
          val nextArgs = worklist.head
          worklist = worklist - nextArgs
          // call with the next arguments
          val res = callWithArgs(fpos, nextArgs, store, args => {
            deps += (args -> (deps(args) + nextArgs))
            if (cache.get(args).isEmpty) worklist = worklist + args
            cache(args)
          }, alloc)
          // update the cache, worklist and store
          val oldValue = cache(nextArgs)
          val updatedValue = mfMon.append(oldValue, res)
          //val updatedValue = mfMon.append(oldValue, res >>= {case (vl, store) => curStore = store ; vl}) // MUTABLE STORE (REPLACED)
          if (updatedValue != oldValue) {
            cache += (nextArgs -> updatedValue)
            worklist ++= deps(nextArgs)
          }
        }
        cache(initArgs).map((_, store))
        //cache(initArgs).map((_, curStore)) // MUTABLE STORE (REPLACED)
      }
    }

    def liftTailRec(x: MayFail[TailRec[MayFail[V, Error]], Error]): TailRec[MayFail[V, Error]] =
      x match {
        case MayFailSuccess(v)   => tailcall(v)
        case MayFailError(err)   => done(MayFailError(err))
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
          } yield join(accv, res)
      )
    def dereferencePointerGetAddressReturnStore(x: V, store: Store[A, V])(
        f: (A, V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]
    ): MayFail[(V, Store[A, V]), Error] =
      getPointerAddresses(x).foldLeft(MayFail.success[(V, Store[A, V]), Error]((bottom, store)))(
        (acc: MayFail[(V, Store[A, V]), Error], a: A) =>
          acc >>= ({
            case (accv, updatedStore) =>
              /* We use the old store because the new added information can only negatively influence precision (as it didn't hold at the point of the function call */
              store.lookupMF(a) >>= (
                  v =>
                    /* But we pass the updated store around as it should reflect all updates */
                    f(a, v, updatedStore) >>= ({
                      case (res, newStore) =>
                        MayFail.success((join(accv, res), newStore))
                    })
                )
          })
      )

    def dereferencePointerTR(x: V, store: Store[A, V])(
        f: V => TailRec[MayFail[V, Error]]
    ): TailRec[MayFail[V, Error]] =
      getPointerAddresses(x).foldLeft(done(MayFail.success[V, Error](bottom)))(
        (acc: TailRec[MayFail[V, Error]], a: A) =>
          acc.flatMap(
            accv =>
              liftTailRec(store.lookupMF(a).map(f))
                .flatMap(fv => done(fv.flatMap(res => accv.flatMap(accvv => join(accvv, res)))))
          )
      )

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

    def ifThenElse(
        cond: MayFail[V, Error]
    )(thenBranch: => MayFail[V, Error])(elseBranch: => MayFail[V, Error]): MayFail[V, Error] = {
      cond >>= { condv =>
        val t = if (isTrue(condv)) {
          thenBranch
        } else {
          MayFail.success[V, Error](latMon.zero)
        }
        val f = if (isFalse(condv)) {
          elseBranch
        } else {
          MayFail.success[V, Error](latMon.zero)
        }
        mfMon.append(t, f)
      }
    }

    private trait Clause {
      def otherwise(act: => MayFail[V,Error]): MayFail[V,Error]
      def otherwise(cls: Clause): Clause = otherwise_(cls, this)
      def otherwise_(cls: Clause, parent: Clause) = new Clause {
        def otherwise(alt: => MayFail[V,Error]) = parent.otherwise(cls.otherwise(alt))
      }
    }

    private def ifV(prd: => MayFail[V,Error])(csq: => MayFail[V,Error]) = new Clause {
      def otherwise(alt: => MayFail[V,Error]) = ifThenElse(prd) { csq } { alt }
    }

    def ifThenElseTR(cond: MayFail[V, Error])(
        thenBranch: => TailRec[MayFail[V, Error]]
    )(elseBranch: => TailRec[MayFail[V, Error]]): TailRec[MayFail[V, Error]] = {
      val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
      val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)
      liftTailRec(cond >>= { condv =>
        val t = if (isTrue(condv)) {
          thenBranch
        } else {
          done(MayFail.success[V, Error](latMon.zero))
        }
        val f = if (isFalse(condv)) {
          elseBranch
        } else {
          done(MayFail.success[V, Error](latMon.zero))
        }
        t.flatMap(tval => f.map(fval => mfMon.append(tval, fval)))
      })
    }

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
    def characterToInt = schemeLattice.unaryOp(SchemeOps.UnaryOperator.CharacterToInteger) _

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
      override def call2(x: V, y: V) = quotient(x, y)
    }

    object Expt extends NoStoreOperation("expt") {
      def expt(x: V, y: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
        if (visited.contains(y) || x == bottom || y == bottom) {
          done(bottom)
        } else {
          ifThenElseTR(numEq(y, number(0))) {
            done(number(1))
          } {
            liftTailRec(
              minus(y, number(1)).flatMap(
                y1 =>
                  tailcall(expt(x, y1, visited + y))
                    .flatMap(exptrest => done(exptrest.flatMap(y => times(x, y))))
              )
            )
          }
        }
      }
      override def call2(x: V, y: V) = expt(x, y, Set()).result
    }

    object LessThan extends NoStoreOperation("<", Some(2)) {
      override def call2(x: V, y: V) =
        lt(x, y) /* TODO[easy]: < should accept any number of arguments (same for <= etc.) */
    }
    object LessOrEqual extends NoStoreOperation("<=", Some(2)) {
      override def call2(x: V, y: V) = (or _)(lt(x, y), numEq(x, y)) /*for {
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
      override def call2(x: V, y: V) = not(LessOrEqual.call2(x, y))
    }
    object GreaterOrEqual extends NoStoreOperation(">=", Some(2)) {
      override def call2(x: V, y: V) = not(LessThan.call2(x, y))
    }
    object Modulo extends NoStoreOperation("modulo", Some(2)) {
      override def call2(x: V, y: V) = modulo(x, y)
    }
    object Remainder extends NoStoreOperation("remainder", Some(2)) {
      override def call2(x: V, y: V) = remainder(x, y)
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
        ifThenElse(LessOrEqual.call2(number(0), x)) {
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
    object CharacterToInteger extends NoStoreOperation("char->integer", Some(1)) {
      override def call(x: V) = characterToInt(x)
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
    /*
    object Gcd extends NoStoreOperation("gcd", Some(2)) {
      private def gcd(a: V, b: V, visited: Set[(V, V)]): TailRec[MayFail[V, Error]] = {
        if (visited.contains((a, b)) || a == bottom || b == bottom) {
          done(bottom)
        } else {
          ifThenElseTR(numEq(b, number(0))) {
            done(a)
          } {
            liftTailRec(modulo(a, b) >>= (amodb => tailcall(gcd(b, amodb, visited + ((a, b))))))
          }
        }
      }
      override def call(x: V, y: V) = gcd(x, y, Set()).result
    }
    */
    object Gcd extends SimpleFixpointPrimitive("gcd", Some(2)) {
      def callWithArgs(args: Args, gcd: Args => MayFail[V, Error]): MayFail[V, Error] = {
        val a :: b :: Nil = args
        ifThenElse(numEq(b, number(0))) { a } {
          modulo(a, b) >>= (amodb => gcd(List(b, amodb)))
        }
      }
    }

    object Nullp extends NoStoreOperation("null?", Some(1)) {
      override def call(x: V) = isNull(x)
    }
    object Pairp extends StoreOperation("pair?", Some(1)) {
      /* TODO[easy]: this should be a store operation that dereferences the pointer to check if it is indeed a cons */
      override def call(x: V, store: Store[A, V]) =
        (ifThenElse(isPointer(x)) {
          dereferencePointer(x, store) { v =>
            isCons(v)
          }
        } {
          bool(false)
        }).map(v => (v, store))
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
    object Vectorp extends StoreOperation("vector?", Some(1)) {
      override def call(x: V, store: Store[A, V]) =
        for {
          ispointer <- isPointer(x)
          isvector <- dereferencePointer(x, store) { v =>
            isVector(v)
          }
        } yield (and(ispointer, isvector), store)
    }
    object Eq extends NoStoreOperation("eq?", Some(2)) {
      override def call2(x: V, y: V) = eqq(x, y)
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
        case l             => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }
    object StringLt extends NoStoreOperation("string<?", Some(2)) {
      override def call2(x: V, y: V) = stringLt(x, y)
    }
    object StringLength extends NoStoreOperation("string-length", Some(1)) {
      override def call(x: V) = stringLength(x)
    }
    object Newline extends NoStoreOperation("newline", Some(0)) {
      override def call() = { /* disabled println(""); */ MayFailSuccess(bool(false)) }
    }
    object Display extends NoStoreOperation("display", Some(1)) {
      override def call(x: V) = {
        /* // Disabled
        val str = x.toString
        print(if (str.startsWith("\"")) {
          str.substring(1, str.size - 1)
        } else {
          str
        }) */
        x /* Undefined behavior in R5RS, but we return the printed value */
      }
    }
    object Error extends NoStoreOperation("error", Some(1)) {
      override def call1pos(fpos: Identity.Position, x: (Identity.Position, V)) =
        MayFail.failure(UserError(x._2.toString))
    }

    object Cons extends SchemePrimitive[V,A] {
      val name = "cons"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val consa = alloc.pointer((fpos, cpos))
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
        .reverseIterator
        .map(
          c =>
            if (c == 'a') {
              Car
            } else if (c == 'd') {
              Cdr
            } else {
              throw new Exception(s"Incorrect car/cdr operation: $name")
            }
        ).toList
      override def call(v: V, store: Store[A, V]) =
        for {
          v <- spec.foldLeft(MayFail.success[V, Error](v))(
            (acc, op) =>
              for {
                v <- acc
                res <- dereferencePointer(v, store) { consv =>
                  op match {
                    case Car => car(consv)
                    case Cdr => cdr(consv)
                  }
                }
              } yield res
          )
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
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1 = value /* update car */
                v2 <- cdr(consv) /* preserves cdr */
              } yield st.update(a, cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
    }
    object SetCdr extends StoreOperation("set-cdr!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1    <- car(consv) /* preserves car */
                v2 = value /* update cdr */
              } yield st.update(a, cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
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
                  liftTailRec(
                    car(a).flatMap(
                      cara =>
                        car(b).flatMap(
                          carb =>
                            cdr(a).flatMap(
                              cdra =>
                                cdr(b).flatMap(
                                  cdrb =>
                                    tailcall(equalp(cara, carb, visited2)).flatMap(
                                      eqcar =>
                                        tailcall(equalp(cdra, cdrb, visited2)).map(
                                          eqcdr =>
                                            for {
                                              x <- eqcar
                                              y <- eqcdr
                                            } yield and(x, y)
                                        )
                                    )
                                )
                            )
                        )
                    )
                  )
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

    /** (define (length l)
          (if (null? l)
              0
              (+ 1 (length (cdr l)))))
    */
    object Length extends FixpointPrimitiveUsingStore("length",Some(1)) {
      /** the argument to length is just the current pair */
      type Args = V
      def initialArgs(fexp: Identity.Position, args: List[(Identity.Position, V)]) = args match {
        case (_, arg) :: Nil  => Some(arg)
        case _                => None
      }
      /** calls are just that argument */
      type Call = Args
      def callFor(args: V) = args
      /** since Args == Calls, the argument remains the same */
      def updateArgs(oldArgs: V, newArgs: V) = { assert(newArgs == oldArgs) ; oldArgs }
      /** The actual implementation of the primitive */
      override def callWithArgs(l: V)(alloc: SchemeAllocator[A], store: Store[A,V], length: V => MayFail[V,Error]): MayFail[V,Error] =
        ifV(isNull(l)) {
          // if we have l = '(), length(l) = 0
          number(0)
        } otherwise ifV(isPointer(l)) {
          // if we have l = cons(a,d), length(l) = length(d) + 1
          dereferencePointer(l, store) { consv =>
            for {
              next      <- cdr(consv)
              len_next  <- length(next)
              result    <- plus(len_next, number(1))
            } yield result
          }
        } otherwise {
          // if we have have something else (i.e., not a list), length throws a type error!
          MayFail.failure(PrimitiveNotApplicable("length", List(l)))
        }
    }
    /** (define (append l1 l2)
          (if (null? l1)
              l2
              (cons (car l1)
                    (append (cdr l1) l2))))
    */
    object Append extends FixpointPrimitiveUsingStore("append", Some(2)) {
      /** the arguments to append are the two given input arguments + the 'append expression' and 'the current index' into the list (used for allocation) */
      type Args = (V,V,Identity.Position,Int)
      def initialArgs(fexp: Identity.Position, args: List[(Identity.Position, V)]) = args match {
        case (_, l1) :: (_, l2) :: Nil  => Some((l1, l2, fexp, 0))
        case _                          => None
      }
      /** calls only take into account the current pair (= first argument of append) */
      type Call = V
      def callFor(args: Args) = args._1
      /** arguments: ignore changes to the index to ensure termination (and other args are guaranteed to be the same anyway)*/
      def updateArgs(oldArgs: Args, newArgs: Args) = oldArgs
      /** The actual implementation of append */
      override def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A,V], append: Args => MayFail[V,Error]): MayFail[V,Error] = args match {
        case (l1, l2, fexp, idx) =>
          ifV(isNull(l1)) {
            // if we have l1 = '(), append(l1,l2) = l2
            l2
          } otherwise ifV(isPointer(l1)) {
            // if we have l1 = cons(a,d), append(l1,l2) = cons(a,append(d,l2))
            val addr = alloc.pointer((fexp, fexp), idx)
            dereferencePointer(l1, store) { consv =>
              for {
                carv      <- car(consv)
                cdrv      <- cdr(consv)
                app_next  <- append((cdrv, l2, fexp, idx + 1))
                result    <- cons(carv, app_next)
              } yield {
                store.extend(addr, result)
                pointer(addr)
              }
            }
          } otherwise {
            // if we have have something else (i.e., not a list), append throws a type error!
            MayFail.failure(PrimitiveNotApplicable("length", List(l1)))
          }
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
      override def call(fpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) =
        args match {
          case Nil => (nil, store)
          case (exp, v) :: rest =>
            for {
              (restv, store2) <- call(fpos, rest, store, alloc)
              consv  = cons(v, restv)
              consa  = alloc.pointer((exp, fpos))
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
              // dereferences the pointer and list-ref that
              dereferencePointerTR(l, store) { consv =>
                tailcall(listRef(consv, index, visited + ((l, index))))
              }
            } {
              ifThenElseTR(isCons(l)) {
                ifThenElseTR(numEq(index, number(0))) {
                  // index is 0, return car
                  done(car(l))
                } {
                  // index is >0, decrease it and continue looking into the cdr
                  liftTailRec(
                    cdr(l) >>= (
                        cdrl =>
                          minus(index, number(1)) >>= (
                              index2 => tailcall(listRef(cdrl, index2, visited + ((l, index))))
                          )
                      )
                  )
                }
              } {
                // not a list
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
    abstract class MemberLike(
        override val name: String,
        eqFn: (V, V, Store[A, V]) => MayFail[V, Error]
    ) extends StoreOperation(name, Some(2)) {
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
                  liftTailRec(
                    car(lv) >>= (
                        carl =>
                          ifThenElseTR(eqFn(e, carl, store)) {
                            /* (car l) and e are equal, return l */
                            done(l)
                          } {
                            liftTailRec(cdr(lv) >>= (cdrl => tailcall(mem(e, cdrl, visited + l))))
                          }
                      )
                  )
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
        extends MemberLike(
          "member",
          (x: V, y: V, store: Store[A, V]) => Equal.call(x, y, store).map(_._1)
        )
    object Memq extends MemberLike("memq", (x: V, y: V, store: Store[A, V]) => Eq.call2(x, y))

    abstract class AssocLike(
        override val name: String,
        eqFn: (V, V, Store[A, V]) => MayFail[V, Error]
    ) extends StoreOperation(name, Some(2)) {
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
                  liftTailRec(
                    car(lv) >>= (
                        carl =>
                          ifThenElseTR(isPointer(carl)) {
                            dereferencePointerTR(carl, store) { carlv =>
                              liftTailRec(
                                car(carlv) >>= (
                                    caarl =>
                                      ifThenElseTR(eqFn(e, caarl, store)) {
                                        done(carl)
                                      } {
                                        liftTailRec(
                                          cdr(lv) >>= (
                                              cdrl => tailcall(assoc(e, cdrl, visited + l))
                                          )
                                        )
                                      }
                                  )
                              )
                            }
                          } {
                            done(MayFail.failure(PrimitiveNotApplicable(name, List(e, l))))
                          }
                      )
                  )
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
        extends AssocLike(
          "assoc",
          (x: V, y: V, store: Store[A, V]) => Equal.call(x, y, store).map(_._1)
        )
    object Assq extends AssocLike("assq", (x: V, y: V, store: Store[A, V]) => Eq.call2(x, y))

    object MakeVector extends SchemePrimitive[V,A] {
      val name = "make-vector"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        def createVec(size: V, init: V): MayFail[(V, Store[A, V]), Error] = {
          isInteger(size) >>= (
              isint =>
                if (isTrue(isint)) {
                  val veca = alloc.pointer((fpos, cpos))
                  vector(size, init) >>= (vec => (pointer(veca), store.extend(veca, vec)))
                } else {
                  MayFail.failure(PrimitiveNotApplicable(name, args.map(_._2)))
                }
            )
        }
        args match {
          case (_, size) :: Nil              => createVec(size, /* XXX: unspecified */ bool(false))
          case (_, size) :: (_, init) :: Nil => createVec(size, init)
          case l                             => MayFail.failure(PrimitiveVariadicArityError(name, 1, l.size))
        }
      }
    }

    object Vector extends SchemePrimitive[V,A] {
      val name = "vector"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        val veca = alloc.pointer((fpos, cpos))
        vector(number(args.size), bottom) >>= (
            emptyvec =>
              args.zipWithIndex.foldLeft(MayFail.success[V, Error](emptyvec))(
                (acc, arg) =>
                  acc >>= (
                      vec =>
                        arg match {
                          case ((_, value), index) =>
                            vectorSet(vec, number(index), value)
                        }
                    )
              )
          ) >>= (vec => (pointer(veca), store.extend(veca, vec)))
      }
    }

    object VectorLength extends StoreOperation("vector-length", Some(1)) {
      def length(v: V, store: Store[A, V]): MayFail[V, Error] = {
        dereferencePointer(v, store) { vec =>
          ifThenElse(isVector(vec)) {
            vectorLength(vec)
          } {
            MayFail.failure(PrimitiveNotApplicable(name, List(v)))
          }
        }
      }
      override def call(v: V, store: Store[A, V]) = {
        length(v, store).map(v => (v, store))
      }
    }

    object VectorRef extends StoreOperation("vector-ref", Some(2)) {
      def vectorRef(v: V, index: V, store: Store[A, V]): MayFail[V, Error] = {
        dereferencePointer(v, store) { vec =>
          ifThenElse(isVector(vec)) {
            schemeLattice.vectorRef(vec, index)
          } {
            MayFail.failure(PrimitiveNotApplicable(name, List(v, index)))
          }
        }
      }
      override def call(v: V, index: V, store: Store[A, V]) =
        vectorRef(v, index, store).map(v => (v, store))
    }

    object VectorSet extends StoreOperation("vector-set!", Some(3)) {
      def vectorSet(
          v: V,
          index: V,
          newval: V,
          store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] = {
        dereferencePointerGetAddressReturnStore(v, store) {
          case (veca, vec, store) =>
            isVector(vec) >>= (test => {
              val t: MayFail[(V, Option[(A, V)]), Error] =
                if (isTrue(test)) {
                  schemeLattice.vectorSet(vec, index, newval) >>= (
                      newvec =>
                        MayFail.success(( /* unspecified */ bool(false), Some((veca, newvec))))
                    )
                } else {
                  MayFail.success((bottom, None))
                }
              val f: MayFail[V, Error] =
                if (isFalse(test)) {
                  MayFail.failure(PrimitiveNotApplicable(name, List(v, index, newval)))
                } else {
                  MayFail.success(bottom)
                }
              t >>= ({
                case (tv, None) => f.join(MayFail.success(tv), join).map(v => (v, store))
                case (tv, Some((a, va))) =>
                  f.join(MayFail.success(tv), join).map(v => (v, store.update(a, va)))
              })
            })
        }
      }
      override def call(args: List[V], store: Store[A, V]) = args match {
        case v :: index :: newval :: Nil => vectorSet(v, index, newval, store)
        case _                           => MayFail.failure(PrimitiveArityError(name, 3, args.size))
      }
    }
  }
}


class CompiledSchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) {

  case class PrimitiveArityError(name: String, expected: Int, got: Int)                extends Error
  case class PrimitiveVariadicArityError(name: String, expectedAtLeast: Int, got: Int) extends Error
  case class PrimitiveNotApplicable(name: String, args: List[V])                       extends Error
  case class UserError(message: String)                                                extends Error

  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def allPrimitives: List[SchemePrimitive[V,A]] = {
    import PrimitiveDefs._
    List(
      `*`, /* [vv] *: Arithmetic */
      `+`, /* [vv] +: Arithmetic */
      `-`, /* [vv] -: Arithmetic */
      `/`, /* [vx] /: Arithmetic (no support for fractions) */
      `abs`, /* [vv] abs: Arithmetic */
      `acos`, /* [vv] acos: Scientific */
      /* [x]  angle: Complex */
      `append`, /* [x]  append: Append/Reverse */
      `reverse`,
      /* [x]  apply: Fly Evaluation */
      `asin`, /* [vv] asin: Scientific */
      `assoc`, /* [vv] assoc: Retrieving Alist Entries */
      `assq`, /* [vv] assq: Retrieving Alist Entries */
      /* [x]  assv: Retrieving Alist Entries */
      `atan`, /* [vv] atan: Scientific */
      `boolean?`, /* [vv] boolean?: Booleans */
      /* [x]  call-with-current-continuation: Continuations */
      /* [x]  call-with-input-file: File Ports */
      /* [x]  call-with-output-file: File Ports */
      /* [x]  call-with-values: Multiple Values */
      `car`, /* [vv] car: Pairs */
      `cdr`, /* [vv] cdr: Pairs */
      `ceiling`, /* [vv] ceiling: Arithmetic */
      `char->integer`, /* [x]  char->integer: Characters */
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
      `char?`, /* [vv] char?: Characters */
      /* [x]  close-input-port: Closing */
      /* [x]  close-output-port: Closing */
      /* [x]  complex?: Complex Numbers */
      `cons`, /* [vv] cons: Pairs */
      `cos`, /* [vv] cos: Scientific */
      /* [x]  current-input-port: Default Ports */
      /* [x]  current-output-port: Default Ports */
      `display`, /* [v]  display: Writing */
      /* [x]  dynamic-wind: Dynamic Wind */
      /* [x]  eof-object?: Reading */
      `eq?`, /* [vv] eq?: Equality */
      `equal?`, /* [vx] equal?: Equality */
      /* [x]  eqv?: Equality */
      /* [x]  eval: Fly Evaluation */
      `even?`, /* [vv] even?: Integer Operations */
      `exact->inexact`, /* [vv] exact->inexact: Exactness */
      /* [x]  exact?: Exactness */
      /* [x]  exp: Scientific */
      `expt`, /* [vv] expt: Scientific */
      `floor`, /* [vv] floor: Arithmetic */
      /* [x]  for-each: List Mapping */
      /* [x]  force: Delayed Evaluation */
      `gcd`, /* [vx] gcd: Integer Operations */
      `lcm`, 
      /* [x]  imag-part: Complex */
      `inexact->exact`, /* [vv] inexact->exact: Exactness */
      /* [x]  inexact?: Exactness */
      /* [x]  input-port?: Ports */
      /* [x]  integer->char: Characters */
      `integer?`, /* [vv] integer?: Integers */
      /* [x]  interaction-environment: Fly Evaluation */
      /* [x]  lcm: Integer Operations */
      `length`, /* [vv] length: List Selection */
      `list`, /* [vv] list: List Constructors */ // TODO
      /* [x]  list->string: String Constructors */
      /* [x]  list->vector: Vector Creation */
      `list-ref`, /* [vv] list-ref: List Selection */
      /* [x]  list-tail: List Selection */
      `list?`, /* [vv] list?: List Predicates */
      /* [x]  load: Loading */
      `log`, /* [vv] log: Scientific */
      /* [x]  magnitude: Complex */
      /* [x]  make-polar: Complex */
      /* [x]  make-rectangular: Complex */
      /* [x]  make-string: String Constructors */
      /* [x]  map: List Mapping */
      `max`, /* [vv] max: Arithmetic */
      `member`, /* [vv] member: List Searching */
      `memq`, /* [v]  memq: List Searching */
      /* [x]  memv: List Searching */
      `min`, /* [vv] min: Arithmetic */
      `modulo`, /* [vv] modulo: Integer Operations */
      `negative?`, /* [vv] negative?: Comparison */
      `newline`, /* [v]  newline: Writing */
      `not`, /* [vv] not: Booleans */
      `null?`, /* [vv] null?: List Predicates */
      `number->string`, /* [vx] number->string: Conversion: does not support two arguments */
      `number?`, /* [vv] number?: Numerical Tower */
      `odd?`, /* [vv] odd?: Integer Operations */
      /* [x]  open-input-file: File Ports */
      /* [x]  open-output-file: File Ports */
      /* [x]  output-port?: Ports */
      `pair?`, /* [vv] pair?: Pairs */
      /* [x]  peek-char?: Reading */
      `positive?`, /* [vv] positive?: Comparison */
      /* [x]  procedure?: Procedure Properties */
      `quotient`, /* [vv] quotient: Integer Operations */
      /* [x]  rational?: Reals and Rationals */
      /* [x]  read: Scheme Read */
      /* [x]  read-char?: Reading */
      /* [x]  real-part: Complex */
      `real?`, /* [vv] real?: Reals and Rationals */
      `remainder`, /* [vv] remainder: Integer Operations */
      /* [x]  reverse: Append/Reverse */
      `round`, /* [vv] round: Arithmetic */
      `set-car!`, /* [vv] set-car!: Pairs */
      `set-cdr!`, /* [vv] set-cdr!: Pairs */
      `sin`, /* [vv] sin: Scientific */
      `sqrt`, /* [vv] sqrt: Scientific */
      /* [x]  string: String Constructors */
      /* [x]  string->list: List/String Conversion */
      /* [x]  string->number: Conversion */
      `string->symbol`, /* [vv] string->symbol: Symbol Primitives */
      `string-append`, /* [vx] string-append: Appending Strings: only two arguments supported */
      /* [x]  string-ci<: String Comparison */
      /* [x]  string-ci=?: String Comparison */
      /* [x]  string-ci>=?: String Comparison */
      /* [x]  string-ci>?: String Comparison */
      /* [x]  string-copy: String Selection */
      /* [x]  string-fill!: String Modification */
      `string-length`, /* [vv] string-length: String Selection */
      `string-ref`, /* [x]  string-ref: String Selection */
      /* [x]  string-set!: String Modification */
      /* [x]  string<=?: String Comparison */
      `string<?`, /* [vv]  string<?: String Comparison */
      /* [x]  string=?: String Comparison */
      /* [x]  string>=?: String Comparison */
      /* [x]  string>?: String Comparison */
      `string?`, /* [vv]  string?: String Predicates */
      /* [x]  substring: String Selection */
      `symbol->string`, /* [vv] symbol->string: Symbol Primitives */
      `symbol?`, /* [vv] symbol?: Symbol Primitives */
      `tan`, /* [vv] tan: Scientific */
      /* [x]  truncate: Arithmetic */
      /* [x]  values: Multiple Values */
      `make-vector`, /* [vv] make-vector: Vector Creation */
      `vector`, /* [vv] vector: Vector Creation */
      /* [x]  vector->list: Vector Creation */
      /* [x]  vector-fill!: Vector Accessors */
      `vector-length`, /* [vv] vector-length: Vector Accessors */
      `vector-ref`, /* [vv] vector-ref: Vector Accessors */
      `vector-set!`, /* [vv] vector-set!: Vector Accessors */
      `vector?`, /* [vv] vector?: Vector Creation */
      /* [x]  with-input-from-file: File Ports */
      /* [x]  with-output-to-file: File Ports */
      /* [x]  write-char: Writing */
      `zero?`, /* [vv] zero?: Comparison */
      `<`, /* [vv]  < */
      `<=`, /* [vv]  <= */
      `=`, /* [vv]  = */
      `>`, /* [vv]  > */
      `>=`, /* [vv]  >= */
      /* [x]  numerator */
      /* [x]  denominator */
      /* [x]  rationalize-string */
      /* [x]  scheme-report-environment */
      /* [x]  null-environment */
      /* [x]  write transcript-on */
      /* [x]  transcript-off */
      `caar`,
      `cadr`, /* [v]  caar etc. */
      `cdar`,
      `cddr`,
      `caaar`,
      `caadr`,
      `cadar`,
      `caddr`,
      `cdaar`,
      `cdadr`,
      `cddar`,
      `cdddr`,
      `caaaar`,
      `caaadr`,
      `caadar`,
      `caaddr`,
      `cadaar`,
      `cadadr`,
      `caddar`,
      `cadddr`,
      `cdaaar`,
      `cdaadr`,
      `cdadar`,
      `cdaddr`,
      `cddaar`,
      `cddadr`,
      `cdddar`,
      `cddddr`,
      /* Other primitives that are not R5RS */
      `random`,
      `error`
    )
  }

  object PrimitiveDefs {

    lazy val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
    lazy val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)

    /** Helper for defining operations that do not modify the store */
    abstract class NoStoreOperation(val name: String, val nargs: Option[Int] = None)
        extends SchemePrimitive[V,A] {
      def call(args: List[V]): MayFail[V, Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call2(arg1: V, arg2: V): MayFail[V, Error] = call(List(arg1, arg2))
      def call2(arg1: (Identity.Position, V), arg2: (Identity.Position, V)): MayFail[V, Error] =
        call2(arg1._2, arg2._2)
      def call2pos(fpos: Identity.Position, arg1: (Identity.Position, V), arg2: (Identity.Position, V)): MayFail[V, Error] =
        call2(arg1, arg2)
      def call(arg: V): MayFail[V, Error]                               = call(List(arg))
      def call(arg: (Identity.Position, V)): MayFail[V, Error]                  = call(arg._2)
      def call1pos(fpos: Identity.Position, arg: (Identity.Position, V)): MayFail[V, Error] = call(arg)
      def call(): MayFail[V, Error]                                     = call(List())
      override def call(fpos: Identity.Position,
               args: List[(Identity.Position, V)],
               store: Store[A, V],
               alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] =
        (args match {
          case Nil           => call()
          case x :: Nil      => call1pos(fpos, x)
          case x :: y :: Nil => call2pos(fpos, x, y)
          case _             => call(args.map({ case (_, v) => v }))
        }).map(v => (v, store))
    }

    abstract class StoreOperation(val name: String, val nargs: Option[Int] = None)
        extends SchemePrimitive[V,A] {
      def call(args: List[V], store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def call(arg: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg), store)
      def call(arg1: V, arg2: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error] =
        call(List(arg1, arg2), store)
      def call( fpos: Identity.Position,
                arg1: (Identity.Position, V),
                arg2: (Identity.Position, V),
                store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg1._2, arg2._2, store)
      def call( pos: Identity.Position,
                arg: (Identity.Position, V),
                store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg._2, store)
      def call(store: Store[A, V]): MayFail[(V, Store[A, V]), Error] = call(List(), store)
      override def call(fpos: Identity.Position,
               args: List[(Identity.Position, V)],
               store: Store[A, V],
               alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil           => call(store)
        case x :: Nil      => call(fpos, x, store)
        case x :: y :: Nil => call(fpos, x, y, store)
        case _             => call(args.map({ case (_, v) => v }), store)
      }
    }

    import schemeLattice._
    import scala.util.control.TailCalls._

    abstract class FixpointPrimitiveUsingStore(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {

      // parameterized by
      // - the arguments to the recursive call
      type Args
      // - the representation of 'call components' (tuning precision)
      type Call
      // - a mapping from arguments to call components
      def callFor(args: Args): Call
      // - a function to compute the initial arguments from the primitive input
      def initialArgs(fpos: Identity.Position, argsWithExps: List[(Identity.Position, V)]): Option[Args]
      // - a function for updating the arguments when upon a new call to a 'call'
      def updateArgs(oldArgs: Args, newArgs: Args): Args
      // - (optional) a function for updating the result of a function call
      def updateResult(oldResult: MayFail[V,Error], newResult: MayFail[V,Error]): MayFail[V, Error] = mfMon.append(oldResult, newResult)
      // - a function to execute a single 'call' with given arguments
      def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[V,Error]

      override def call(fpos: Identity.Position,
               argsWithExps: List[(Identity.Position, V)],
               store: Store[A,V],
               alloc: SchemeAllocator[A]): MayFail[(V,Store[A,V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = initialArgs(fpos, argsWithExps) match {
          case Some(args) =>
            args
          case None =>
            return MayFail.failure(PrimitiveArityError(name,arity.getOrElse(-1),argsWithExps.length))
        }
        val initCall = callFor(initArgs)
        // for every call, keep track of the arguments
        var callArgs = Map[Call,Args]((initCall -> initArgs))
        // keep track of results for "visited" arguments
        var cache = Map[Call,MayFail[V,Error]]().withDefaultValue(mfMon.zero)
        // keep track of which calls depend on which other calls
        var deps = Map[Call,Set[Call]]().withDefaultValue(Set())
        // standard worklist algorithm
        var worklist = Set(initCall)
        while (worklist.nonEmpty) {
          // take the next arguments from the worklist
          val nextCall = worklist.head
          worklist = worklist - nextCall
          // call with the next arguments
          val nextArgs = callArgs(nextCall)
          val res = callWithArgs(nextArgs)(alloc, store, args => {
            val call = callFor(args)
            deps += (call -> (deps(call) + nextCall))
            callArgs.get(call) match {
              case None => // first time calling this 'call'
                worklist = worklist + call
                callArgs += (call -> args)
              case Some(oldArgs) => // call was already called
                val updatedArgs = updateArgs(oldArgs,args)
                if (updatedArgs != oldArgs) {
                  worklist = worklist + call
                  callArgs += (call -> updatedArgs)
                }
              }
            cache(call)
          })
          // update the cache and worklist
          val oldValue = cache(nextCall)
          val updatedValue = updateResult(oldValue, res)
          if (updatedValue != oldValue) {
            cache += (nextCall -> updatedValue)
            worklist ++= deps(nextCall)
          }
        }
        cache(initCall).map(v => (v,store))
      }
    }

    abstract class SimpleFixpointPrimitive(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {
      type Args = List[V]

      // Executes a single call with given arguments.
      def callWithArgs(args: Args, cache: Args => MayFail[V,Error]): MayFail[V,Error]

      override def call(fpos: Identity.Position,
               argsWithExps: List[(Identity.Position, V)],
               store: Store[A,V],
               alloc: SchemeAllocator[A]): MayFail[(V,Store[A,V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = arity match {
          case Some(a) if argsWithExps.length == a =>
            argsWithExps.map(_._2)
          case None =>
            return MayFail.failure(PrimitiveArityError(name,arity.getOrElse(-1),argsWithExps.length))
        }
        // for every call, keep track of the arguments
        // keep track of results for "visited" arguments
        var cache = Map[Args,MayFail[V,Error]]().withDefaultValue(mfMon.zero)
        // keep track of which calls depend on which other calls
        var deps = Map[Args,Set[Args]]().withDefaultValue(Set())
        // standard worklist algorithm
        var worklist = Set(initArgs)
        while (worklist.nonEmpty) {
          // take the next arguments from the worklist
          val nextArgs = worklist.head
          worklist = worklist - nextArgs
          // call with the next arguments
          val res = callWithArgs(nextArgs, args => {
            deps += (args -> (deps(args) + nextArgs))
            if (cache.get(args).isEmpty) worklist = worklist + args
            cache(args)
          })
          // update the cache and worklist
          val oldValue = cache(nextArgs)
          val updatedValue = mfMon.append(oldValue, res)
          if (updatedValue != oldValue) {
            cache += (nextArgs -> updatedValue)
            worklist ++= deps(nextArgs)
          }
        }
        cache(initArgs).map((_, store))
      }
    }

        // Simpler than FixpointPrimitiveUsingStore BUT allows callWithArgs to return a modified store...
    abstract class SimpleFixpointPrimitiveUsingStore(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {
      type Args = List[(Identity.Position, V)]

      // Executes a single call with given arguments.
      //def callWithArgs(prim: Identity.Position, args: Args, store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[(V, Store[A,V]),Error] // MUTABLE STORE (REPLACED)
      def callWithArgs(prim: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A,V], cache: List[(Identity.Position, V)] => MayFail[V,Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]),Error]

      override def call(fpos: Identity.Position, cpos: Identity.Position,
               argsWithExps: List[(Identity.Position, V)],
               store: Store[A,V],
               alloc: SchemeAllocator[A]): MayFail[(V,Store[A,V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = arity match {
          case Some(a) if argsWithExps.length == a => argsWithExps // .map(_._2)
          case None => return MayFail.failure(PrimitiveArityError(name, arity.getOrElse(-1), argsWithExps.length))
        }
        // for every call, keep track of the arguments
        // keep track of results for "visited" arguments
        var cache = Map[Args,MayFail[V,Error]]().withDefaultValue(mfMon.zero)
        // keep track of which calls depend on which other calls, independently of the position of the calls!
        var deps = Map[Args,Set[Args]]().withDefaultValue(Set())
        // standard worklist algorithm
        var worklist = Set(initArgs)
        //var curStore = store // MUTABLE STORE (UNNECESSARY)
        while (worklist.nonEmpty) {
          // take the next arguments from the worklist
          val nextArgs = worklist.head
          worklist = worklist - nextArgs
          // call with the next arguments
          val res = callWithArgs(fpos, cpos, nextArgs, store, args => {
            deps += (args -> (deps(args) + nextArgs))
            if (cache.get(args).isEmpty) worklist = worklist + args
            cache(args)
          }, alloc).map(_._1) // safely drop the store (it is a mutable store with ModF)
          // update the cache, worklist and store
          val oldValue = cache(nextArgs)
          val updatedValue = mfMon.append(oldValue, res)
          //val updatedValue = mfMon.append(oldValue, res >>= {case (vl, store) => curStore = store ; vl}) // MUTABLE STORE (REPLACED)
          if (updatedValue != oldValue) {
            cache += (nextArgs -> updatedValue)
            worklist ++= deps(nextArgs)
          }
        }
        cache(initArgs).map((_, store))
        //cache(initArgs).map((_, curStore)) // MUTABLE STORE (REPLACED)
      }
    }


    def liftTailRec(x: MayFail[TailRec[MayFail[V, Error]], Error]): TailRec[MayFail[V, Error]] =
      x match {
        case MayFailSuccess(v)   => tailcall(v)
        case MayFailError(err)   => done(MayFailError(err))
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
          } yield join(accv, res)
      )
    def dereferencePointerGetAddressReturnStore(x: V, store: Store[A, V])(
        f: (A, V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]
    ): MayFail[(V, Store[A, V]), Error] =
      getPointerAddresses(x).foldLeft(MayFail.success[(V, Store[A, V]), Error]((bottom, store)))(
        (acc: MayFail[(V, Store[A, V]), Error], a: A) =>
          acc >>= ({
            case (accv, updatedStore) =>
              /* We use the old store because the new added information can only negatively influence precision (as it didn't hold at the point of the function call */
              store.lookupMF(a) >>= (
                  v =>
                    /* But we pass the updated store around as it should reflect all updates */
                    f(a, v, updatedStore) >>= ({
                      case (res, newStore) =>
                        MayFail.success((join(accv, res), newStore))
                    })
                )
          })
      )

    def dereferencePointerTR(x: V, store: Store[A, V])(
        f: V => TailRec[MayFail[V, Error]]
    ): TailRec[MayFail[V, Error]] =
      getPointerAddresses(x).foldLeft(done(MayFail.success[V, Error](bottom)))(
        (acc: TailRec[MayFail[V, Error]], a: A) =>
          acc.flatMap(
            accv =>
              liftTailRec(store.lookupMF(a).map(f))
                .flatMap(fv => done(fv.flatMap(res => accv.flatMap(accvv => join(accvv, res)))))
          )
      )

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

    def ifThenElse(
        cond: MayFail[V, Error]
    )(thenBranch: => MayFail[V, Error])(elseBranch: => MayFail[V, Error]): MayFail[V, Error] = {
      cond >>= { condv =>
        val t = if (isTrue(condv)) {
          thenBranch
        } else {
          MayFail.success[V, Error](latMon.zero)
        }
        val f = if (isFalse(condv)) {
          elseBranch
        } else {
          MayFail.success[V, Error](latMon.zero)
        }
        mfMon.append(t, f)
      }
    }

    private trait Clause {
      def otherwise(act: => MayFail[V,Error]): MayFail[V,Error]
      def otherwise(cls: Clause): Clause = otherwise_(cls, this)
      def otherwise_(cls: Clause, parent: Clause) = new Clause {
        def otherwise(alt: => MayFail[V,Error]) = parent.otherwise(cls.otherwise(alt))
      }
    }

    private def ifV(prd: => MayFail[V,Error])(csq: => MayFail[V,Error]) = new Clause {
      def otherwise(alt: => MayFail[V,Error]) = ifThenElse(prd) { csq } { alt }
    }

    def ifThenElseTR(cond: MayFail[V, Error])(
        thenBranch: => TailRec[MayFail[V, Error]]
    )(elseBranch: => TailRec[MayFail[V, Error]]): TailRec[MayFail[V, Error]] = {
      val latMon = scalaam.util.MonoidInstances.latticeMonoid[V]
      val mfMon  = scalaam.util.MonoidInstances.mayFail[V](latMon)
      liftTailRec(cond >>= { condv =>
        val t = if (isTrue(condv)) {
          thenBranch
        } else {
          done(MayFail.success[V, Error](latMon.zero))
        }
        val f = if (isFalse(condv)) {
          elseBranch
        } else {
          done(MayFail.success[V, Error](latMon.zero))
        }
        t.flatMap(tval => f.map(fval => mfMon.append(tval, fval)))
      })
    }

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
    def lat_ceiling        = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Ceiling) _
    def lat_floor          = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Floor) _
    def lat_round          = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Round) _
    def lat_log            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Log) _
    def lat_not            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Not) _
    def lat_random         = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Random) _
    def lat_sin            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Sin) _
    def lat_asin           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ASin) _
    def lat_cos            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Cos) _
    def lat_acos           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ACos) _
    def lat_tan            = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Tan) _
    def lat_atan           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ATan) _
    def lat_sqrt           = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Sqrt) _
    def vectorLength   = schemeLattice.unaryOp(SchemeOps.UnaryOperator.VectorLength) _
    def stringLength   = schemeLattice.unaryOp(SchemeOps.UnaryOperator.StringLength) _
    def numberToString = schemeLattice.unaryOp(SchemeOps.UnaryOperator.NumberToString) _
    def symbolToString = schemeLattice.unaryOp(SchemeOps.UnaryOperator.SymbolToString) _
    def stringToSymbol = schemeLattice.unaryOp(SchemeOps.UnaryOperator.StringToSymbol) _
    def inexactToExact = schemeLattice.unaryOp(SchemeOps.UnaryOperator.InexactToExact) _
    def exactToInexact = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ExactToInexact) _
    def characterToInt = schemeLattice.unaryOp(SchemeOps.UnaryOperator.CharacterToInteger) _

    def plus         = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Plus) _
    def minus        = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Minus) _
    def times        = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Times) _
    def div          = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Div) _
    def lat_quotient     = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Quotient) _
    def lat_modulo       = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Modulo) _
    def lat_remainder    = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Remainder) _
    def lt           = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Lt) _
    def numEq        = schemeLattice.binaryOp(SchemeOps.BinaryOperator.NumEq) _
    def eqq          = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Eq) _
    def stringAppend = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringAppend) _
    def stringRef    = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringRef) _
    def stringLt     = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringLt) _

    //// MANUAL PRIMITIVES /////

    object `+` extends NoStoreOperation("+") {
      override def call(args: List[V]) = args match {
        case Nil       => number(0)
        case x :: rest => call(rest) >>= (plus(x, _))
      }
    }
    object `-` extends NoStoreOperation("-") {
      override def call(args: List[V]) = args match {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: Nil  => minus(number(0), x)
        case x :: rest => `+`.call(rest) >>= (minus(x, _))
      }
    }
    object `*` extends NoStoreOperation("*") {
      override def call(args: List[V]) = args match {
        case Nil       => number(1)
        case x :: rest => call(rest) >>= (times(x, _))
      }
    }
    object `/` extends NoStoreOperation("/") {
      override def call(args: List[V]) = args match {
        case Nil => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: rest =>
          for {
            multrest      <- `*`.call(rest)
            r             <- div(x, multrest)
            fl            <- lat_floor(r)
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
    object `quotient` extends NoStoreOperation("quotient", Some(2)) {
      override def call2(x: V, y: V) = lat_quotient(x, y)
    }

    object `<` extends NoStoreOperation("<", Some(2)) {
      override def call2(x: V, y: V) =
        lt(x, y) /* TODO[easy]: < should accept any number of arguments (same for <= etc.) */
    }

    object `=` extends NoStoreOperation("=", Some(2)) {
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

    object `modulo` extends NoStoreOperation("modulo", Some(2)) {
      override def call2(x: V, y: V) = lat_modulo(x, y)
    }
    object `remainder` extends NoStoreOperation("remainder", Some(2)) {
      override def call2(x: V, y: V) = lat_remainder(x, y)
    }
    object `random` extends NoStoreOperation("random", Some(1)) {
      override def call(x: V) = lat_random(x)
    }
    object `ceiling` extends NoStoreOperation("ceiling", Some(1)) {
      override def call(x: V) = lat_ceiling(x)
    }
    object `floor` extends NoStoreOperation("floor", Some(1)) {
      override def call(x: V) = lat_floor(x)
    }
    object `round` extends NoStoreOperation("round", Some(1)) {
      override def call(x: V) = lat_round(x)
    }
    object `log` extends NoStoreOperation("log", Some(1)) {
      override def call(x: V) = lat_log(x)
    }
    object `sin` extends NoStoreOperation("sin", Some(1)) {
      override def call(x: V) = lat_sin(x)
    }
    object `asin` extends NoStoreOperation("asin", Some(1)) {
      override def call(x: V) = lat_asin(x)
    }
    object `cos` extends NoStoreOperation("cos", Some(1)) {
      override def call(x: V) = lat_cos(x)
    }
    object `acos` extends NoStoreOperation("acos", Some(1)) {
      override def call(x: V) = lat_acos(x)
    }
    object `tan` extends NoStoreOperation("tan", Some(1)) {
      override def call(x: V) = lat_tan(x)
    }
    object `atan` extends NoStoreOperation("atan", Some(1)) {
      override def call(x: V) = lat_atan(x)
    }
    object `sqrt` extends NoStoreOperation("sqrt", Some(1)) {
      override def call(x: V) = ??? // TODO
      /*
        ifThenElse(`<=`.call2(number(0), x)) {
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
        } */
    }
    object `exact->inexact` extends NoStoreOperation("exact->inexact", Some(1)) {
      override def call(x: V) = exactToInexact(x)
    }
    object `inexact->exact` extends NoStoreOperation("inexact->exact", Some(1)) {
      override def call(x: V) = inexactToExact(x)
    }
    object `char->integer` extends NoStoreOperation("char->integer", Some(1)) {
      override def call(x: V) = characterToInt(x)
    }
    object `null?` extends NoStoreOperation("null?", Some(1)) {
      override def call(x: V) = isNull(x)
    }
    object `pair?` extends StoreOperation("pair?", Some(1)) {
      /* TODO[easy]: this should be a store operation that dereferences the pointer to check if it is indeed a cons */
      override def call(x: V, store: Store[A, V]) =
        (ifThenElse(isPointer(x)) {
          dereferencePointer(x, store) { v =>
            isCons(v)
          }
        } {
          bool(false)
        }).map(v => (v, store))
    }
    object `char?` extends NoStoreOperation("char?", Some(1)) {
      override def call(x: V) = isChar(x)
    }
    object `symbol?` extends NoStoreOperation("symbol?", Some(1)) {
      override def call(x: V) = isSymbol(x)
    }
    object `string?` extends NoStoreOperation("string?", Some(1)) {
      override def call(x: V) = isString(x)
    }
    object `integer?` extends NoStoreOperation("integer?", Some(1)) {
      override def call(x: V) = isInteger(x)
    }
    object `real?` extends NoStoreOperation("real?", Some(1)) {
      override def call(x: V) =
        for {
          isint  <- isInteger(x)
          isreal <- isReal(x)
        } yield or(isint, isreal)
    }
    object `number?` extends NoStoreOperation("number?", Some(1)) {
      override def call(x: V) =
        `real?`.call(x) /* No support for complex number, so number? is equivalent as real? */
    }
    object `boolean?` extends NoStoreOperation("boolean?", Some(1)) {
      override def call(x: V) = isBoolean(x)
    }
    object `vector?` extends StoreOperation("vector?", Some(1)) {
      override def call(x: V, store: Store[A, V]) =
        for {
          ispointer <- isPointer(x)
          isvector <- dereferencePointer(x, store) { v =>
            isVector(v)
          }
        } yield (and(ispointer, isvector), store)
    }
    object `eq?` extends NoStoreOperation("eq?", Some(2)) {
      override def call2(x: V, y: V) = eqq(x, y)
    }

    object `number->string` extends NoStoreOperation("number->string", Some(1)) {
      override def call(x: V) = numberToString(x)
    }
    object `symbol->string` extends NoStoreOperation("symbol->string", Some(1)) {
      override def call(x: V) = symbolToString(x)
    }
    object `string->symbol` extends NoStoreOperation("string->symbol", Some(1)) {
      override def call(x: V) = stringToSymbol(x)
    }
    object `string-append` extends NoStoreOperation("string-append") {
      override def call(args: List[V]) = args match {
        case Nil       => string("")
        case x :: rest => call(rest) >>= (stringAppend(x, _))
      }
    }
    object `string-ref` extends NoStoreOperation("string-ref") {
      override def call(args: List[V]) = args match {
        case s :: n :: Nil => stringRef(s, n)
        case l             => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }
    object `string<?` extends NoStoreOperation("string<?", Some(2)) {
      override def call2(x: V, y: V) = stringLt(x, y)
    }
    object `string-length` extends NoStoreOperation("string-length", Some(1)) {
      override def call(x: V) = stringLength(x)
    }

    object `error` extends NoStoreOperation("error", Some(1)) {
      override def call1pos(fpos: Identity.Position, x: (Identity.Position, V)) =
        MayFail.failure(UserError(x._2.toString))
    }

    object `cons` extends SchemePrimitive[V,A] {
      val name = "cons"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val consa = alloc.pointer((fpos, cpos))
          (pointer(consa), store.extend(consa, schemeLattice.cons(car, cdr)))
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
        .reverseIterator
        .map(
          c =>
            if (c == 'a') {
              Car
            } else if (c == 'd') {
              Cdr
            } else {
              throw new Exception(s"Incorrect car/cdr operation: $name")
            }
        ).toList
      override def call(v: V, store: Store[A, V]) =
        for {
          v <- spec.foldLeft(MayFail.success[V, Error](v))(
            (acc, op) =>
              for {
                v <- acc
                res <- dereferencePointer(v, store) { consv =>
                  op match {
                    case Car => schemeLattice.car(consv)
                    case Cdr => schemeLattice.cdr(consv)
                  }
                }
              } yield res
          )
        } yield (v, store)
    }

    object `car`    extends CarCdrOperation("car") // MANUAL
    object `cdr`    extends  CarCdrOperation("cdr") // MANUAL

        object `set-car!` extends StoreOperation("set-car!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1 = value /* update car */
                v2 <- schemeLattice.cdr(consv) /* preserves cdr */
              } yield st.update(a, schemeLattice.cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
    }
    object `set-cdr!` extends StoreOperation("set-cdr!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1    <- schemeLattice.car(consv) /* preserves car */
                v2 = value /* update cdr */
              } yield st.update(a, schemeLattice.cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
    }


    object `make-vector` extends SchemePrimitive[V,A] {
      val name = "make-vector"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        def createVec(size: V, init: V): MayFail[(V, Store[A, V]), Error] = {
          isInteger(size) >>= (
              isint =>
                if (isTrue(isint)) {
                  val veca = alloc.pointer((fpos, cpos))
                  schemeLattice.vector(size, init) >>= (vec => (pointer(veca), store.extend(veca, vec)))
                } else {
                  MayFail.failure(PrimitiveNotApplicable(name, args.map(_._2)))
                }
            )
        }
        args match {
          case (_, size) :: Nil              => createVec(size, /* XXX: unspecified */ bool(false))
          case (_, size) :: (_, init) :: Nil => createVec(size, init)
          case l                             => MayFail.failure(PrimitiveVariadicArityError(name, 1, l.size))
        }
      }
    }

    object `vector` extends SchemePrimitive[V,A] {
      val name = "vector"
      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        val veca = alloc.pointer((fpos, cpos))
        schemeLattice.vector(number(args.size), bottom) >>= (
            emptyvec =>
              args.zipWithIndex.foldLeft(MayFail.success[V, Error](emptyvec))(
                (acc, arg) =>
                  acc >>= (
                      vec =>
                        arg match {
                          case ((_, value), index) =>
                            vectorSet(vec, number(index), value)
                        }
                    )
              )
          ) >>= (vec => (pointer(veca), store.extend(veca, vec)))
      }
    }

    object `vector-length` extends StoreOperation("vector-length", Some(1)) {
      def length(v: V, store: Store[A, V]): MayFail[V, Error] = {
        dereferencePointer(v, store) { vec =>
          ifThenElse(isVector(vec)) {
            vectorLength(vec)
          } {
            MayFail.failure(PrimitiveNotApplicable(name, List(v)))
          }
        }
      }
      override def call(v: V, store: Store[A, V]) = {
        length(v, store).map(v => (v, store))
      }
    }

    object `vector-ref` extends StoreOperation("vector-ref", Some(2)) {
      def vectorRef(v: V, index: V, store: Store[A, V]): MayFail[V, Error] = {
        dereferencePointer(v, store) { vec =>
          ifThenElse(isVector(vec)) {
            schemeLattice.vectorRef(vec, index)
          } {
            MayFail.failure(PrimitiveNotApplicable(name, List(v, index)))
          }
        }
      }
      override def call(v: V, index: V, store: Store[A, V]) =
        vectorRef(v, index, store).map(v => (v, store))
    }

    object `vector-set!` extends StoreOperation("vector-set!", Some(3)) {
      def vectorSet(
          v: V,
          index: V,
          newval: V,
          store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] = {
        dereferencePointerGetAddressReturnStore(v, store) {
          case (veca, vec, store) =>
            isVector(vec) >>= (test => {
              val t: MayFail[(V, Option[(A, V)]), Error] =
                if (isTrue(test)) {
                  schemeLattice.vectorSet(vec, index, newval) >>= (
                      newvec =>
                        MayFail.success(( /* unspecified */ bool(false), Some((veca, newvec))))
                    )
                } else {
                  MayFail.success((bottom, None))
                }
              val f: MayFail[V, Error] =
                if (isFalse(test)) {
                  MayFail.failure(PrimitiveNotApplicable(name, List(v, index, newval)))
                } else {
                  MayFail.success(bottom)
                }
              t >>= ({
                case (tv, None) => f.join(MayFail.success(tv), join).map(v => (v, store))
                case (tv, Some((a, va))) =>
                  f.join(MayFail.success(tv), join).map(v => (v, store.update(a, va)))
              })
            })
        }
      }
      override def call(args: List[V], store: Store[A, V]) = args match {
        case v :: index :: newval :: Nil => vectorSet(v, index, newval, store)
        case _                           => MayFail.failure(PrimitiveArityError(name, 3, args.size))
      }
    }
    object `list` extends StoreOperation("list", None) {
      override def call(fpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) =
        args match {
          case Nil => (nil, store)
          case (exp, v) :: rest =>
            for {
              (restv, store2) <- call(fpos, rest, store, alloc)
              consv  = schemeLattice.cons(v, restv)
              consa  = alloc.pointer((exp, fpos))
              store3 = store2.extend(consa, consv)
            } yield (pointer(consa), store3)
        }
    }


    // PRIMITIVES THAT WE COULD PROBABLY COMPILE
    object `expt` extends NoStoreOperation("expt") {
      def expt(x: V, y: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
        if (visited.contains(y) || x == bottom || y == bottom) {
          done(bottom)
        } else {
          ifThenElseTR(numEq(y, number(0))) {
            done(number(1))
          } {
            liftTailRec(
              minus(y, number(1)).flatMap(
                y1 =>
                  tailcall(expt(x, y1, visited + y))
                    .flatMap(exptrest => done(exptrest.flatMap(y => times(x, y))))
              )
            )
          }
        }
      }
      override def call2(x: V, y: V) = expt(x, y, Set()).result
    }


 // COMPILED PRIMITIVES

object `<=` extends StoreOperation("<=", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
val `y_pos` = args(1)._1
val `y` = args(1)._2
    `<`.call(fpos, (-1,-23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(true)
      }
      {
        `=`.call(fpos, (-1,-31), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("<=", 2, args.length))
}

object `>=` extends StoreOperation(">=", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
val `y_pos` = args(1)._1
val `y` = args(1)._2
    `>`.call(fpos, (-1,-23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(true)
      }
      {
        `=`.call(fpos, (-1,-31), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError(">=", 2, args.length))
}

object `>` extends StoreOperation(">", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
val `y_pos` = args(1)._1
val `y` = args(1)._2
    `<=`.call(fpos, (-1,-23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0`  =>
      `not`.call(fpos, (-1,-18), List(((-1,-23), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError(">", 2, args.length))
}

object `zero?` extends StoreOperation("zero?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `=`.call(fpos, (-1,-20), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("zero?", 1, args.length))
}

object `positive?` extends StoreOperation("positive?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `>`.call(fpos, (-1,-24), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("positive?", 1, args.length))
}

object `negative?` extends StoreOperation("negative?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `<`.call(fpos, (-1,-24), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("negative?", 1, args.length))
}

object `odd?` extends StoreOperation("odd?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `modulo`.call(fpos, (-1,-24), List((`x_pos`, `x`), ((0, 0), number(2))), store, alloc).map(_._1) >>= { `_0`  =>
      `=`.call(fpos, (-1,-19), List(((0, 0), number(1)), ((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("odd?", 1, args.length))
}

object `even?` extends StoreOperation("even?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `modulo`.call(fpos, (-1,-25), List((`x_pos`, `x`), ((0, 0), number(2))), store, alloc).map(_._1) >>= { `_0`  =>
      `=`.call(fpos, (-1,-20), List(((0, 0), number(0)), ((-1,-25), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("even?", 1, args.length))
}

object `max` extends StoreOperation("max", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `a_pos` = args(0)._1
val `a` = args(0)._2
val `b_pos` = args(1)._1
val `b` = args(1)._2
    `<`.call(fpos, (-1,-24), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `b`
      }
      {
        `a`
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("max", 2, args.length))
}

object `min` extends StoreOperation("min", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `a_pos` = args(0)._1
val `a` = args(0)._2
val `b_pos` = args(1)._1
val `b` = args(1)._2
    `<`.call(fpos, (-1,-24), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `a`
      }
      {
        `b`
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("min", 2, args.length))
}

object `abs` extends StoreOperation("abs", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `<`.call(fpos, (-1,-22), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `-`.call(fpos, (-1,-30), List(((0, 0), number(0)), (`x_pos`, `x`)), store, alloc).map(_._1)
      }
      {
        `x`
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("abs", 1, args.length))
}

object `gcd` extends SimpleFixpointPrimitiveUsingStore("gcd", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `a_pos` = args(0)._1
val `a` = args(0)._2
val `b_pos` = args(1)._1
val `b` = args(1)._2
    `=`.call(fpos, (-1,-24), List((`b_pos`, `b`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `a`
      }
      {
        `modulo`.call(fpos, (-1,-41), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_1`  =>
          recursiveCall(List((`b_pos`, `b`), ((-1,-41), `_1`)))
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("gcd", 2, args.length))
}

object `lcm` extends StoreOperation("lcm", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `m_pos` = args(0)._1
val `m` = args(0)._2
val `n_pos` = args(1)._1
val `n` = args(1)._2
    `*`.call(fpos, (-1,-28), List((`m_pos`, `m`), (`n_pos`, `n`)), store, alloc).map(_._1) >>= { `_0`  =>
      `abs`.call(fpos, (-1,-23), List(((-1,-28), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `gcd`.call(fpos, (-1,-37), List((`m_pos`, `m`), (`n_pos`, `n`)), store, alloc).map(_._1) >>= { `_2`  =>
          `/`.call(fpos, (-1,-20), List(((-1,-23), `_1`), ((-1,-37), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("lcm", 2, args.length))
}

object `not` extends StoreOperation("not", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    ifThenElse(`x`)
    {
      bool(false)
    }
    {
      bool(true)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("not", 1, args.length))
}

object `newline` extends StoreOperation("newline", Some(0)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 0) {
      {     bool(false) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("newline", 0, args.length))
}

object `display` extends StoreOperation("display", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `x` }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("display", 1, args.length))
}

object `caar` extends StoreOperation("caar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caar", 1, args.length))
}

object `cadr` extends StoreOperation("cadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadr", 1, args.length))
}

object `cddr` extends StoreOperation("cddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddr", 1, args.length))
}

object `cdar` extends StoreOperation("cdar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdar", 1, args.length))
}

object `caaar` extends StoreOperation("caaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaar", 1, args.length))
}

object `caadr` extends StoreOperation("caadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caadr", 1, args.length))
}

object `cadar` extends StoreOperation("cadar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadar", 1, args.length))
}

object `caddr` extends StoreOperation("caddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caddr", 1, args.length))
}

object `cdaar` extends StoreOperation("cdaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaar", 1, args.length))
}

object `cdadr` extends StoreOperation("cdadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdadr", 1, args.length))
}

object `cddar` extends StoreOperation("cddar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddar", 1, args.length))
}

object `cdddr` extends StoreOperation("cdddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdddr", 1, args.length))
}

object `caaaar` extends StoreOperation("caaaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaaar", 1, args.length))
}

object `caaadr` extends StoreOperation("caaadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaadr", 1, args.length))
}

object `caadar` extends StoreOperation("caadar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caadar", 1, args.length))
}

object `caaddr` extends StoreOperation("caaddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaddr", 1, args.length))
}

object `cadaar` extends StoreOperation("cadaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadaar", 1, args.length))
}

object `cadadr` extends StoreOperation("cadadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadadr", 1, args.length))
}

object `caddar` extends StoreOperation("caddar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caddar", 1, args.length))
}

object `cadddr` extends StoreOperation("cadddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadddr", 1, args.length))
}

object `cdaaar` extends StoreOperation("cdaaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaaar", 1, args.length))
}

object `cdaadr` extends StoreOperation("cdaadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaadr", 1, args.length))
}

object `cdadar` extends StoreOperation("cdadar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdadar", 1, args.length))
}

object `cdaddr` extends StoreOperation("cdaddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaddr", 1, args.length))
}

object `cddaar` extends StoreOperation("cddaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddaar", 1, args.length))
}

object `cddadr` extends StoreOperation("cddadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddadr", 1, args.length))
}

object `cdddar` extends StoreOperation("cdddar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdddar", 1, args.length))
}

object `cddddr` extends StoreOperation("cddddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `x_pos` = args(0)._1
val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddddr", 1, args.length))
}

object `equal?` extends SimpleFixpointPrimitiveUsingStore("equal?", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `a_pos` = args(0)._1
val `a` = args(0)._2
val `b_pos` = args(1)._1
val `b` = args(1)._2
    `eq?`.call(fpos, (-2,-16), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(true)
      }
      {
        `null?`.call(fpos, (-3,-19), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_1`  =>
          ifThenElse(`_1`)
          {
            `null?`.call(fpos, (-3,-29), List((`b_pos`, `b`)), store, alloc).map(_._1)
          }
          {
            bool(false)
          } >>= { `_2`  =>
            ifThenElse(`_2`)
            {
              bool(true)
            }
            {
              `pair?`.call(fpos, (-4,-19), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_3`  =>
                ifThenElse(`_3`)
                {
                  `pair?`.call(fpos, (-4,-29), List((`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_4`  =>
                    ifThenElse(`_4`)
                    {
                      `car`.call(fpos, (-4,-47), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_5`  =>
                        `car`.call(fpos, (-4,-55), List((`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_6`  =>
                          recursiveCall(List(((-4,-47), `_5`), ((-4,-55), `_6`))) >>= { `_7`  =>
                            ifThenElse(`_7`)
                            {
                              `cdr`.call(fpos, (-4,-72), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_8`  =>
                                `cdr`.call(fpos, (-4,-80), List((`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_9`  =>
                                  recursiveCall(List(((-4,-72), `_8`), ((-4,-80), `_9`)))
                                }
                              }
                            }
                            {
                              bool(false)
                            }
                          }
                        }
                      }
                    }
                    {
                      bool(false)
                    }
                  }
                }
                {
                  bool(false)
                }
              }
            }
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("equal?", 2, args.length))
}

object `list?` extends SimpleFixpointPrimitiveUsingStore("list?", Some(1)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `l_pos` = args(0)._1
val `l` = args(0)._2
    `pair?`.call(fpos, (-1,-29), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `cdr`.call(fpos, (-1,-46), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          recursiveCall(List(((-1,-46), `_1`)))
        }
      }
      {
        bool(false)
      } >>= { `_2`  =>
        ifThenElse(`_2`)
        {
          bool(true)
        }
        {
          `null?`.call(fpos, (-1,-56), List((`l_pos`, `l`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("list?", 1, args.length))
}

object `list-ref` extends SimpleFixpointPrimitiveUsingStore("list-ref", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `l_pos` = args(0)._1
val `l` = args(0)._2
val `index_pos` = args(1)._1
val `index` = args(1)._2
    `=`.call(fpos, (-2,-18), List((`index_pos`, `index`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `car`.call(fpos, (-3,-16), List((`l_pos`, `l`)), store, alloc).map(_._1)
      }
      {
        `cdr`.call(fpos, (-4,-26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          `-`.call(fpos, (-4,-34), List((`index_pos`, `index`), ((0, 0), number(1))), store, alloc).map(_._1) >>= { `_2`  =>
            recursiveCall(List(((-4,-26), `_1`), ((-4,-34), `_2`)))
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("list-ref", 2, args.length))
}

object `member` extends SimpleFixpointPrimitiveUsingStore("member", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `e_pos` = args(0)._1
val `e` = args(0)._2
val `l_pos` = args(1)._1
val `l` = args(1)._2
    `null?`.call(fpos, (-2,-16), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(false)
      }
      {
        `car`.call(fpos, (-4,-26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          `equal?`.call(fpos, (-4,-18), List(((-4,-26), `_1`), (`e_pos`, `e`)), store, alloc).map(_._1) >>= { `_2`  =>
            ifThenElse(`_2`)
            {
              `l`
            }
            {
              `cdr`.call(fpos, (-6,-26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3`  =>
                recursiveCall(List((`e_pos`, `e`), ((-6,-26), `_3`)))
              }
            }
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("member", 2, args.length))
}

object `memq` extends SimpleFixpointPrimitiveUsingStore("memq", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `e_pos` = args(0)._1
val `e` = args(0)._2
val `l_pos` = args(1)._1
val `l` = args(1)._2
    `null?`.call(fpos, (-2,-16), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(false)
      }
      {
        `car`.call(fpos, (-4,-23), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          `eq?`.call(fpos, (-4,-18), List(((-4,-23), `_1`), (`e_pos`, `e`)), store, alloc).map(_._1) >>= { `_2`  =>
            ifThenElse(`_2`)
            {
              `l`
            }
            {
              `cdr`.call(fpos, (-6,-24), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3`  =>
                recursiveCall(List((`e_pos`, `e`), ((-6,-24), `_3`)))
              }
            }
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("memq", 2, args.length))
}

object `assoc` extends SimpleFixpointPrimitiveUsingStore("assoc", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `k_pos` = args(0)._1
val `k` = args(0)._2
val `l_pos` = args(1)._1
val `l` = args(1)._2
    `null?`.call(fpos, (-2,-14), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(false)
      }
      {
        `caar`.call(fpos, (-4,-23), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          `equal?`.call(fpos, (-4,-15), List(((-4,-23), `_1`), (`k_pos`, `k`)), store, alloc).map(_._1) >>= { `_2`  =>
            ifThenElse(`_2`)
            {
              `car`.call(fpos, (-5,-13), List((`l_pos`, `l`)), store, alloc).map(_._1)
            }
            {
              `cdr`.call(fpos, (-6,-22), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3`  =>
                recursiveCall(List((`k_pos`, `k`), ((-6,-22), `_3`)))
              }
            }
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("assoc", 2, args.length))
}

object `assq` extends SimpleFixpointPrimitiveUsingStore("assq", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `k_pos` = args(0)._1
val `k` = args(0)._2
val `l_pos` = args(1)._1
val `l` = args(1)._2
    `null?`.call(fpos, (-2,-14), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(false)
      }
      {
        `caar`.call(fpos, (-4,-20), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          `eq?`.call(fpos, (-4,-15), List(((-4,-20), `_1`), (`k_pos`, `k`)), store, alloc).map(_._1) >>= { `_2`  =>
            ifThenElse(`_2`)
            {
              `car`.call(fpos, (-5,-13), List((`l_pos`, `l`)), store, alloc).map(_._1)
            }
            {
              `cdr`.call(fpos, (-6,-21), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3`  =>
                recursiveCall(List((`k_pos`, `k`), ((-6,-21), `_3`)))
              }
            }
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("assq", 2, args.length))
}

    object `list-tail` extends SimpleFixpointPrimitiveUsingStore("list-tail", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          { val `x_pos` = args(0)._1
            val `x` = args(0)._2
            val `k_pos` = args(1)._1
            val `k` = args(1)._2
            `zero?`.call(fpos, (-2,-10), List((`k_pos`, `k`)), store, alloc).map(_._1) >>= { `_0`  =>
              ifThenElse(`_0`)
              {
                `x`
              }
              {
                `cdr`.call(fpos, (-4,-21), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_1`  =>
                  `-`.call(fpos, (-4,-29), List((`k_pos`, `k`), ((0, 0), number(1))), store, alloc).map(_._1) >>= { `_2`  =>
                    recursiveCall(List(((-4,-21), `_1`), ((-4,-29), `_2`)))
                  }
                }
              }
            } }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("list-tail", 2, args.length))
    }

object `length` extends SimpleFixpointPrimitiveUsingStore("length", Some(1)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `l_pos` = args(0)._1
val `l` = args(0)._2
    `null?`.call(fpos, (-2,-16), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        number(0)
      }
      {
        `cdr`.call(fpos, (-4,-29), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          recursiveCall(List(((-4,-29), `_1`))) >>= { `_2`  =>
            `+`.call(fpos, (-4,-16), List(((0, 0), number(1)), ((-4,-21), `_2`)), store, alloc).map(_._1)
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("length", 1, args.length))
}

object `append` extends SimpleFixpointPrimitiveUsingStore("append", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      { val `l1_pos` = args(0)._1
val `l1` = args(0)._2
val `l2_pos` = args(1)._1
val `l2` = args(1)._2
    `null?`.call(fpos, (-2,-16), List((`l1_pos`, `l1`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `l2`
      }
      {
        `car`.call(fpos, (-4,-22), List((`l1_pos`, `l1`)), store, alloc).map(_._1) >>= { `_1`  =>
          `cdr`.call(fpos, (-5,-30), List((`l1_pos`, `l1`)), store, alloc).map(_._1) >>= { `_2`  =>
            recursiveCall(List(((-5,-30), `_2`), (`l2_pos`, `l2`))) >>= { `_3`  =>
              `cons`.call(fpos, (-4,-16), List(((-4,-22), `_1`), ((-5,-22), `_3`)), store, alloc).map(_._1)
            }
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("append", 2, args.length))
}

object `reverse` extends SimpleFixpointPrimitiveUsingStore("reverse", Some(1)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      { val `l_pos` = args(0)._1
val `l` = args(0)._2
    `null?`.call(fpos, (-2,-9), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `nil`
      }
      {
        `cdr`.call(fpos, (-4,-26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1`  =>
          recursiveCall(List(((-4,-26), `_1`))) >>= { `_2`  =>
            `car`.call(fpos, (-5,-23), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3`  =>
              `list`.call(fpos, (-5,-17), List(((-5,-23), `_3`)), store, alloc).map(_._1) >>= { `_4`  =>
                `append`.call(fpos, (-4,-9), List(((-4,-17), `_2`), ((-5,-17), `_4`)), store, alloc).map(_._1)
              }
            }
          }
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("reverse", 1, args.length))
}


  }
}


