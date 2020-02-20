class CompiledSchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) {

  import SchemeLattice.scala

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

  object PrimitiveDefs extends PrimitiveBase {

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