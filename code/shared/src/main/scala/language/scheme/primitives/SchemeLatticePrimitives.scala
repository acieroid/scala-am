package scalaam.language.scheme.primitives

import scalaam.core.{Address, Error, Identity, MayFail, Store}
import scalaam.language.scheme.SchemeLattice

class SchemeLatticePrimitives[V, A <: Address](override implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) extends SchemePrimitives[V, A] {
  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def allPrimitives: List[SchemePrimitive[V,A]] = {
    import PrimitiveDefs._
    List(
      `modulo`,
      `*`, /* [vv] *: Arithmetic */
      `+`, /* [vv] +: Arithmetic */
      `-`, /* [vv] -: Arithmetic */
      `/`, /* [vx] /: Arithmetic (no support for fractions) */
      `acos`, /* [vv] acos: Scientific */
      /* [x]  angle: Complex */
      /* [x]  apply: Fly Evaluation */
      `asin`, /* [vv] asin: Scientific */
      /* [x]  assv: Retrieving Alist Entries */
      `atan`, /* [vv] atan: Scientific */
      `append`,
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
      /* [x]  dynamic-wind: Dynamic Wind */
      /* [x]  eof-object?: Reading */
      `eq?`, /* [vv] eq?: Equality */
      /* [x]  eqv?: Equality */
      /* [x]  eval: Fly Evaluation */
      `exact->inexact`, /* [vv] exact->inexact: Exactness */
      /* [x]  exact?: Exactness */
      /* [x]  exp: Scientific */
      `expt`, /* [vv] expt: Scientific */
      `floor`, /* [vv] floor: Arithmetic */
      /* [x]  for-each: List Mapping */
      /* [x]  force: Delayed Evaluation */
      /* [x]  imag-part: Complex */
      `inexact->exact`, /* [vv] inexact->exact: Exactness */
      /* [x]  inexact?: Exactness */
      /* [x]  input-port?: Ports */
      /* [x]  integer->char: Characters */
      `integer?`, /* [vv] integer?: Integers */
      /* [x]  interaction-environment: Fly Evaluation */
      /* [x]  lcm: Integer Operations */
      `list`, /* [vv] list: List Constructors */
      /* [x]  list->string: String Constructors */
      /* [x]  list->vector: Vector Creation */
      /* [x]  list-tail: List Selection */
      /* [x]  load: Loading */
      `log`, /* [vv] log: Scientific */
      /* [x]  magnitude: Complex */
      /* [x]  make-polar: Complex */
      /* [x]  make-rectangular: Complex */
      /* [x]  make-string: String Constructors */
      /* [x]  map: List Mapping */
      /* [x]  memv: List Searching */
      `null?`, /* [vv] null?: List Predicates */
      `number->string`, /* [vx] number->string: Conversion: does not support two arguments */
      `number?`, /* [vv] number?: Numerical Tower */
//      Oddp, /* [vv] odd?: Integer Operations */
      /* [x]  open-input-file: File Ports */
      /* [x]  open-output-file: File Ports */
      /* [x]  output-port?: Ports */
      `pair?`, /* [vv] pair?: Pairs */
      /* [x]  peek-char?: Reading */
//      Positivep, /* [vv] positive?: Comparison */
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
      `<`, /* [vv]  < */
      `=`, /* [vv]  = */
//      `>`, /* [vv]  > */
      /* [x]  numerator */
      /* [x]  denominator */
      /* [x]  rationalize-string */
      /* [x]  scheme-report-environment */
      /* [x]  null-environment */
      /* [x]  write transcript-on */
      /* [x]  transcript-off */
      /* Other primitives that are not R5RS */
      `random`,
      `error`
    )
  }

  //assert(PrimitiveDefinitions.names.intersect(allPrimitives.map(_.name).toSet).isEmpty)

  abstract class NoStore1Operation(val name: String) extends SchemePrimitive[V, A] {
    def call(x: V): MayFail[V, Error]
    override def call(fpos: Identity,
      args: List[(Identity, V)],
      store: Store[A, V],
      alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: Nil => call(x._2).map(v => (v, store))
      case _ => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  abstract class NoStore2Operation(val name: String) extends SchemePrimitive[V, A] {
    def call(x: V, y: V): MayFail[V, Error]
    override def call(fpos: Identity,
      args: List[(Identity, V)],
      store: Store[A, V],
      alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: Nil => call(x._2, y._2).map(v => (v, store))
      case _ => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  abstract class NoStoreLOperation(val name: String) extends SchemePrimitive[V, A] {
    def call(args: List[V]): MayFail[V, Error]
    override def call(fpos: Identity,
      args: List[(Identity, V)],
      store: Store[A, V],
      alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
      call(args.map(_._2)).map(v => (v, store))
  }

  abstract class Store1Operation(val name: String) extends SchemePrimitive[V, A] {
    def call(x: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error]

    override def call(fpos: Identity,
      args: List[(Identity, V)],
      store: Store[A, V],
      alloc: SchemeAllocator[A]
    ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: Nil => call(x._2, store)
      case _ => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  abstract class Store2Operation(val name: String) extends SchemePrimitive[V, A] {
    def call(arg1: V, arg2: V, store: Store[A, V]): MayFail[(V, Store[A, V]), Error]

    override def call(fpos: Identity,
      args: List[(Identity, V)],
      store: Store[A, V],
      alloc: SchemeAllocator[A]
    ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: Nil => call(x._2, y._2, store)
      case _             => MayFail.failure(PrimitiveArityError(name, 2, args.length))
    }
  }

  def dereferenceAddrs(addrs: Set[A], store: Store[A,V]): MayFail[V,Error] =
    addrs.foldLeft(MayFail.success[V,Error](schemeLattice.bottom))(
      (acc: MayFail[V,Error], addr: A) =>
        for {
          v <- store.lookupMF(addr)
          accv <- acc
        } yield schemeLattice.join(accv,v)
    )

  object PrimitiveDefs extends PrimitiveBuildingBlocks[V, A] {

    val lat: SchemeLattice[V, A, SchemePrimitive[V, A], _] = schemeLattice

    import schemeLattice._

    import scala.util.control.TailCalls._

    //// MANUAL PRIMITIVES /////

    object `+` extends NoStoreLOperation("+") {
      override def call(args: List[V]) = args match {
        case Nil       => number(0)
        case x :: rest => call(rest) >>= (plus(x, _))
      }
    }
    object `-` extends NoStoreLOperation("-") {
      override def call(args: List[V]) = args match {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: Nil  => minus(number(0), x)
        case x :: rest => `+`.call(rest) >>= (minus(x, _))
      }
    }
    object `*` extends NoStoreLOperation("*") {
      override def call(args: List[V]) = args match {
        case Nil       => number(1)
        case x :: rest => call(rest) >>= (times(x, _))
      }
    }
    object `/` extends NoStoreLOperation("/") {
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
    object `expt` extends NoStore2Operation("expt") {
      override def call(x: V, y: V) = lat_expt(x, y)
    }
    object `quotient` extends NoStore2Operation("quotient") {
      override def call(x: V, y: V) = lat_quotient(x, y)
    }

    object `<` extends NoStore2Operation("<") {
      override def call(x: V, y: V) =
        lt(x, y) /* TODO[easy]: < should accept any number of arguments (same for <= etc.) */
    }

    object `=` extends NoStoreLOperation("=") {
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

    object `modulo` extends NoStore2Operation("modulo") {
      override def call(x: V, y: V) = lat_modulo(x, y)
    }
    object `remainder` extends NoStore2Operation("remainder") {
      override def call(x: V, y: V) = lat_remainder(x, y)
    }
    object `random` extends NoStore1Operation("random") {
      override def call(x: V) = lat_random(x)
    }
    object `ceiling` extends NoStore1Operation("ceiling") {
      override def call(x: V) = lat_ceiling(x)
    }
    object `floor` extends NoStore1Operation("floor") {
      override def call(x: V) = lat_floor(x)
    }
    object `round` extends NoStore1Operation("round") {
      override def call(x: V) = lat_round(x)
    }
    object `log` extends NoStore1Operation("log") {
      override def call(x: V) = lat_log(x)
    }
    object `sin` extends NoStore1Operation("sin") {
      override def call(x: V) = lat_sin(x)
    }
    object `asin` extends NoStore1Operation("asin") {
      override def call(x: V) = lat_asin(x)
    }
    object `cos` extends NoStore1Operation("cos") {
      override def call(x: V) = lat_cos(x)
    }
    object `acos` extends NoStore1Operation("acos") {
      override def call(x: V) = lat_acos(x)
    }
    object `tan` extends NoStore1Operation("tan") {
      override def call(x: V) = lat_tan(x)
    }
    object `atan` extends NoStore1Operation("atan") {
      override def call(x: V) = lat_atan(x)
    }
    object `sqrt` extends NoStore1Operation("sqrt") {
      override def call(x: V) =
        ifThenElse(`<`.call(number(1), x)) {
          /* n >= 0 */
          for {
            r          <- lat_sqrt(x)
            fl         <- lat_floor(r)
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
    object `exact->inexact` extends NoStore1Operation("exact->inexact") {
      override def call(x: V) = exactToInexact(x)
    }
    object `inexact->exact` extends NoStore1Operation("inexact->exact") {
      override def call(x: V) = inexactToExact(x)
    }
    object `char->integer` extends NoStore1Operation("char->integer") {
      override def call(x: V) = characterToInt(x)
    }
    object `null?` extends NoStore1Operation("null?") {
      override def call(x: V) = isNull(x)
    }
    object `pair?` extends Store1Operation("pair?") {
      override def call(x: V, store: Store[A, V]) = isCons(x).map(v => (v, store))
    }
    object `char?` extends NoStore1Operation("char?") {
      override def call(x: V) = isChar(x)
    }
    object `symbol?` extends NoStore1Operation("symbol?") {
      override def call(x: V) = isSymbol(x)
    }
    object `string?` extends NoStore1Operation("string?") {
      override def call(x: V) = isString(x)
    }
    object `integer?` extends NoStore1Operation("integer?") {
      override def call(x: V) = isInteger(x)
    }
    object `real?` extends NoStore1Operation("real?") {
      override def call(x: V) =
        for {
          isint  <- isInteger(x)
          isreal <- isReal(x)
        } yield or(isint, isreal)
    }
    object `number?` extends NoStore1Operation("number?") {
      override def call(x: V) =
        `real?`.call(x) /* No support for complex number, so number? is equivalent as real? */
    }
    object `boolean?` extends NoStore1Operation("boolean?") {
      override def call(x: V) = isBoolean(x)
    }
    object `vector?` extends Store1Operation("vector?") {
      override def call(x: V, store: Store[A, V]) =
        for {
          ispointer <- isPointer(x)
          isvector <- dereferencePointer(x, store) { v =>
            isVector(v)
          }
        } yield (and(ispointer, isvector), store)
    }
    object `eq?` extends NoStore2Operation("eq?") {
      override def call(x: V, y: V) = eqq(x, y)
    }

    object `number->string` extends NoStore1Operation("number->string") {
      override def call(x: V) = numberToString(x)
    }
    object `symbol->string` extends NoStore1Operation("symbol->string") {
      override def call(x: V) = symbolToString(x)
    }
    object `string->symbol` extends NoStore1Operation("string->symbol") {
      override def call(x: V) = stringToSymbol(x)
    }
    object `string-append` extends NoStoreLOperation("string-append") {
      override def call(args: List[V]) = args match {
        case Nil       => string("")
        case x :: rest => call(rest) >>= (stringAppend(x, _))
      }
    }
    object `string-ref` extends NoStore2Operation("string-ref") {
      override def call(s: V, n: V) = stringRef(s, n)
    }
    object `string<?` extends NoStore2Operation("string<?") {
      override def call(x: V, y: V) = stringLt(x, y)
    }
    object `string-length` extends NoStore1Operation("string-length") {
      override def call(x: V) = stringLength(x)
    }

    object `error` extends NoStore1Operation("error") {
      override def call(x: V) =
        MayFail.failure(UserError(x.toString))
    }

    object `cons` extends SchemePrimitive[V,A] {
      val name = "cons"
      override def call(fpos: Identity, args: List[(Identity, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val carAddr = alloc.carAddr(fpos)
          val cdrAddr = alloc.cdrAddr(fpos)
          val consVal = lat.cons(carAddr, cdrAddr)
          (consVal, store.extend(carAddr, car).extend(cdrAddr, cdr))
        case l => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }

    class CarCdrOperation(override val name: String) extends Store1Operation(name) {
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
                consv <- acc
                addrs = op match {
                  case Car => lat.car(consv)
                  case Cdr => lat.cdr(consv)
                }
                res <- dereferenceAddrs(addrs, store)
              } yield res
          )
        } yield (v, store)
    }

    object `car`    extends CarCdrOperation("car") // MANUAL
    object `cdr`    extends  CarCdrOperation("cdr") // MANUAL

    object `set-car!` extends Store2Operation("set-car!") {
      override def call(cell: V, value: V, store: Store[A, V]) =
        lat.car(cell)
          .foldLeft(store)((acc,addr) => acc.update(addr, value))
          .map(store => (bool(false) /* undefined */, store))
    }

    object `set-cdr!` extends Store2Operation("set-cdr!") {
      override def call(cell: V, value: V, store: Store[A, V]) =
        lat.cdr(cell)
          .foldLeft(store)((acc,addr) => acc.update(addr, value))
          .map(store => (bool(false) /* undefined */, store))
    }

    object `make-vector` extends SchemePrimitive[V,A] {
      val name = "make-vector"
      override def call(fpos: Identity, args: List[(Identity, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        def createVec(size: V, init: V): MayFail[(V, Store[A, V]), Error] = {
          isInteger(size) >>= (
              isint =>
                if (isTrue(isint)) {
                  val veca = alloc.pointer(fpos)
                  lat.vector(size, init) >>= (vec => (pointer(veca), store.extend(veca, vec)))
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
      override def call(fpos: Identity, args: List[(Identity, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        val veca = alloc.pointer(fpos)
        lat.vector(number(args.size), bottom) >>= (
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

    object `vector-length` extends Store1Operation("vector-length") {
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

    object `vector-ref` extends Store2Operation("vector-ref") {
      def vectorRef(v: V, index: V, store: Store[A, V]): MayFail[V, Error] = {
        dereferencePointer(v, store) { vec =>
          ifThenElse(isVector(vec)) {
            lat.vectorRef(vec, index)
          } {
            MayFail.failure(PrimitiveNotApplicable(name, List(v, index)))
          }
        }
      }
      override def call(v: V, index: V, store: Store[A, V]) =
        vectorRef(v, index, store).map(v => (v, store))
    }

    object `vector-set!` extends SchemePrimitive[V, A] {
      def name = "vector-set!"
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
                  lat.vectorSet(vec, index, newval) >>= (
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
      override def call(fpos: Identity,
           args: List[(Identity, V)],
           store: Store[A, V],
           alloc: SchemeAllocator[A]) = args match {
        case v :: index :: newval :: Nil => vectorSet(v._2, index._2, newval._2, store)
        case _                           => MayFail.failure(PrimitiveArityError(name, 3, args.size))
      }
    }

    object `list` extends SchemePrimitive[V, A] {
      def name = "list"
      override def call(fpos: Identity,
        args: List[(Identity, V)],
        store: Store[A, V],
        alloc: SchemeAllocator[A]) = args match {
        case Nil => (nil, store)
        case (argpos, v) :: rest =>
          for {
            (restv, store2) <- call(fpos, rest, store, alloc)
          } yield {
            val carAddr = alloc.carAddr(argpos)
            val cdrAddr = alloc.cdrAddr(argpos)
            val updatedStore = store2.extend(carAddr, v) // Can use store here if store is mutated.
              .extend(cdrAddr, restv)
            (lat.cons(carAddr, cdrAddr), updatedStore)
          }
      }
    }

    abstract class FixpointPrimitiveUsingStore(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {

      // parameterized by
      // - the arguments to the recursive call
      type Args
      // - the representation of 'call components' (tuning precision)
      type Call
      // - a mapping from arguments to call components
      def callFor(args: Args): Call
      // - a function to compute the initial arguments from the primitive input
      def initialArgs(fpos: Identity, argsWithExps: List[(Identity, V)]): Option[Args]
      // - a function for updating the arguments when upon a new call to a 'call'
      def updateArgs(oldArgs: Args, newArgs: Args): Args
      // - (optional) a function for updating the result of a function call
      def updateResult(oldResult: MayFail[V,Error], newResult: MayFail[V,Error]): MayFail[V, Error] = mfMon.append(oldResult, newResult)
      // - a function to execute a single 'call' with given arguments
      def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[V,Error]

      override def call(fpos: Identity,
                        argsWithExps: List[(Identity, V)],
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

    object `append` extends FixpointPrimitiveUsingStore("append", Some(2)) {
      /** the arguments to append are the two given input arguments + the 'append expression' */
      type Args = (V,V,Identity)
      def initialArgs(fexp: Identity, args: List[(Identity, V)]) = args match {
        case (_, l1) :: (_, l2) :: Nil  => Some((l1, l2, fexp))
        case _                          => None
      }
      /** calls only take into account the current pair (= first argument of append) */
      type Call = V
      def callFor(args: Args) = args._1
      /** arguments: ignore changes to the index to ensure termination (and other args are guaranteed to be the same anyway)*/
      def updateArgs(oldArgs: Args, newArgs: Args) = oldArgs
      /** The actual implementation of append */
      override def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A,V], append: Args => MayFail[V,Error]): MayFail[V,Error] = args match {
        case (l1, l2, fexp) =>
          ifThenElse(isNull(l1)) {
            // if we have l1 = '(), append(l1,l2) = l2
            l2
          } {
            ifThenElse (isCons(l1)) {
            // if we have l1 = cons(a,d), append(l1,l2) = cons(a,append(d,l2))
            for {
              carv <- car.call(l1, store).map(_._1)
              cdrv <- cdr.call(l1, store).map(_._1)
              app_next <- append((cdrv, l2, fexp))
            } yield {
              val carAddr = alloc.carAddr(fexp)
              val cdrAddr = alloc.cdrAddr(fexp)
              store.extend(carAddr, carv)
              store.extend(cdrAddr, app_next)
              lat.cons(carAddr,cdrAddr)
            }
          } {
            // if we have have something else (i.e., not a list), append throws a type error!
            MayFail.failure(PrimitiveNotApplicable("length", List(l1)))
          }
        }
      }
    }
  }
}
