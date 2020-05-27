package scalaam.language.scheme.primitives

import scalaam.core._
import scalaam.language.scheme._

class  SchemeLatticePrimitives[V, A <: Address](override implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) extends SchemePrimitives[V, A] with PrimitiveBuildingBlocks[V,A] {
  val lat = schemeLattice
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
      /* [x]  assv: Retrieving Alist Entries => Prelude */
      `atan`, /* [vv] atan: Scientific */
      //`append`, // => SchemePrelude
      `boolean?`, /* [vv] boolean?: Booleans */
      /* [x]  call-with-current-continuation: Continuations */
      /* [x]  call-with-input-file: File Ports */
      /* [x]  call-with-output-file: File Ports */
      /* [x]  call-with-values: Multiple Values */
      `car`, /* [vv] car: Pairs */
      `cdr`, /* [vv] cdr: Pairs */
      `ceiling`, /* [vv] ceiling: Arithmetic */
      `char->integer`, /* [x]  char->integer: Characters */
      `char->string`,
      /* [x]  char-alphabetic?: Characters */
      /* [x]  char-ci<=?: Characters */
      /* [x]  char-ci<?: Characters */
      /* [x]  char-ci=?: Characters */
      /* [x]  char-ci>=?: Characters */
      /* [x]  char-ci>?: Characters */
      `char-downcase`, /* [x]  char-downcase: Characters */
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
      `max`,
      /* [x]  memv: List Searching */
      `min`,
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
      `string->number`, /* [x]  string->number: Conversion */
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

  class NoStore1Operation(val name: String, val call: V => MayFail[V, Error]) extends SchemePrimitive[V, A] {
    override def call(fpos: SchemeExp,
                      args: List[(SchemeExp, V)],
                      store: Store[A, V],
                      alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: Nil => call(x._2).map(v => (v, store))
      case _ => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  class NoStore2Operation(val name: String, val call: (V, V) => MayFail[V, Error]) extends SchemePrimitive[V, A] {
    override def call(fpos: SchemeExp,
                      args: List[(SchemeExp, V)],
                      store: Store[A, V],
                      alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: Nil => call(x._2, y._2).map(v => (v, store))
      case _ => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  abstract class NoStoreLOperation(val name: String) extends SchemePrimitive[V, A] {
    def call(args: List[V]): MayFail[V, Error]
    override def call(fpos: SchemeExp,
      args: List[(SchemeExp, V)],
      store: Store[A, V],
      alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
      call(args.map(_._2)).map(v => (v, store))
  }

  class NoStoreLOp(val n: String, val c: List[V] => MayFail[V, Error]) extends NoStoreLOperation(n) {
    def call(args: List[V]): MayFail[V, Error] = c(args)
  }

  class NoStoreLOpRec(val n: String, val c: (List[V], List[V] => MayFail[V, Error]) => MayFail[V, Error]) extends NoStoreLOperation(n) {
    def call(args: List[V]): MayFail[V, Error] = c(args, call)
  }

  class Store1Operation(val name: String, val call: (V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]) extends SchemePrimitive[V, A] {
    override def call(fpos: SchemeExp,
                      args: List[(SchemeExp, V)],
                      store: Store[A, V],
                      alloc: SchemeAllocator[A]
                     ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: Nil => call(x._2, store)
      case _ => MayFail.failure(PrimitiveArityError(name, 1, args.length))
    }
  }

  class Store2Operation(val name: String, val call: (V, V, Store[A, V]) => MayFail[(V, Store[A, V]), Error]) extends SchemePrimitive[V, A] {
    override def call(fpos: SchemeExp,
                      args: List[(SchemeExp, V)],
                      store: Store[A, V],
                      alloc: SchemeAllocator[A]
                     ): MayFail[(V, Store[A, V]), Error] = args match {
      case x :: y :: Nil => call(x._2, y._2, store)
      case _             => MayFail.failure(PrimitiveArityError(name, 2, args.length))
    }
  }

  object PrimitiveDefs extends PrimitiveBuildingBlocks[V, A] {

    val lat: SchemeLattice[V, A, SchemePrimitive[V, A], _] = schemeLattice

    import schemeLattice._

    object `+` extends NoStoreLOpRec("+", {
        case (Nil, _)          => number(0)
        case (x :: rest, call) => call(rest) >>= (binaryOp(SchemeOps.BinaryOperator.Plus)(x, _))
    })
    object `-` extends NoStoreLOp("-", {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError("-", 1, 0))
        case x :: Nil  => binaryOp(SchemeOps.BinaryOperator.Minus)(number(0), x)
        case x :: rest => `+`.call(rest) >>= (binaryOp(SchemeOps.BinaryOperator.Minus)(x, _))
    })
    object `*` extends NoStoreLOpRec("*", {
        case (Nil, _)          => number(1)
        case (x :: rest, call) => call(rest) >>= (binaryOp(SchemeOps.BinaryOperator.Times)(x, _))
    })
    object `/` extends NoStoreLOp("/", {
      case Nil => MayFail.failure(PrimitiveVariadicArityError("/", 1, 0))
      case x :: rest =>
        for {
          multrest      <- `*`.call(rest)
          r             <- binaryOp(SchemeOps.BinaryOperator.Div)(x, multrest)
          fl            <- unaryOp(SchemeOps.UnaryOperator.Floor)(r)
          isexact       <- eqq(r, fl)
          xisint        <- isInteger(x)
          multrestisint <- isInteger(multrest)
          convert       <- and(isexact, and(xisint, multrestisint))
          exr           <- inexactToExact(r)
          res           <- ifThenElse(convert) { exr } { r }
        } yield {
          res
        }
    })

    object `max` extends NoStoreLOpRec("max", {
      case (Nil, _)          => MayFail.failure(PrimitiveVariadicArityError("max", 1, 0))
      case (x :: Nil, _)     => x
      case (x :: rest, call) => call(rest) >>= { y => ifThenElse(`<`.call(x, y)) { y } { x } }
    })
    object `min` extends NoStoreLOpRec("min", {
      case (Nil, _)          => MayFail.failure(PrimitiveVariadicArityError("min", 1, 0))
      case (x :: Nil, _)     => x
      case (x :: rest, call) => call(rest) >>= { y => ifThenElse(`<`.call(x, y)) { x } { y } }
    })

    object `expt`     extends NoStore2Operation("expt", binaryOp(SchemeOps.BinaryOperator.Expt))
    object `quotient` extends NoStore2Operation("quotient", binaryOp(SchemeOps.BinaryOperator.Quotient))
    object `<`        extends NoStore2Operation("<", binaryOp(SchemeOps.BinaryOperator.Lt)) // xTODO[easy]: < should accept any number of arguments (same for <= etc.)

    object `=` extends NoStoreLOperation("=") {
      def eq(first: V, l: List[V]): MayFail[V, Error] = l match {
        case Nil => bool(true)
        case x :: rest =>
          ifThenElse(binaryOp(SchemeOps.BinaryOperator.NumEq)(first, x)) {
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

    object `modulo`    extends NoStore2Operation("modulo",    binaryOp(SchemeOps.BinaryOperator.Modulo))
    object `remainder` extends NoStore2Operation("remainder", binaryOp(SchemeOps.BinaryOperator.Remainder))
    object `random`    extends NoStore1Operation("random",    unaryOp(SchemeOps.UnaryOperator.Random))
    object `ceiling`   extends NoStore1Operation("ceiling",   unaryOp(SchemeOps.UnaryOperator.Ceiling))
    object `floor`     extends NoStore1Operation("floor",     unaryOp(SchemeOps.UnaryOperator.Floor))
    object `round`     extends NoStore1Operation("round",     unaryOp(SchemeOps.UnaryOperator.Round))
    object `log`       extends NoStore1Operation("log",       unaryOp(SchemeOps.UnaryOperator.Log))
    object `sin`       extends NoStore1Operation("sin",       unaryOp(SchemeOps.UnaryOperator.Sin))
    object `asin`      extends NoStore1Operation("asin",      unaryOp(SchemeOps.UnaryOperator.ASin))
    object `cos`       extends NoStore1Operation("cos",       unaryOp(SchemeOps.UnaryOperator.Cos))
    object `acos`      extends NoStore1Operation("acos",      unaryOp(SchemeOps.UnaryOperator.ACos))
    object `tan`       extends NoStore1Operation("tan",       unaryOp(SchemeOps.UnaryOperator.Tan))
    object `atan`      extends NoStore1Operation("atan",      unaryOp(SchemeOps.UnaryOperator.ATan))
    object `sqrt`      extends NoStore1Operation("sqrt", { x =>
      ifThenElse(`<`.call(x, number(0))) {
        /* n < 0 */
        MayFail.failure(PrimitiveNotApplicable("sqrt", List(x)))
      } {
        /* n >= 0 */
        for {
          r          <- unaryOp(SchemeOps.UnaryOperator.Sqrt)(x)
          fl         <- unaryOp(SchemeOps.UnaryOperator.Floor)(r)
          argisexact <- isInteger(x)
          resisexact <- eqq(r, fl)
          convert    <- and(argisexact, resisexact)
          exr        <- inexactToExact(r)
          res        <- ifThenElse(convert) { exr } { r }
        } yield {
          res
        }
      }
    })
    object `exact->inexact` extends NoStore1Operation("exact->inexact", unaryOp(SchemeOps.UnaryOperator.ExactToInexact))
    object `inexact->exact` extends NoStore1Operation("inexact->exact", unaryOp(SchemeOps.UnaryOperator.InexactToExact))
    object `char->integer`  extends NoStore1Operation("char->integer",  unaryOp(SchemeOps.UnaryOperator.CharacterToInteger))
    object `char->string`   extends NoStore1Operation("char->string",   unaryOp(SchemeOps.UnaryOperator.CharacterToString))
    object `char-downcase`  extends NoStore1Operation("char-downcase",  unaryOp(SchemeOps.UnaryOperator.CharacterDowncase))
    object `null?`          extends NoStore1Operation("null?",          unaryOp(SchemeOps.UnaryOperator.IsNull))

    object `pair?`    extends Store1Operation("pair?", { (x, store) => 
      ifThenElse(unaryOp(SchemeOps.UnaryOperator.IsPointer)(x)) {
        dereferencePointer(x, store) { cons =>
          unaryOp(SchemeOps.UnaryOperator.IsCons)(cons)
        }
      } {
        bool(false)
      }.map(v => (v, store))
    })

    object `char?`    extends NoStore1Operation("char?",    unaryOp(SchemeOps.UnaryOperator.IsChar))
    object `symbol?`  extends NoStore1Operation("symbol?",  unaryOp(SchemeOps.UnaryOperator.IsSymbol))
    object `string?`  extends NoStore1Operation("string?",  unaryOp(SchemeOps.UnaryOperator.IsString))
    object `integer?` extends NoStore1Operation("integer?", unaryOp(SchemeOps.UnaryOperator.IsInteger))
    object `real?`    extends NoStore1Operation("real?", { x =>
          for {
            isint  <- isInteger(x)
            isreal <- unaryOp(SchemeOps.UnaryOperator.IsReal)(x)
          } yield or(isint, isreal)
      })
    object `number?`  extends NoStore1Operation("number?", `real?`.call(_)) /* No support for complex number, so number? is equivalent as real? */
    object `boolean?` extends NoStore1Operation("boolean?", unaryOp(SchemeOps.UnaryOperator.IsBoolean))
    object `vector?`  extends Store1Operation("vector?", { (x, store) =>
        for {
          ispointer <- unaryOp(SchemeOps.UnaryOperator.IsPointer)(x)
          isvector <- dereferencePointer(x, store) { v =>
            isVector(v)
          }
        } yield (and(ispointer, isvector), store)
    })
    object `eq?` extends NoStore2Operation("eq?", eqq)

    object `number->string` extends NoStore1Operation("number->string", unaryOp(SchemeOps.UnaryOperator.NumberToString))
    object `symbol->string` extends NoStore1Operation("symbol->string", unaryOp(SchemeOps.UnaryOperator.SymbolToString))
    object `string->symbol` extends NoStore1Operation("string->symbol", unaryOp(SchemeOps.UnaryOperator.StringToSymbol))
    object `string->number` extends NoStore1Operation("string->number", unaryOp(SchemeOps.UnaryOperator.StringToNumber))
    object `string-append`  extends NoStoreLOpRec("string-append", {
        case (Nil, _)          => string("")
        case (x :: rest, call) => call(rest) >>= (binaryOp(SchemeOps.BinaryOperator.StringAppend)(x, _))
    })
    object `string-ref`    extends NoStore2Operation("string-ref",    binaryOp(SchemeOps.BinaryOperator.StringRef))
    object `string<?`      extends NoStore2Operation("string<?",      binaryOp(SchemeOps.BinaryOperator.StringLt))
    object `string-length` extends NoStore1Operation("string-length", unaryOp(SchemeOps.UnaryOperator.StringLength))

    object `error` extends NoStore1Operation("error", { x => MayFail.failure(UserError(x.toString))})

    object `cons` extends SchemePrimitive[V,A] {
      val name = "cons"
      override def call(fpos: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val addr = alloc.pointer(fpos)
          val consVal = lat.cons(car, cdr)
          val pointer = lat.pointer(addr)
          (pointer, store.extend(addr, consVal))
        case l => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }

    object `car` extends Store1Operation("car", { (x, store) =>
      dereferencePointer(x, store) { cons => lat.car(cons) }.map(v => (v,store))
    })
    object `cdr` extends Store1Operation("cdr", { (x, store) => 
      dereferencePointer(x, store) { cons => lat.cdr(cons) }.map(v => (v,store))
    })

    object `set-car!` extends Store2Operation("set-car!", { (cell, value, store) =>
      dereferencePointerGetAddressReturnStore(cell, store) { (addr,cons,store) => 
        for {
          car <- lat.car(cons)
          cdr <- lat.cdr(cons)
          joined = lat.join(car,value)
          updated = lat.cons(joined,cdr) 
        } yield (bool(false), store.update(addr,updated))
      }})

    object `set-cdr!` extends Store2Operation("set-cdr!", { (cell, value, store) =>
      dereferencePointerGetAddressReturnStore(cell, store) { (addr,cons,store) => 
        for {
          car <- lat.car(cons)
          cdr <- lat.cdr(cons)
          joined = lat.join(cdr,value)
          updated = lat.cons(car,joined) 
        } yield (bool(false), store.update(addr,updated))
      }})

    object `make-vector` extends SchemePrimitive[V,A] {
      val name = "make-vector"
      override def call(fpos: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
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
      override def call(fpos: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
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

    object `vector-length` extends Store1Operation("vector-length", { (v, store) =>
      dereferencePointer(v, store) { vec =>
        ifThenElse(isVector(vec)) {
          vectorLength(vec)
        } {
          MayFail.failure(PrimitiveNotApplicable("vector-length", List(v)))
        }
      }.map((_, store))
    })

    object `vector-ref` extends Store2Operation("vector-ref", { (v, index, store) =>
      dereferencePointer(v, store) { vec =>
        ifThenElse(isVector(vec)) {
          lat.vectorRef(vec, index)
        } {
          MayFail.failure(PrimitiveNotApplicable("vector-ref", List(v, index)))
        }
      }.map((_, store))
    })

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
      override def call(fpos: SchemeExp,
           args: List[(SchemeExp, V)],
           store: Store[A, V],
           alloc: SchemeAllocator[A]) = args match {
        case v :: index :: newval :: Nil => vectorSet(v._2, index._2, newval._2, store)
        case _                           => MayFail.failure(PrimitiveArityError(name, 3, args.size))
      }
    }

    object `list` extends SchemePrimitive[V, A] {
      def name = "list"
      override def call(fpos: SchemeExp,
        args: List[(SchemeExp, V)],
        store: Store[A, V],
        alloc: SchemeAllocator[A]) = args match {
        case Nil => (nil, store)
        case (argpos, v) :: rest =>
          for {
            (restv, store2) <- call(fpos, rest, store, alloc)
          } yield {
            val addr = alloc.pointer(argpos)
            val pair = lat.cons(v, restv)
            val updatedStore = store2.extend(addr, pair)
            val pointer = lat.pointer(addr)
            (pointer, updatedStore)
          }
      }
    }
  }
}
