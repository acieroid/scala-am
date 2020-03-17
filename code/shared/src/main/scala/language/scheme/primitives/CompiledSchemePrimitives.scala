package scalaam.language.scheme.primitives

import scalaam.core.{Address, Error, Identity, MayFail, Store}
import scalaam.language.scheme.SchemeLattice

class CompiledSchemePrimitives[V, A <: Address](override implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V, A], _]) extends SchemeLatticePrimitives[V, A] {
  override def allPrimitives: List[SchemePrimitive[V, A]] = {
    import CompiledPrimitiveDefs._
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
      `list`, /* [vv] list: List Constructors */
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

  object CompiledPrimitiveDefs {

    import PrimitiveDefs._
    import schemeLattice.{bool, number}

    // Simpler than FixpointPrimitiveUsingStore BUT allows callWithArgs to return a modified store...
    abstract class SimpleFixpointPrimitiveUsingStore(val name: String, arity: Option[Int]) extends SchemePrimitive[V, A] {
      type Args = List[(Identity.Position, V)]

      // Executes a single call with given arguments.
      //def callWithArgs(prim: Identity.Position, args: Args, store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[(V, Store[A,V]),Error] // MUTABLE STORE (REPLACED)
      def callWithArgs(prim: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], cache: List[(Identity.Position, V)] => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error]

      override def call(fpos: Identity.Position, cpos: Identity.Position,
                        argsWithExps: List[(Identity.Position, V)],
                        store: Store[A, V],
                        alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = arity match {
          case Some(a) if argsWithExps.length == a => argsWithExps // .map(_._2)
          case None => return MayFail.failure(PrimitiveArityError(name, arity.getOrElse(-1), argsWithExps.length))
        }
        // for every call, keep track of the arguments
        // keep track of results for "visited" arguments
        var cache = Map[Args, MayFail[V, Error]]().withDefaultValue(mfMon.zero)
        // keep track of which calls depend on which other calls, independently of the position of the calls!
        var deps = Map[Args, Set[Args]]().withDefaultValue(Set())
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

    // COMPILED PRIMITIVES
    object `<=` extends SchemePrimitive[V, A] {
      val name = "<="

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            val `y_pos` = args(1)._1
            val `y` = args(1)._2
            `<`.call(fpos, (-1, -23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                bool(true)
              } {
                `=`.call(fpos, (-1, -31), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("<=", 2, args.length))
    }

    object `>=` extends SchemePrimitive[V, A] {
      val name = ">="

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            val `y_pos` = args(1)._1
            val `y` = args(1)._2
            `>`.call(fpos, (-1, -23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                bool(true)
              } {
                `=`.call(fpos, (-1, -31), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError(">=", 2, args.length))
    }

    object `>` extends SchemePrimitive[V, A] {
      val name = ">"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            val `y_pos` = args(1)._1
            val `y` = args(1)._2
            `<=`.call(fpos, (-1, -23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0` =>
              `not`.call(fpos, (-1, -18), List(((-1, -23), `_0`)), store, alloc).map(_._1)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError(">", 2, args.length))
    }

    object `zero?` extends SchemePrimitive[V, A] {
      val name = "zero?"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `=`.call(fpos, (-1, -20), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1)
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("zero?", 1, args.length))
    }

    object `positive?` extends SchemePrimitive[V, A] {
      val name = "positive?"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `>`.call(fpos, (-1, -24), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1)
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("positive?", 1, args.length))
    }

    object `negative?` extends SchemePrimitive[V, A] {
      val name = "negative?"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `<`.call(fpos, (-1, -24), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1)
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("negative?", 1, args.length))
    }

    object `odd?` extends SchemePrimitive[V, A] {
      val name = "odd?"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `modulo`.call(fpos, (-1, -24), List((`x_pos`, `x`), ((0, 0), number(2))), store, alloc).map(_._1) >>= { `_0` =>
              `=`.call(fpos, (-1, -19), List(((0, 0), number(1)), ((-1, -24), `_0`)), store, alloc).map(_._1)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("odd?", 1, args.length))
    }

    object `even?` extends SchemePrimitive[V, A] {
      val name = "even?"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `modulo`.call(fpos, (-1, -25), List((`x_pos`, `x`), ((0, 0), number(2))), store, alloc).map(_._1) >>= { `_0` =>
              `=`.call(fpos, (-1, -20), List(((0, 0), number(0)), ((-1, -25), `_0`)), store, alloc).map(_._1)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("even?", 1, args.length))
    }

    object `max` extends SchemePrimitive[V, A] {
      val name = "max"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `a_pos` = args(0)._1
            val `a` = args(0)._2
            val `b_pos` = args(1)._1
            val `b` = args(1)._2
            `<`.call(fpos, (-1, -24), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `b`
              } {
                `a`
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("max", 2, args.length))
    }

    object `min` extends SchemePrimitive[V, A] {
      val name = "min"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `a_pos` = args(0)._1
            val `a` = args(0)._2
            val `b_pos` = args(1)._1
            val `b` = args(1)._2
            `<`.call(fpos, (-1, -24), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `a`
              } {
                `b`
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("min", 2, args.length))
    }

    object `abs` extends SchemePrimitive[V, A] {
      val name = "abs"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `<`.call(fpos, (-1, -22), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `-`.call(fpos, (-1, -30), List(((0, 0), number(0)), (`x_pos`, `x`)), store, alloc).map(_._1)
              } {
                `x`
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("abs", 1, args.length))
    }

    object `gcd` extends SimpleFixpointPrimitiveUsingStore("gcd", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `a_pos` = args(0)._1
            val `a` = args(0)._2
            val `b_pos` = args(1)._1
            val `b` = args(1)._2
            `=`.call(fpos, (-1, -24), List((`b_pos`, `b`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `a`
              } {
                `modulo`.call(fpos, (-1, -41), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_1` =>
                  recursiveCall(List((`b_pos`, `b`), ((-1, -41), `_1`)))
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("gcd", 2, args.length))
    }

    object `lcm` extends SchemePrimitive[V, A] {
      val name = "lcm"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `m_pos` = args(0)._1
            val `m` = args(0)._2
            val `n_pos` = args(1)._1
            val `n` = args(1)._2
            `*`.call(fpos, (-1, -28), List((`m_pos`, `m`), (`n_pos`, `n`)), store, alloc).map(_._1) >>= { `_0` =>
              `abs`.call(fpos, (-1, -23), List(((-1, -28), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `gcd`.call(fpos, (-1, -37), List((`m_pos`, `m`), (`n_pos`, `n`)), store, alloc).map(_._1) >>= { `_2` =>
                  `/`.call(fpos, (-1, -20), List(((-1, -23), `_1`), ((-1, -37), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("lcm", 2, args.length))
    }

    object `not` extends SchemePrimitive[V, A] {
      val name = "not"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            ifThenElse(`x`) {
              bool(false)
            } {
              bool(true)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("not", 1, args.length))
    }

    object `newline` extends SchemePrimitive[V, A] {
      val name = "newline"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 0) {
          {
            bool(false)
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("newline", 0, args.length))
    }

    object `display` extends SchemePrimitive[V, A] {
      val name = "display"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `x`
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("display", 1, args.length))
    }

    object `caar` extends SchemePrimitive[V, A] {
      val name = "caar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -19), List(((-1, -24), `_0`)), store, alloc).map(_._1)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caar", 1, args.length))
    }

    object `cadr` extends SchemePrimitive[V, A] {
      val name = "cadr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -19), List(((-1, -24), `_0`)), store, alloc).map(_._1)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cadr", 1, args.length))
    }

    object `cddr` extends SchemePrimitive[V, A] {
      val name = "cddr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -19), List(((-1, -24), `_0`)), store, alloc).map(_._1)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cddr", 1, args.length))
    }

    object `cdar` extends SchemePrimitive[V, A] {
      val name = "cdar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -19), List(((-1, -24), `_0`)), store, alloc).map(_._1)
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdar", 1, args.length))
    }

    object `caaar` extends SchemePrimitive[V, A] {
      val name = "caaar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caaar", 1, args.length))
    }

    object `caadr` extends SchemePrimitive[V, A] {
      val name = "caadr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caadr", 1, args.length))
    }

    object `cadar` extends SchemePrimitive[V, A] {
      val name = "cadar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cadar", 1, args.length))
    }

    object `caddr` extends SchemePrimitive[V, A] {
      val name = "caddr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caddr", 1, args.length))
    }

    object `cdaar` extends SchemePrimitive[V, A] {
      val name = "cdaar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdaar", 1, args.length))
    }

    object `cdadr` extends SchemePrimitive[V, A] {
      val name = "cdadr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdadr", 1, args.length))
    }

    object `cddar` extends SchemePrimitive[V, A] {
      val name = "cddar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cddar", 1, args.length))
    }

    object `cdddr` extends SchemePrimitive[V, A] {
      val name = "cdddr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -25), List(((-1, -30), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -20), List(((-1, -25), `_1`)), store, alloc).map(_._1)
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdddr", 1, args.length))
    }

    object `caaaar` extends SchemePrimitive[V, A] {
      val name = "caaaar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caaaar", 1, args.length))
    }

    object `caaadr` extends SchemePrimitive[V, A] {
      val name = "caaadr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caaadr", 1, args.length))
    }

    object `caadar` extends SchemePrimitive[V, A] {
      val name = "caadar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caadar", 1, args.length))
    }

    object `caaddr` extends SchemePrimitive[V, A] {
      val name = "caaddr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caaddr", 1, args.length))
    }

    object `cadaar` extends SchemePrimitive[V, A] {
      val name = "cadaar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cadaar", 1, args.length))
    }

    object `cadadr` extends SchemePrimitive[V, A] {
      val name = "cadadr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cadadr", 1, args.length))
    }

    object `caddar` extends SchemePrimitive[V, A] {
      val name = "caddar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("caddar", 1, args.length))
    }

    object `cadddr` extends SchemePrimitive[V, A] {
      val name = "cadddr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `car`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cadddr", 1, args.length))
    }

    object `cdaaar` extends SchemePrimitive[V, A] {
      val name = "cdaaar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdaaar", 1, args.length))
    }

    object `cdaadr` extends SchemePrimitive[V, A] {
      val name = "cdaadr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdaadr", 1, args.length))
    }

    object `cdadar` extends SchemePrimitive[V, A] {
      val name = "cdadar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdadar", 1, args.length))
    }

    object `cdaddr` extends SchemePrimitive[V, A] {
      val name = "cdaddr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `car`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdaddr", 1, args.length))
    }

    object `cddaar` extends SchemePrimitive[V, A] {
      val name = "cddaar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cddaar", 1, args.length))
    }

    object `cddadr` extends SchemePrimitive[V, A] {
      val name = "cddadr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `car`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cddadr", 1, args.length))
    }

    object `cdddar` extends SchemePrimitive[V, A] {
      val name = "cdddar"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `car`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cdddar", 1, args.length))
    }

    object `cddddr` extends SchemePrimitive[V, A] {
      val name = "cddddr"

      override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            `cdr`.call(fpos, (-1, -36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0` =>
              `cdr`.call(fpos, (-1, -31), List(((-1, -36), `_0`)), store, alloc).map(_._1) >>= { `_1` =>
                `cdr`.call(fpos, (-1, -26), List(((-1, -31), `_1`)), store, alloc).map(_._1) >>= { `_2` =>
                  `cdr`.call(fpos, (-1, -21), List(((-1, -26), `_2`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("cddddr", 1, args.length))
    }

    object `equal?` extends SimpleFixpointPrimitiveUsingStore("equal?", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `a_pos` = args(0)._1
            val `a` = args(0)._2
            val `b_pos` = args(1)._1
            val `b` = args(1)._2
            `eq?`.call(fpos, (-2, -16), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                bool(true)
              } {
                `null?`.call(fpos, (-3, -19), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_1` =>
                  ifThenElse(`_1`) {
                    `null?`.call(fpos, (-3, -29), List((`b_pos`, `b`)), store, alloc).map(_._1)
                  } {
                    bool(false)
                  } >>= { `_2` =>
                    ifThenElse(`_2`) {
                      bool(true)
                    } {
                      `pair?`.call(fpos, (-4, -19), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_3` =>
                        ifThenElse(`_3`) {
                          `pair?`.call(fpos, (-4, -29), List((`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_4` =>
                            ifThenElse(`_4`) {
                              `car`.call(fpos, (-4, -47), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_5` =>
                                `car`.call(fpos, (-4, -55), List((`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_6` =>
                                  recursiveCall(List(((-4, -47), `_5`), ((-4, -55), `_6`))) >>= { `_7` =>
                                    ifThenElse(`_7`) {
                                      `cdr`.call(fpos, (-4, -72), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_8` =>
                                        `cdr`.call(fpos, (-4, -80), List((`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_9` =>
                                          recursiveCall(List(((-4, -72), `_8`), ((-4, -80), `_9`)))
                                        }
                                      }
                                    } {
                                      bool(false)
                                    }
                                  }
                                }
                              }
                            } {
                              bool(false)
                            }
                          }
                        } {
                          bool(false)
                        }
                      }
                    }
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("equal?", 2, args.length))
    }

    object `list?` extends SimpleFixpointPrimitiveUsingStore("list?", Some(1)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `l_pos` = args(0)._1
            val `l` = args(0)._2
            `pair?`.call(fpos, (-1, -29), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `cdr`.call(fpos, (-1, -46), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  recursiveCall(List(((-1, -46), `_1`)))
                }
              } {
                bool(false)
              } >>= { `_2` =>
                ifThenElse(`_2`) {
                  bool(true)
                } {
                  `null?`.call(fpos, (-1, -56), List((`l_pos`, `l`)), store, alloc).map(_._1)
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("list?", 1, args.length))
    }

    object `list-ref` extends SimpleFixpointPrimitiveUsingStore("list-ref", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `l_pos` = args(0)._1
            val `l` = args(0)._2
            val `index_pos` = args(1)._1
            val `index` = args(1)._2
            `=`.call(fpos, (-2, -18), List((`index_pos`, `index`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `car`.call(fpos, (-3, -16), List((`l_pos`, `l`)), store, alloc).map(_._1)
              } {
                `cdr`.call(fpos, (-4, -26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  `-`.call(fpos, (-4, -34), List((`index_pos`, `index`), ((0, 0), number(1))), store, alloc).map(_._1) >>= { `_2` =>
                    recursiveCall(List(((-4, -26), `_1`), ((-4, -34), `_2`)))
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("list-ref", 2, args.length))
    }

    object `member` extends SimpleFixpointPrimitiveUsingStore("member", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `e_pos` = args(0)._1
            val `e` = args(0)._2
            val `l_pos` = args(1)._1
            val `l` = args(1)._2
            `null?`.call(fpos, (-2, -16), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                bool(false)
              } {
                `car`.call(fpos, (-4, -26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  `equal?`.call(fpos, (-4, -18), List(((-4, -26), `_1`), (`e_pos`, `e`)), store, alloc).map(_._1) >>= { `_2` =>
                    ifThenElse(`_2`) {
                      `l`
                    } {
                      `cdr`.call(fpos, (-6, -26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3` =>
                        recursiveCall(List((`e_pos`, `e`), ((-6, -26), `_3`)))
                      }
                    }
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("member", 2, args.length))
    }

    object `memq` extends SimpleFixpointPrimitiveUsingStore("memq", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `e_pos` = args(0)._1
            val `e` = args(0)._2
            val `l_pos` = args(1)._1
            val `l` = args(1)._2
            `null?`.call(fpos, (-2, -16), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                bool(false)
              } {
                `car`.call(fpos, (-4, -23), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  `eq?`.call(fpos, (-4, -18), List(((-4, -23), `_1`), (`e_pos`, `e`)), store, alloc).map(_._1) >>= { `_2` =>
                    ifThenElse(`_2`) {
                      `l`
                    } {
                      `cdr`.call(fpos, (-6, -24), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3` =>
                        recursiveCall(List((`e_pos`, `e`), ((-6, -24), `_3`)))
                      }
                    }
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("memq", 2, args.length))
    }

    object `assoc` extends SimpleFixpointPrimitiveUsingStore("assoc", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `k_pos` = args(0)._1
            val `k` = args(0)._2
            val `l_pos` = args(1)._1
            val `l` = args(1)._2
            `null?`.call(fpos, (-2, -14), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                bool(false)
              } {
                `caar`.call(fpos, (-4, -23), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  `equal?`.call(fpos, (-4, -15), List(((-4, -23), `_1`), (`k_pos`, `k`)), store, alloc).map(_._1) >>= { `_2` =>
                    ifThenElse(`_2`) {
                      `car`.call(fpos, (-5, -13), List((`l_pos`, `l`)), store, alloc).map(_._1)
                    } {
                      `cdr`.call(fpos, (-6, -22), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3` =>
                        recursiveCall(List((`k_pos`, `k`), ((-6, -22), `_3`)))
                      }
                    }
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("assoc", 2, args.length))
    }

    object `assq` extends SimpleFixpointPrimitiveUsingStore("assq", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `k_pos` = args(0)._1
            val `k` = args(0)._2
            val `l_pos` = args(1)._1
            val `l` = args(1)._2
            `null?`.call(fpos, (-2, -14), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                bool(false)
              } {
                `caar`.call(fpos, (-4, -20), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  `eq?`.call(fpos, (-4, -15), List(((-4, -20), `_1`), (`k_pos`, `k`)), store, alloc).map(_._1) >>= { `_2` =>
                    ifThenElse(`_2`) {
                      `car`.call(fpos, (-5, -13), List((`l_pos`, `l`)), store, alloc).map(_._1)
                    } {
                      `cdr`.call(fpos, (-6, -21), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3` =>
                        recursiveCall(List((`k_pos`, `k`), ((-6, -21), `_3`)))
                      }
                    }
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("assq", 2, args.length))
    }

    object `list-tail` extends SimpleFixpointPrimitiveUsingStore("list-tail", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `x_pos` = args(0)._1
            val `x` = args(0)._2
            val `k_pos` = args(1)._1
            val `k` = args(1)._2
            `zero?`.call(fpos, (-2, -10), List((`k_pos`, `k`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `x`
              } {
                `cdr`.call(fpos, (-4, -21), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_1` =>
                  `-`.call(fpos, (-4, -29), List((`k_pos`, `k`), ((0, 0), number(1))), store, alloc).map(_._1) >>= { `_2` =>
                    recursiveCall(List(((-4, -21), `_1`), ((-4, -29), `_2`)))
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("list-tail", 2, args.length))
    }

    object `length` extends SimpleFixpointPrimitiveUsingStore("length", Some(1)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `l_pos` = args(0)._1
            val `l` = args(0)._2
            `null?`.call(fpos, (-2, -16), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                number(0)
              } {
                `cdr`.call(fpos, (-4, -29), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  recursiveCall(List(((-4, -29), `_1`))) >>= { `_2` =>
                    `+`.call(fpos, (-4, -16), List(((0, 0), number(1)), ((-4, -21), `_2`)), store, alloc).map(_._1)
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("length", 1, args.length))
    }

    object `append` extends SimpleFixpointPrimitiveUsingStore("append", Some(2)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 2) {
          {
            val `l1_pos` = args(0)._1
            val `l1` = args(0)._2
            val `l2_pos` = args(1)._1
            val `l2` = args(1)._2
            `null?`.call(fpos, (-2, -16), List((`l1_pos`, `l1`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                `l2`
              } {
                `car`.call(fpos, (-4, -22), List((`l1_pos`, `l1`)), store, alloc).map(_._1) >>= { `_1` =>
                  `cdr`.call(fpos, (-5, -30), List((`l1_pos`, `l1`)), store, alloc).map(_._1) >>= { `_2` =>
                    recursiveCall(List(((-5, -30), `_2`), (`l2_pos`, `l2`))) >>= { `_3` =>
                      `cons`.call((4, 16), (-4, -16), List(((-4, -22), `_1`), ((-5, -22), `_3`)), store, alloc).map(_._1)
                    }
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("append", 2, args.length))
    }

    object `reverse` extends SimpleFixpointPrimitiveUsingStore("reverse", Some(1)) {
      def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.length == 1) {
          {
            val `l_pos` = args(0)._1
            val `l` = args(0)._2
            `null?`.call(fpos, (-2, -9), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_0` =>
              ifThenElse(`_0`) {
                schemeLattice.nil
              } {
                `cdr`.call(fpos, (-4, -26), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_1` =>
                  recursiveCall(List(((-4, -26), `_1`))) >>= { `_2` =>
                    `car`.call(fpos, (-5, -23), List((`l_pos`, `l`)), store, alloc).map(_._1) >>= { `_3` =>
                      `list`.call(fpos, (-5, -17), List(((-5, -23), `_3`)), store, alloc).map(_._1) >>= { `_4` =>
                        `append`.call(fpos, (-4, -9), List(((-4, -17), `_2`), ((-5, -17), `_4`)), store, alloc).map(_._1)
                      }
                    }
                  }
                }
              }
            }
          }.map(x => (x, store))
        } else MayFail.failure(PrimitiveArityError("reverse", 1, args.length))
    }

  }

}
