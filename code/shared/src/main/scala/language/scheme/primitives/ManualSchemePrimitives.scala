package scalaam.language.scheme.primitives

import scalaam.core.{Address, Error, Identity, MayFail, Store}
import scalaam.language.scheme.SchemeLattice
import scalaam.util.Annotations.unsound

class ManualSchemePrimitives[V, A <: Address](override implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V, A], _]) extends SchemeLatticePrimitives[V, A] {
  override def allPrimitives: List[SchemePrimitive[V, A]] = {
    import ManualPrimitiveDefs._
    import PrimitiveDefs._
    List(
      `modulo`,
      `*`, /* [vv] *: Arithmetic */
      `+`, /* [vv] +: Arithmetic */
      `-`, /* [vv] -: Arithmetic */
      `/`, /* [vx] /: Arithmetic (no support for fractions) */
      `acos`, /* [vv] acos: Scientific */
      /* [x]  angle: Complex */
      `append`, /* [x]  append: Append/Reverse */
      // MANUAL
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
      `equal?`,
      `eq?`, /* [vv] eq?: Equality */
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

  object ManualPrimitiveDefs extends PrimitiveBuildingBlocks[V, A] {

    val lat: SchemeLattice[V, A, SchemePrimitive[V, A], _] = schemeLattice

    import scala.util.control.TailCalls._

    // Simpler than FixpointPrimitiveUsingStore BUT allows callWithArgs to return a modified store...
    abstract class SimpleFixpointPrimitiveUsingStore(val name: String, arity: Option[Int]) extends SchemePrimitive[V, A] {
      type Args = List[V]

      // Executes a single call with given arguments.
      //def callWithArgs(prim: Identity.Position, args: Args, store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[(V, Store[A,V]),Error] // MUTABLE STORE (REPLACED)
      def callWithArgs(prim: Identity.Position, args: Args, store: Store[A, V], cache: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[V, Error]

      override def call(fpos: Identity.Position,
                        argsWithExps: List[(Identity.Position, V)],
                        store: Store[A, V],
                        alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = arity match {
          case Some(a) if argsWithExps.length == a => argsWithExps.map(_._2)
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

    import schemeLattice._

    private trait Clause {
      def otherwise(act: => MayFail[V, Error]): MayFail[V, Error]

      def otherwise(cls: Clause): Clause = otherwise_(cls, this)

      def otherwise_(cls: Clause, parent: Clause) = new Clause {
        def otherwise(alt: => MayFail[V, Error]) = parent.otherwise(cls.otherwise(alt))
      }
    }

    private def ifV(prd: => MayFail[V, Error])(csq: => MayFail[V, Error]) = new Clause {
      def otherwise(alt: => MayFail[V, Error]) = ifThenElse(prd) {
        csq
      } {
        alt
      }
    }

    object `<=` extends NoStore2Operation("<=") {
      override def call(x: V, y: V) = (or _) (lt(x, y), numEq(x, y))
    }

    object `>` extends NoStore2Operation(">") {
      override def call(x: V, y: V) = lat_not(`<=`.call(x, y))
    }

    object `>=` extends NoStore2Operation(">=") {
      override def call(x: V, y: V) = lat_not(PrimitiveDefs.`<`.call(x, y))
    }

    /** (define (zero? x) (= x 0)) */
    object `zero?` extends NoStore1Operation("zero?") {
      override def call(x: V) = numEq(number(0), x)
    }

    /** (define (positive? x) (< 0 x)) */
    object `positive?` extends NoStore1Operation("positive?") {
      override def call(x: V) = lt(number(0), x)
    }

    /** (define (negative? x) (< x 0)) */
    object `negative?` extends NoStore1Operation("negative?") {
      override def call(x: V) = lt(x, number(0))
    }

    /** (define (odd? x) (= 1 (modulo x 2))) */
    object `odd?` extends NoStore1Operation("odd?") {
      override def call(x: V) = lat_modulo(x, number(2)) >>= (numEq(number(1), _))
    }

    /** (define (even? x) (= 0 (modulo x 2))) */
    object `even?` extends NoStore1Operation("even?") {
      override def call(x: V) = lat_modulo(x, number(2)) >>= (numEq(number(0), _))
    }

    object `max` extends NoStore2Operation("max") {
      override def call(x: V, y: V) =
        ifThenElse(lt(x, y)) {
          y
        } {
          x
        }
    }

    object `min` extends NoStore2Operation("min") {
      override def call(x: V, y: V) =
        ifThenElse(lt(x, y)) {
          x
        } {
          y
        }
    }

    /** (define (abs x) (if (< x 0) (- 0 x) x)) */
    object `abs` extends NoStore1Operation("abs") {
      override def call(x: V) =
        ifThenElse(lt(x, number(0))) {
          minus(number(0), x)
        } {
          x
        }
    }

    /** (define (gcd a b) (if (= b 0) a (gcd b (modulo a b)))) */
    object `gcd` extends SimpleFixpointPrimitive("gcd", Some(2)) {
      def callWithArgs(args: Args, gcd: Args => MayFail[V, Error]): MayFail[V, Error] = {
        val a :: b :: Nil = args
        ifThenElse(numEq(b, number(0))) {
          a
        } {
          lat_modulo(a, b) >>= (amodb => gcd(List(b, amodb)))
        }
      }
    }

    object `not` extends NoStore1Operation("not") {
      override def call(x: V) = lat_not(x)
    }

    object `newline` extends SchemePrimitive[V, A] {
      val name = "newline"

      override def call(fpos: Identity.Position,
                        args: List[(Identity.Position, V)],
                        store: Store[A, V],
                        alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
        if (args.isEmpty) {
          MayFail.success((bool(false), store))
        } else {
          MayFail.failure(PrimitiveArityError(name, 0, args.length))
        }
    }

    object `display` extends NoStore1Operation("display") {
      override def call(x: V) = {
        x /* Undefined behavior in R5RS, but we return the printed value */
      }
    }

    object `caar` extends PrimitiveDefs.CarCdrOperation("caar")

    object `cadr` extends PrimitiveDefs.CarCdrOperation("cadr")

    object `cdar` extends PrimitiveDefs.CarCdrOperation("cdar")

    object `cddr` extends PrimitiveDefs.CarCdrOperation("cddr")

    object `caaar` extends PrimitiveDefs.CarCdrOperation("caaar")

    object `caadr` extends PrimitiveDefs.CarCdrOperation("caadr")

    object `cadar` extends PrimitiveDefs.CarCdrOperation("cadar")

    object `caddr` extends PrimitiveDefs.CarCdrOperation("caddr")

    object `cdaar` extends PrimitiveDefs.CarCdrOperation("cdaar")

    object `cdadr` extends PrimitiveDefs.CarCdrOperation("cdadr")

    object `cddar` extends PrimitiveDefs.CarCdrOperation("cddar")

    object `cdddr` extends PrimitiveDefs.CarCdrOperation("cdddr")

    object `caaaar` extends PrimitiveDefs.CarCdrOperation("caaaar")

    object `caaadr` extends PrimitiveDefs.CarCdrOperation("caaadr")

    object `caadar` extends PrimitiveDefs.CarCdrOperation("caadar")

    object `caaddr` extends PrimitiveDefs.CarCdrOperation("caaddr")

    object `cadaar` extends PrimitiveDefs.CarCdrOperation("cadaar")

    object `cadadr` extends PrimitiveDefs.CarCdrOperation("cadadr")

    object `caddar` extends PrimitiveDefs.CarCdrOperation("caddar")

    object `cadddr` extends PrimitiveDefs.CarCdrOperation("cadddr")

    object `cdaaar` extends PrimitiveDefs.CarCdrOperation("cdaaar")

    object `cdaadr` extends PrimitiveDefs.CarCdrOperation("cdaadr")

    object `cdadar` extends PrimitiveDefs.CarCdrOperation("cdadar")

    object `cdaddr` extends PrimitiveDefs.CarCdrOperation("cdaddr")

    object `cddaar` extends PrimitiveDefs.CarCdrOperation("cddaar")

    object `cddadr` extends PrimitiveDefs.CarCdrOperation("cddadr")

    object `cdddar` extends PrimitiveDefs.CarCdrOperation("cdddar")

    object `cddddr` extends PrimitiveDefs.CarCdrOperation("cddddr")

    /** (define (equal? a b)
     * (or (eq? a b)
     * (and (null? a) (null? b))
     * (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
     * (and (vector? a) (vector? b)
     * (let ((n (vector-length a)))
     * (and (= (vector-length b) n)
     * (letrec ((loop (lambda (i)
     * (or (= i n)
     * (and (equal? (vector-ref a i) (vector-ref b i))
     * (loop (+ i 1)))))))
     * (loop 0)))))))
     */
    // TODO: this is without vectors
    @unsound("Returns bottom in incorrect cases.")
    object `equal?` extends Store2Operation("equal?") {
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
              ifThenElseTR((and _) (isNull(a), isNull(b))) {
                /* If both a and b are null, then they are equal? */
                done(bool(true))
              } {
                ifThenElseTR((and _) (isCons(a), isCons(b))) {
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
                  ifThenElseTR((and _) (isPointer(a), isPointer(b))) {
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
     * (if (null? l)
     * 0
     * (+ 1 (length (cdr l)))))
     */
    object `length` extends FixpointPrimitiveUsingStore("length", Some(1)) {
      /** the argument to length is just the current pair */
      type Args = V

      def initialArgs(fexp: Identity.Position, args: List[(Identity.Position, V)]) = args match {
        case (_, arg) :: Nil => Some(arg)
        case _ => None
      }

      /** calls are just that argument */
      type Call = Args

      def callFor(args: V) = args

      /** since Args == Calls, the argument remains the same */
      def updateArgs(oldArgs: V, newArgs: V) = {
        assert(newArgs == oldArgs); oldArgs
      }

      /** The actual implementation of the primitive */
      override def callWithArgs(l: V)(alloc: SchemeAllocator[A], store: Store[A, V], length: V => MayFail[V, Error]): MayFail[V, Error] =
        ifV(isNull(l)) {
          // if we have l = '(), length(l) = 0
          number(0)
        } otherwise ifV(isPointer(l)) {
          // if we have l = cons(a,d), length(l) = length(d) + 1
          dereferencePointer(l, store) { consv =>
            for {
              next <- cdr(consv)
              len_next <- length(next)
              result <- plus(len_next, number(1))
            } yield result
          }
        } otherwise {
          // if we have have something else (i.e., not a list), length throws a type error!
          MayFail.failure(PrimitiveNotApplicable("length", List(l)))
        }
    }

    /** (define (append l1 l2)
     * (if (null? l1)
     * l2
     * (cons (car l1)
     * (append (cdr l1) l2))))
     */
    object `append` extends FixpointPrimitiveUsingStore("append", Some(2)) {
      /** the arguments to append are the two given input arguments + the 'append expression' and 'the current index' into the list (used for allocation) */
      type Args = (V, V, Identity.Position, Int)

      def initialArgs(fexp: Identity.Position, args: List[(Identity.Position, V)]) = args match {
        case (_, l1) :: (_, l2) :: Nil => Some((l1, l2, fexp, 0))
        case _ => None
      }

      /** calls only take into account the current pair (= first argument of append) */
      type Call = V

      def callFor(args: Args) = args._1

      /** arguments: ignore changes to the index to ensure termination (and other args are guaranteed to be the same anyway) */
      def updateArgs(oldArgs: Args, newArgs: Args) = oldArgs

      /** The actual implementation of append */
      override def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A, V], append: Args => MayFail[V, Error]): MayFail[V, Error] = args match {
        case (l1, l2, fexp, idx) =>
          ifV(isNull(l1)) {
            // if we have l1 = '(), append(l1,l2) = l2
            l2
          } otherwise ifV(isPointer(l1)) {
            // if we have l1 = cons(a,d), append(l1,l2) = cons(a,append(d,l2))
            val addr = alloc.pointer((fexp, fexp), idx)
            dereferencePointer(l1, store) { consv =>
              for {
                carv <- lat.car(consv)
                cdrv <- lat.cdr(consv)
                app_next <- append((cdrv, l2, fexp, idx + 1))
                result <- lat.cons(carv, app_next)
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

    /** (define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l))) */
    object `list?` extends Store1Operation("list?") {
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
     * (if (pair? l)
     * (if (= index 0)
     * (car l)
     * (list-ref (cdr l) (- index 1)))
     * (error "list-ref applied to a non-list"))) */
    object `list-ref` extends Store2Operation("list-ref") {
      @unsound("Returns bottom in incorrect cases.")
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
     * (if (null? l)
     * #f
     * (if (equal? (car l) e)
     * l
     * (member e (cdr l))))) */
    abstract class MemberLike(
                               override val name: String,
                               eqFn: (V, V, Store[A, V]) => MayFail[V, Error]
                             ) extends Store2Operation(name) {
      @unsound("Returns bottom in incorrect cases.")
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

    @unsound("Returns bottom in incorrect cases.")
    object `member`
      extends MemberLike(
        "member",
        (x: V, y: V, store: Store[A, V]) => `equal?`.call(x, y, store).map(_._1)
      )

    @unsound("Returns bottom in incorrect cases.")
    object `memq` extends MemberLike("memq", (x: V, y: V, store: Store[A, V]) => PrimitiveDefs.`eq?`.call(x, y))

    abstract class AssocLike(
                              override val name: String,
                              eqFn: (V, V, Store[A, V]) => MayFail[V, Error]
                            ) extends Store2Operation(name) {
      @unsound("Returns bottom in incorrect cases.")
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

    @unsound("Returns bottom in incorrect cases.")
    object `assoc`
      extends AssocLike(
        "assoc",
        (x: V, y: V, store: Store[A, V]) => `equal?`.call(x, y, store).map(_._1)
      )

    @unsound("Returns bottom in incorrect cases.")
    object `assq` extends AssocLike("assq", (x: V, y: V, store: Store[A, V]) => PrimitiveDefs.`eq?`.call(x, y))

  }

}
