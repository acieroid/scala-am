package scalaam.language.scheme

import scalaam.core._

trait SchemeAllocator[A] {
  def pointer(exp: SchemeExp): A
  def carAddr(exp: SchemeExp): A
  def cdrAddr(exp: SchemeExp): A
}

trait SchemePrimitive[V, A <: Address] extends Primitive {
  def call(fexp: SchemeExp,
           args: List[(SchemeExp, V)],
           store: Store[A, V],
           alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error]
}

class SchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) {

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
      // Append, /* [x]  append: Append/Reverse */
      /* [x]  apply: Fly Evaluation */
      ASin, /* [vv] asin: Scientific */
      //Assoc, /* [vv] assoc: Retrieving Alist Entries */
      //Assq, /* [vv] assq: Retrieving Alist Entries */
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
      //Equal, /* [vx] equal?: Equality */
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
      //ListRef, /* [vv] list-ref: List Selection */
      /* [x]  list-tail: List Selection */
      //Listp, /* [vv] list?: List Predicates */
      /* [x]  load: Loading */
      Log, /* [vv] log: Scientific */
      /* [x]  magnitude: Complex */
      /* [x]  make-polar: Complex */
      /* [x]  make-rectangular: Complex */
      /* [x]  make-string: String Constructors */
      /* [x]  map: List Mapping */
      Max, /* [vv] max: Arithmetic */
      //Member, /* [vv] member: List Searching */
      //Memq, /* [v]  memq: List Searching */
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
      def call(arg1: V, arg2: V): MayFail[V, Error] = call(List(arg1, arg2))
      def call(arg1: (SchemeExp, V), arg2: (SchemeExp, V)): MayFail[V, Error] =
        call(arg1._2, arg2._2)
      def call(fexp: SchemeExp, arg1: (SchemeExp, V), arg2: (SchemeExp, V)): MayFail[V, Error] =
        call(arg1, arg2)
      def call(arg: V): MayFail[V, Error]                               = call(List(arg))
      def call(arg: (SchemeExp, V)): MayFail[V, Error]                  = call(arg._2)
      def call(fexp: SchemeExp, arg: (SchemeExp, V)): MayFail[V, Error] = call(arg)
      def call(): MayFail[V, Error]                                     = call(List())
      def call(
          fexp: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] =
        (args match {
          case Nil           => call()
          case x :: Nil      => call(fexp, x)
          case x :: y :: Nil => call(fexp, x, y)
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
      def call(
          fexp: SchemeExp,
          arg1: (SchemeExp, V),
          arg2: (SchemeExp, V),
          store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg1._2, arg2._2, store)
      def call(
          fexp: SchemeExp,
          arg: (SchemeExp, V),
          store: Store[A, V]
      ): MayFail[(V, Store[A, V]), Error] =
        call(arg._2, store)
      def call(store: Store[A, V]): MayFail[(V, Store[A, V]), Error] = call(List(), store)
      def call(
          fexp: SchemeExp,
          args: List[(SchemeExp, V)],
          store: Store[A, V],
          alloc: SchemeAllocator[A]
      ): MayFail[(V, Store[A, V]), Error] = args match {
        case Nil           => call(store)
        case x :: Nil      => call(fexp, x, store)
        case x :: y :: Nil => call(fexp, x, y, store)
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
      def initialArgs(fexp: SchemeExp, argsWithExps: List[(SchemeExp, V)]): Option[Args]
      // - a function for updating the arguments when upon a new call to a 'call'
      def updateArgs(oldArgs: Args, newArgs: Args): Args
      // - (optional) a function for updating the result of a function call
      def updateResult(oldResult: MayFail[V,Error], newResult: MayFail[V,Error]): MayFail[V, Error] = mfMon.append(oldResult, newResult)
      // - a function to execute a single 'call' with given arguments
      def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[V,Error]

      def call(fexp: SchemeExp,
               argsWithExps: List[(SchemeExp, V)],
               store: Store[A,V],
               alloc: SchemeAllocator[A]): MayFail[(V,Store[A,V]), Error] = {
        // determine the initial args & call from the primitive input
        val initArgs = initialArgs(fexp, argsWithExps) match {
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

      def call(fexp: SchemeExp,
               argsWithExps: List[(SchemeExp, V)],
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
      def callWithArgs(args: Args, store: Store[A,V], cache: Args => MayFail[V,Error]): MayFail[(V, Store[A,V]),Error]

      def call(fexp: SchemeExp,
               argsWithExps: List[(SchemeExp, V)],
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
        var curStore = store
        while (worklist.nonEmpty) {
          // take the next arguments from the worklist
          val nextArgs = worklist.head
          worklist = worklist - nextArgs
          // call with the next arguments
          val res = callWithArgs(nextArgs, store, args => {
            deps += (args -> (deps(args) + nextArgs))
            if (cache.get(args).isEmpty) worklist = worklist + args
            cache(args)
          })
          // update the cache, worklist and store
          val oldValue = cache(nextArgs)
          val updatedValue = mfMon.append(oldValue, res >>= {case (vl, store) => curStore = store ; vl})
          if (updatedValue != oldValue) {
            cache += (nextArgs -> updatedValue)
            worklist ++= deps(nextArgs)
          }
        }
        cache(initArgs).map((_, curStore))
      }
    }

    def liftTailRec(x: MayFail[TailRec[MayFail[V, Error]], Error]): TailRec[MayFail[V, Error]] =
      x match {
        case MayFailSuccess(v)   => tailcall(v)
        case MayFailError(err)   => done(MayFailError(err))
        case MayFailBoth(v, err) => tailcall(v).map(_.addErrors(err))
      }

    def dereferenceAddrs(addrs: Set[A], store: Store[A,V]): MayFail[V,Error] =
      addrs.foldLeft(MayFail.success[V,Error](bottom))(
        (acc: MayFail[V,Error], addr: A) =>
          for {
            v <- store.lookupMF(addr)
            accv <- acc 
          } yield join(accv,v)
      )
    
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
    object Pairp extends NoStoreOperation("pair?", Some(1)) {
      /* TODO[easy]: this should be a store operation that dereferences the pointer to check if it is indeed a cons */
      override def call(x: V) = isCons(x)
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
        case l             => MayFail.failure(PrimitiveArityError(name, 2, l.size))
      }
    }
    object StringLt extends NoStoreOperation("string<?", Some(2)) {
      override def call(x: V, y: V) = stringLt(x, y)
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
      override def call(fexp: SchemeExp, x: (SchemeExp, V)) =
        MayFail.failure(UserError(x._2.toString))
    }

    object Cons extends SchemePrimitive[V,A] {
      val name = "cons"
      def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = args match {
        case (_, car) :: (_, cdr) :: Nil =>
          val carAddr = alloc.carAddr(fexp)
          val cdrAddr = alloc.cdrAddr(fexp)
          val consVal = cons(carAddr,cdrAddr)
          (consVal, store.extend(carAddr, car).extend(cdrAddr, cdr))
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
                consv <- acc
                addrs = op match {
                  case Car => car(consv)
                  case Cdr => cdr(consv)
                }
                res <- dereferenceAddrs(addrs, store)
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
        car(cell)
          .foldLeft(store)((acc,addr) => acc.update(addr, value))
          .map(store => (bool(false) /* undefined */, store))
    }
    object SetCdr extends StoreOperation("set-cdr!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        cdr(cell)
          .foldLeft(store)((acc,addr) => acc.update(addr, value))
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

    /** (define (length l)
          (if (null? l)
              0
              (+ 1 (length (cdr l)))))
    */
    object Length extends FixpointPrimitiveUsingStore("length",Some(1)) {
      /** the argument to length is just the current pair */
      type Args = V
      def initialArgs(fexp: SchemeExp, args: List[(SchemeExp, V)]) = args match {
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
        } otherwise ifV(isCons(l)) {
          // if we have l = cons(a,d), length(l) = length(d) + 1
          val next = dereferenceAddrs(cdr(l), store)
          for {
            len_next  <- length(next)
            result    <- plus(len_next, number(1))
          } yield result
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

    /** (define list (lambda args args))*/
    object ListPrim extends StoreOperation("list", None) {
      override def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], alloc: SchemeAllocator[A]) =
        args match {
          case Nil => (nil, store)
          case (exp, v) :: rest =>
            for {
              (restv, store2) <- call(fexp, rest, store, alloc)
            } yield {
              val carAddr = alloc.carAddr(exp)
              val cdrAddr = alloc.cdrAddr(exp)
              val updatedStore = store.extend(carAddr, v)
                                      .extend(cdrAddr, restv)
              (cons(carAddr,cdrAddr), updatedStore)
            }
        }
    }

    /** (define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l))) */
    /** (define (list-ref l index)
          (if (pair? l)
            (if (= index 0)
              (car l)
              (list-ref (cdr l) (- index 1)))
            (error "list-ref applied to a non-list"))) */

    /** (define (member e l) ; member, memq and memv are similar, the difference lies in the comparison function used
          (if (null? l)
            #f
            (if (equal? (car l) e)
              l
              (member e (cdr l))))) */

    object MakeVector extends SchemePrimitive[V,A] {
      val name = "make-vector"
      def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        def createVec(size: V, init: V): MayFail[(V, Store[A, V]), Error] = {
          isInteger(size) >>= (
              isint =>
                if (isTrue(isint)) {
                  val veca = alloc.pointer(fexp)
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
      def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], alloc: SchemeAllocator[A]) = {
        val veca = alloc.pointer(fexp)
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
