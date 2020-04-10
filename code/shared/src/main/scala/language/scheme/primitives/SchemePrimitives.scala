package scalaam.language.scheme.primitives

import scalaam.core._
import scalaam.language.scheme._
import scalaam.util.Monoid

import scala.util.control.TailCalls.{TailRec, done, tailcall}

trait SchemeAllocator[A] {
  def pointer(exp: Identity): A
  def carAddr(exp: Identity): A
  def cdrAddr(exp: Identity): A
}

trait SchemePrimitive[V, A <: Address] extends Primitive {
  def call(fpos: Identity,
           args: List[(Identity, V)],
           store: Store[A, V],
           alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error]
}

case class PrimitiveArityError(name: String, expected: Int, got: Int)                extends Error
case class PrimitiveVariadicArityError(name: String, expectedAtLeast: Int, got: Int) extends Error
case class PrimitiveNotApplicable[V](name: String, args: List[V])                    extends Error
case class UserError(message: String)                                                extends Error

abstract class SchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) {
  def allPrimitives: List[SchemePrimitive[V, A]]
}

trait PrimitiveBuildingBlocks[V, A <: Address] {

  val lat: SchemeLattice[V, A, SchemePrimitive[V,A], _]
  lazy val latMon: Monoid[V] = scalaam.util.MonoidInstances.latticeMonoid[V](lat)
  lazy val mfMon: Monoid[MayFail[V, Error]] = scalaam.util.MonoidInstances.mayFail[V](latMon)

  import lat._

  implicit def fromMF[X](x: X): MayFail[X, Error] = MayFail.success(x)

  /* Simpler names for lattice operations */
  def isNull         = lat.unaryOp(SchemeOps.UnaryOperator.IsNull) _
  def isCons         = lat.unaryOp(SchemeOps.UnaryOperator.IsCons) _
  def isPointer      = lat.unaryOp(SchemeOps.UnaryOperator.IsPointer) _
  def isChar         = lat.unaryOp(SchemeOps.UnaryOperator.IsChar) _
  def isSymbol       = lat.unaryOp(SchemeOps.UnaryOperator.IsSymbol) _
  def isString       = lat.unaryOp(SchemeOps.UnaryOperator.IsString) _
  def isInteger      = lat.unaryOp(SchemeOps.UnaryOperator.IsInteger) _
  def isReal         = lat.unaryOp(SchemeOps.UnaryOperator.IsReal) _
  def isBoolean      = lat.unaryOp(SchemeOps.UnaryOperator.IsBoolean) _
  def isVector       = lat.unaryOp(SchemeOps.UnaryOperator.IsVector) _
  def lat_ceiling        = lat.unaryOp(SchemeOps.UnaryOperator.Ceiling) _
  def lat_floor          = lat.unaryOp(SchemeOps.UnaryOperator.Floor) _
  def lat_round          = lat.unaryOp(SchemeOps.UnaryOperator.Round) _
  def lat_log            = lat.unaryOp(SchemeOps.UnaryOperator.Log) _
  def lat_not            = lat.unaryOp(SchemeOps.UnaryOperator.Not) _
  def lat_random         = lat.unaryOp(SchemeOps.UnaryOperator.Random) _
  def lat_sin            = lat.unaryOp(SchemeOps.UnaryOperator.Sin) _
  def lat_asin           = lat.unaryOp(SchemeOps.UnaryOperator.ASin) _
  def lat_cos            = lat.unaryOp(SchemeOps.UnaryOperator.Cos) _
  def lat_acos           = lat.unaryOp(SchemeOps.UnaryOperator.ACos) _
  def lat_tan            = lat.unaryOp(SchemeOps.UnaryOperator.Tan) _
  def lat_atan           = lat.unaryOp(SchemeOps.UnaryOperator.ATan) _
  def lat_sqrt           = lat.unaryOp(SchemeOps.UnaryOperator.Sqrt) _
  def vectorLength   = lat.unaryOp(SchemeOps.UnaryOperator.VectorLength) _
  def stringLength   = lat.unaryOp(SchemeOps.UnaryOperator.StringLength) _
  def numberToString = lat.unaryOp(SchemeOps.UnaryOperator.NumberToString) _
  def symbolToString = lat.unaryOp(SchemeOps.UnaryOperator.SymbolToString) _
  def stringToSymbol = lat.unaryOp(SchemeOps.UnaryOperator.StringToSymbol) _
  def inexactToExact = lat.unaryOp(SchemeOps.UnaryOperator.InexactToExact) _
  def exactToInexact = lat.unaryOp(SchemeOps.UnaryOperator.ExactToInexact) _
  def characterToInt = lat.unaryOp(SchemeOps.UnaryOperator.CharacterToInteger) _

  def plus         = lat.binaryOp(SchemeOps.BinaryOperator.Plus) _
  def minus        = lat.binaryOp(SchemeOps.BinaryOperator.Minus) _
  def times        = lat.binaryOp(SchemeOps.BinaryOperator.Times) _
  def div          = lat.binaryOp(SchemeOps.BinaryOperator.Div) _
  def lat_quotient     = lat.binaryOp(SchemeOps.BinaryOperator.Quotient) _
  def lat_modulo       = lat.binaryOp(SchemeOps.BinaryOperator.Modulo) _
  def lat_remainder    = lat.binaryOp(SchemeOps.BinaryOperator.Remainder) _
  def lt           = lat.binaryOp(SchemeOps.BinaryOperator.Lt) _
  def numEq        = lat.binaryOp(SchemeOps.BinaryOperator.NumEq) _
  def eqq          = lat.binaryOp(SchemeOps.BinaryOperator.Eq) _
  def stringAppend = lat.binaryOp(SchemeOps.BinaryOperator.StringAppend) _
  def stringRef    = lat.binaryOp(SchemeOps.BinaryOperator.StringRef) _
  def stringLt     = lat.binaryOp(SchemeOps.BinaryOperator.StringLt) _

  /*
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

  abstract class SimpleFixpointPrimitive(val name: String, arity: Option[Int]) extends SchemePrimitive[V,A] {
    type Args = List[V]

    // Executes a single call with given arguments.
    def callWithArgs(args: Args, cache: Args => MayFail[V,Error]): MayFail[V,Error]

    override def call(fpos: Identity,
                      argsWithExps: List[(Identity, V)],
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
  }*/

  /*
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
  */

  def liftTailRec(x: MayFail[TailRec[MayFail[V, Error]], Error]): TailRec[MayFail[V, Error]] =
    x match {
      case MayFailSuccess(v)   => tailcall(v)
      case MayFailError(err)   => done(MayFailError(err))
      case MayFailBoth(v, err) => tailcall(v).map(_.addErrors(err))
    }

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

  def ifThenElseTR(cond: MayFail[V, Error])(
    thenBranch: => TailRec[MayFail[V, Error]]
  )(elseBranch: => TailRec[MayFail[V, Error]]): TailRec[MayFail[V, Error]] = {
    val latMon = scalaam.util.MonoidInstances.latticeMonoid[V](lat)
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

  /*
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
    */
}
