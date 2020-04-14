package scalaam.language.scheme.primitives

import scalaam.core._
import scalaam.language.scheme._
import scalaam.util.Monoid

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

case class PrimitiveArityError(name: String, expectd: Int, given: Int)               extends ArityError(name, expectd, given)
case class PrimitiveVariadicArityError(name: String, expectedAtLeast: Int, got: Int) extends Error
case class PrimitiveNotApplicable[V](name: String, args: List[V])                    extends OperatorNotApplicable(name, args)
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
  def isNull         = unaryOp(SchemeOps.UnaryOperator.IsNull) _
  def isCons         = unaryOp(SchemeOps.UnaryOperator.IsCons) _
  def isPointer      = unaryOp(SchemeOps.UnaryOperator.IsPointer) _
  def isChar         = unaryOp(SchemeOps.UnaryOperator.IsChar) _
  def isSymbol       = unaryOp(SchemeOps.UnaryOperator.IsSymbol) _
  def isString       = unaryOp(SchemeOps.UnaryOperator.IsString) _
  def isInteger      = unaryOp(SchemeOps.UnaryOperator.IsInteger) _
  def isReal         = unaryOp(SchemeOps.UnaryOperator.IsReal) _
  def isBoolean      = unaryOp(SchemeOps.UnaryOperator.IsBoolean) _
  def isVector       = unaryOp(SchemeOps.UnaryOperator.IsVector) _
  def lat_ceiling    = unaryOp(SchemeOps.UnaryOperator.Ceiling) _
  def lat_floor      = unaryOp(SchemeOps.UnaryOperator.Floor) _
  def lat_round      = unaryOp(SchemeOps.UnaryOperator.Round) _
  def lat_log        = unaryOp(SchemeOps.UnaryOperator.Log) _
  def lat_not        = unaryOp(SchemeOps.UnaryOperator.Not) _
  def lat_random     = unaryOp(SchemeOps.UnaryOperator.Random) _
  def lat_sin        = unaryOp(SchemeOps.UnaryOperator.Sin) _
  def lat_asin       = unaryOp(SchemeOps.UnaryOperator.ASin) _
  def lat_cos        = unaryOp(SchemeOps.UnaryOperator.Cos) _
  def lat_acos       = unaryOp(SchemeOps.UnaryOperator.ACos) _
  def lat_tan        = unaryOp(SchemeOps.UnaryOperator.Tan) _
  def lat_atan       = unaryOp(SchemeOps.UnaryOperator.ATan) _
  def lat_sqrt       = unaryOp(SchemeOps.UnaryOperator.Sqrt) _
  def vectorLength   = unaryOp(SchemeOps.UnaryOperator.VectorLength) _
  def stringLength   = unaryOp(SchemeOps.UnaryOperator.StringLength) _
  def numberToString = unaryOp(SchemeOps.UnaryOperator.NumberToString) _
  def symbolToString = unaryOp(SchemeOps.UnaryOperator.SymbolToString) _
  def stringToSymbol = unaryOp(SchemeOps.UnaryOperator.StringToSymbol) _
  def inexactToExact = unaryOp(SchemeOps.UnaryOperator.InexactToExact) _
  def exactToInexact = unaryOp(SchemeOps.UnaryOperator.ExactToInexact) _
  def characterToInt = unaryOp(SchemeOps.UnaryOperator.CharacterToInteger) _

  def plus          = binaryOp(SchemeOps.BinaryOperator.Plus) _
  def minus         = binaryOp(SchemeOps.BinaryOperator.Minus) _
  def times         = binaryOp(SchemeOps.BinaryOperator.Times) _
  def div           = binaryOp(SchemeOps.BinaryOperator.Div) _
  def lat_expt      = binaryOp(SchemeOps.BinaryOperator.Expt) _
  def lat_quotient  = binaryOp(SchemeOps.BinaryOperator.Quotient) _
  def lat_modulo    = binaryOp(SchemeOps.BinaryOperator.Modulo) _
  def lat_remainder = binaryOp(SchemeOps.BinaryOperator.Remainder) _
  def lt            = binaryOp(SchemeOps.BinaryOperator.Lt) _
  def numEq         = binaryOp(SchemeOps.BinaryOperator.NumEq) _
  def eqq           = binaryOp(SchemeOps.BinaryOperator.Eq) _
  def stringAppend  = binaryOp(SchemeOps.BinaryOperator.StringAppend) _
  def stringRef     = binaryOp(SchemeOps.BinaryOperator.StringRef) _
  def stringLt      = binaryOp(SchemeOps.BinaryOperator.StringLt) _

  def ifThenElse(cond: MayFail[V, Error])(thenBranch: => MayFail[V, Error])(elseBranch: => MayFail[V, Error]): MayFail[V, Error] = {
    cond >>= { condv =>
      val t = if (isTrue (condv)) thenBranch else MayFail.success[V, Error](latMon.zero)
      val f = if (isFalse(condv)) elseBranch else MayFail.success[V, Error](latMon.zero)
      mfMon.append(t, f)
    }
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
}
