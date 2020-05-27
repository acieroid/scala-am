package scalaam.language.scheme.primitives

import scalaam.core._
import scalaam.language.scheme._

trait SchemeAllocator[A] {
  def pointer(exp: SchemeExp): A
}

trait SchemePrimitive[V, A <: Address] extends Primitive {
  def call(fexp: SchemeExp,
           args: List[(SchemeExp, V)],
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
