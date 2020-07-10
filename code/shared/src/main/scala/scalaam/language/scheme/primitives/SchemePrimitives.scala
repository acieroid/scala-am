package scalaam.language.scheme.primitives

import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.CScheme._

trait SchemeInterpreterBridge[A] {
  def pointer(exp: SchemeExp): A
  def currentThread: TID
}

trait SchemePrimitive[V, A <: Address] extends Primitive {
  // Every primitive in Scheme has a unique name
  def name: String
  // They can be called given the arguments, expressions, store and some interface to the Scheme interpreter
  def call(fexp: SchemeExp,
           args: List[(SchemeExp, V)],
           store: Store[A, V],
           scheme: SchemeInterpreterBridge[A]): MayFail[(V, Store[A, V]), Error]
}

case class PrimitiveArityError(name: String, expected: Int, got: Int)                extends Error
case class PrimitiveVariadicArityError(name: String, expectedAtLeast: Int, got: Int) extends Error
case class PrimitiveNotApplicable[V](name: String, args: List[V])                    extends Error
case class UserError(message: String)                                                extends Error

abstract class SchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A]]) {
  def allPrimitives: List[SchemePrimitive[V, A]]
}
