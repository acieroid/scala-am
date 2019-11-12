package scalaam.core

import scalaam.util.SmartHash

case class UnboundAddress[A <: Address](a: A) extends Error

trait Store[A <: Address, V] extends SmartHash {

  def content: Map[A, V]

  /** Gets all the keys of the store */
  def keys: Iterable[A]

  /** Restrict the store to only certain keys */
  def restrictTo(keys: Set[A]): Store[A, V]

  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((A, V)) => Boolean): Boolean

  /** Looks up a value in the store */
  def lookup(a: A): Option[V]
  def lookupDefault(a: A, default: V): V = lookup(a) match {
    case Some(a) => a
    case None    => default
  }
  def lookupMF(a: A): MayFail[V, Error] = lookup(a) match {
    case Some(a) => MayFail.success(a)
    case None    => MayFail.failure(UnboundAddress(a))
  }

  /** Add a new entry in the store */
  def extend(a: A, v: V): Store[A, V]

  /** Update an entry in the store */
  def update(a: A, v: V): Store[A, V] = extend(a, v)

  /** Tries to update an address if it's already mapped into the store. Otherwise, extend the store */
  def updateOrExtend(a: A, v: V): Store[A, V] = extend(a, v)

  /** Joins two stores together */
  def join(that: Store[A, V]): Store[A, V]

  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[A, V]): Boolean
}
