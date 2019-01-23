package scalaam.core

case class UnboundAddress[A <: Address](a: A) extends Error

trait Store[A <: Address, V] extends SmartHash {

  def content: Map[A, V]

  /** Gets all the keys of the store */
  def keys: Iterable[A]

  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((A, V)) => Boolean): Boolean

  /** Looks up a value in the store */
  def lookup(a: A): Option[V]
  def lookupDefault(a: A, default: V): V
  def lookupMF(a: A): MayFail[V, Error]

  /** Add a new entry in the store */
  def extend(a: A, v: V): Store[A, V]

  /** Update an entry in the store */
  def update(a: A, v: V): Store[A, V]

  /** Tries to update an address if it's already mapped into the store. Otherwise, extend the store */
  def updateOrExtend(a: A, v: V): Store[A, V]

  /** Joins two stores together */
  def join(that: Store[A, V]): Store[A, V]

  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[A, V]): Boolean
}

/** Basic store with no fancy feature, just a map from addresses to values */
case class BasicStore[A <: Address, V](val content: Map[A, V])(implicit val lat: Lattice[V])
    extends Store[A, V] {
  override def toString               = content.filterKeys(_.printable).mkString("\n")
  def keys                            = content.keys
  def forall(p: ((A, V)) => Boolean)  = content.forall({ case (a, v) => p((a, v)) })
  def lookup(a: A)                    = content.get(a)
  def lookupDefault(a: A, default: V) = content.get(a) match {
    case Some(a) => a
    case None => default
  }
  def lookupMF(a: A) = content.get(a) match {
    case Some(a) => MayFail.success(a)
    case None    => MayFail.failure(UnboundAddress(a))
  }
  def extend(a: A, v: V) = content.get(a) match {
    case None     => new BasicStore[A, V](content + (a -> v))
    case Some(v2) => new BasicStore[A, V](content + (a -> lat.join(v, v2)))
  }
  def update(a: A, v: V)         = extend(a, v)
  def updateOrExtend(a: A, v: V) = extend(a, v)
  def join(that: Store[A, V]) =
    keys.foldLeft(that)((acc, k) => lookup(k).fold(acc)(v => acc.extend(k, v)))
  def subsumes(that: Store[A, V]) =
    that.forall((binding: (A, V)) =>
      content.get(binding._1).exists(v => lat.subsumes(v, binding._2)))
}

object Store {
  def empty[A <: Address, V: Lattice]: Store[A, V] = new BasicStore(Map())
  def initial[A <: Address, V: Lattice](values: Iterable[(A, V)]): Store[A, V] =
    new BasicStore(values.toMap)
}
