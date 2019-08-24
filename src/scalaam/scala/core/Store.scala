package scalaam.core

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

/** Basic store with no fancy feature, just a map from addresses to values */
case class BasicStore[A <: Address, V](val content: Map[A, V])(implicit val lat: Lattice[V])
    extends Store[A, V] {
  override def toString              = content.view.filterKeys(_.printable).mkString("\n")
  def keys                           = content.keys
  def restrictTo(keys: Set[A])       = BasicStore(content.view.filterKeys(a => keys.contains(a)).toMap)
  def forall(p: ((A, V)) => Boolean) = content.forall({ case (a, v) => p((a, v)) })
  def lookup(a: A)                   = content.get(a)
  def extend(a: A, v: V) = content.get(a) match {
    case None     => new BasicStore[A, V](content + (a -> v))
    case Some(v2) => new BasicStore[A, V](content + (a -> lat.join(v, v2)))
  }
  def join(that: Store[A, V]) =
    keys.foldLeft(that)((acc, k) => lookup(k).fold(acc)(v => acc.extend(k, v)))
  def subsumes(that: Store[A, V]) =
    that.forall((binding: (A, V)) =>
      content.get(binding._1).exists(v => lat.subsumes(v, binding._2)))
}

object Profiler {
  val methodsCount: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map().withDefaultValue(0)
  val methodsTotalTime: scala.collection.mutable.Map[String, Long] = scala.collection.mutable.Map().withDefaultValue(0)
  def profileCall[T](name: String)(f: => T): T = {
    val t0 = System.nanoTime()
    val res = f
    val t1 = System.nanoTime()
    methodsCount += (name -> (methodsCount(name) + 1))
    methodsTotalTime += (name -> (methodsTotalTime(name) + (t1 - t0)))
    res
  }
  def printResults(): Unit = {
    // TODO: sort results per total time?
    methodsCount.keySet.foreach(k =>
      println(s"$k has been called ${methodsCount(k)} times for a total time of ${methodsTotalTime(k) / 1000000}ms")
    )
  }
}

case class ProfiledStore[A <: Address, V](val store: Store[A, V]) extends Store[A, V] {
  import Profiler.profileCall
  override def toString = profileCall("store.toString") { store.toString }
  def content = profileCall("store.content") { store.content }
  def keys = profileCall("store.keys") { store.keys }
  def restrictTo(keys: Set[A]) = profileCall("store.restrictTo") { ProfiledStore(store.restrictTo(keys)) }
  def forall(p: ((A, V)) => Boolean) = profileCall("store.forall") { store.forall(p) }
  def lookup(a: A) = profileCall("store.lookup") { store.lookup(a) }
  def extend(a: A, v: V) = profileCall("store.extend") { ProfiledStore(store.extend(a, v)) }
  def join(that: Store[A, V]) = profileCall("store.join") {
    that match {
      case ProfiledStore(store2) => ProfiledStore(store.join(store2))
      case _ => ProfiledStore(store.join(that))
    }
  }
  def subsumes(that: Store[A, V]) = profileCall("store.subsumes") { store.subsumes(that) }
}

object Store {
  def initial[A <: Address, V: Lattice](values: Iterable[(A, V)]): Store[A, V] =
    new ProfiledStore(BasicStore(values.toMap))
  def empty[A <: Address, V: Lattice]: Store[A, V] = initial[A, V](List())
}
