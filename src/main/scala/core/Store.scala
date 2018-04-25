import scalaz._
import scalaz.Scalaz._
import scalaz.Semigroup

abstract class Store[Addr : Address, Abs : JoinLattice] {
  /** Gets all the keys of the store */
  def keys: Iterable[Addr]
  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((Addr, Abs)) => Boolean): Boolean
  /** Looks up a value in the store */
  def lookup(a: Addr): Option[Abs]
  def lookupMF(a: Addr): MayFail[Abs] = lookup(a) match {
    case Some(a) => a
    case None => UnboundAddress(a.toString)
  }
  /** Looks up a  value in the store, or return bottom if it's not there */
  def lookupBot(a: Addr): Abs
  /** Add a new entry in the store */
  def extend(a: Addr, v: Abs): Store[Addr, Abs]
  /** Update an entry in the store */
  def update(a: Addr, v: Abs): Store[Addr, Abs]
  /** Tries to update an address if it's already mapped into the store. Otherwise, extend the store */
  def updateOrExtend(a: Addr, v: Abs): Store[Addr, Abs]
  /** Joins two stores together */
  def join(that: Store[Addr, Abs]): Store[Addr, Abs]
  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[Addr, Abs]): Boolean
  /** Returns a store containing items that differ between the two stores */
  def diff(that: Store[Addr, Abs]): Store[Addr, Abs]
  /** Returns a delta of the changes made on this store. None if the store doesn't support store deltas */
  def delta: Option[Map[Addr, Abs]] = None
  /** Add a delta to the store. This clears the current delta */
  def addDelta(delta: Map[Addr, Abs]): Store[Addr, Abs] = throw new Exception("Store doesn't support deltas")
  /** Returns the cardinality of each lattice element of this store */
  def cardinalities(withPrimitives: Boolean = false): Map[Addr, Cardinality] =
    keys
      .filter(a => withPrimitives || !Address[Addr].isPrimitive(a))
      .foldLeft(Map[Addr, Cardinality]())((acc, a) => lookup(a) match {
        case Some(v) => acc + (a -> JoinLattice[Abs].cardinality(v))
        case None => acc /* should not happen */
      })
}

/** Basic store with no fancy feature, just a map from addresses to values */
case class BasicStore[Addr : Address, Abs : JoinLattice](content: Map[Addr, Abs]) extends Store[Addr, Abs] {
  override def toString = content.filterKeys(a => !Address[Addr].isPrimitive(a)).toString
  def keys = content.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({ case (a, v) => p(a, v) })
  def lookup(a: Addr) = content.get(a)
  def lookupBot(a: Addr) = content.get(a).getOrElse(JoinLattice[Abs].bottom)
  def extend(a: Addr, v: Abs) = content.get(a) match {
    case None => this.copy(content = content + (a -> v))
    case Some(v2) => this.copy(content = content + (a -> JoinLattice[Abs].join(v2, v)))
  }
  def update(a: Addr, v: Abs) = extend(a, v)
  def updateOrExtend(a: Addr, v: Abs) = extend(a, v)
  def join(that: Store[Addr, Abs]) =
    if (that.isInstanceOf[BasicStore[Addr, Abs]]) {
      this.copy(content = content |+| that.asInstanceOf[BasicStore[Addr, Abs]].content)
    } else {
      throw new Exception(s"Incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
    }
  def subsumes(that: Store[Addr, Abs]) =
    that.forall((binding: (Addr, Abs)) => JoinLattice[Abs].subsumes(lookupBot(binding._1), binding._2))
  def diff(that: Store[Addr, Abs]) =
    this.copy(content = content.filter({ case (a, v) => that.lookupBot(a) != v}))
}

/** Store that combines a default read-only store with a writable store */
case class CombinedStore[Addr : Address, Abs : JoinLattice](ro: Store[Addr, Abs], w: Store[Addr, Abs]) extends Store[Addr, Abs] {
  def keys = ro.keys.toSet ++ w.keys.toSet
  def forall(p: ((Addr, Abs)) => Boolean) = keys.forall(a => lookup(a) match {
    case Some(v) => p((a, v))
    case None => throw new Exception(s"shouldn't happen: an existing key is not bound in the store (key: $a, store: $this)")
  })
  def lookup(a: Addr) = w.lookup(a).orElse(ro.lookup(a))
  def lookupBot(a: Addr) = w.lookup(a).getOrElse(ro.lookupBot(a))
  def extend(a: Addr, v: Abs) = this.copy(w = w.extend(a, v))
  def update(a: Addr, v: Abs) = updateOrExtend(a, v)
  def updateOrExtend(a: Addr, v: Abs) = this.copy(w = w.updateOrExtend(a, v))
  def join(that: Store[Addr, Abs]) = throw new Exception("CombinedStore does not support join")
  def subsumes(that: Store[Addr, Abs]) =
    that.forall((binding: (Addr, Abs)) => JoinLattice[Abs].subsumes(lookupBot(binding._1), binding._2))
  def diff(that: Store[Addr, Abs]) = throw new Exception("CombinedStore does not support diff")
}

/** A store that supports store deltas. Many operations are not implemented because they are not needed. */
case class DeltaStore[Addr : Address, Abs : JoinLattice](content: Map[Addr, Abs], d: Map[Addr, Abs]) extends Store[Addr, Abs] {
  def this() = this(Map(), Map())
  override def toString = content.filterKeys(a => !Address[Addr].isPrimitive(a)).toString
  def keys = content.keys ++ d.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({ case (a, v) => p(a, v) })
  def lookup(a: Addr) =
    d.get(a).orElse(content.get(a)) /* information in the delta should always be as broad as the information in the store itself */
  def lookupBot(a: Addr) = d.get(a).orElse(content.get(a)).getOrElse(JoinLattice[Abs].bottom)
  def extend(a: Addr, v: Abs) = d.get(a) match {
    case Some(v2) => this.copy(d = d + (a -> JoinLattice[Abs].join(v2, v)))
    case None => content.get(a) match {
      case None => this.copy(d = d + (a -> v))
      case Some(v2) if v2 == v || JoinLattice[Abs].subsumes(v2, v) => this
      case Some(v2) => this.copy(d = d + (a -> JoinLattice[Abs].join(v2, v)))
    }
  }
  def update(a: Addr, v: Abs) = extend(a, v)
  def updateOrExtend(a: Addr, v: Abs) = extend(a, v)
  def join(that: Store[Addr, Abs]) = if (that.isInstanceOf[DeltaStore[Addr, Abs]]) {
    throw new Exception("DeltaStore does not support join")
  } else {
    throw new Exception(s"Incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
  }
  def subsumes(that: Store[Addr, Abs]) = throw new Exception("DeltaStore does not support subsumes")
  def diff(that: Store[Addr, Abs]) = throw new Exception("DeltaStore does not support diff")
  override def delta = Some(d)
  override def addDelta(delta: Map[Addr, Abs]) = this.copy(content = content |+| delta, d = Map())
}

case class CountingStore[Addr : Address, Abs : JoinLattice](content: Map[Addr, (Count, Abs)]) extends Store[Addr, Abs] {
  override def toString = content.filterKeys(a => !Address[Addr].isPrimitive(a)).toString
  def keys = content.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({ case (a, (_, v)) => p(a, v) })
  def lookup(a: Addr) = content.get(a).map(_._2)
  def lookupBot(a: Addr) = content.get(a).map(_._2).getOrElse(JoinLattice[Abs].bottom)
  def extend(a: Addr, v: Abs) = content.get(a) match {
    case None => this.copy(content = content + (a -> (CountOne, v)))
    case Some((n, v2)) => this.copy(content = content + (a -> (n.inc, JoinLattice[Abs].join(v2, v))))
  }
  def update(a: Addr, v: Abs) = content.get(a) match {
    case None => throw new RuntimeException("Updating store at an adress not used")
    case Some((CountOne, _)) => this.copy(content = content + (a -> (CountOne, v)))
    case _ => extend(a, v)
  }
  def updateOrExtend(a: Addr, v: Abs) = content.get(a) match {
    case None => extend(a, v)
    case Some(_) => update(a, v)
  }
  def join(that: Store[Addr, Abs]) =
    if (that.isInstanceOf[CountingStore[Addr, Abs]]) {
      this.copy(content = content |+| that.asInstanceOf[CountingStore[Addr, Abs]].content)
    } else {
      throw new Exception(s"Incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
    }
  def subsumes(that: Store[Addr, Abs]) =
    that.forall((binding: (Addr, Abs)) => JoinLattice[Abs].subsumes(lookupBot(binding._1), binding._2))
  def diff(that: Store[Addr, Abs]) =
    if (that.isInstanceOf[CountingStore[Addr, Abs]]) {
      val other = that.asInstanceOf[CountingStore[Addr, Abs]]
      this.copy(content = content.filter({ case (a, (n, v)) => other.content.get(a) match {
        case Some((n2, v2)) => n != n2 && v != v2
        case None => true
      }}))
    } else {
      this.copy(content = content.filter({ case (a, v) => that.lookupBot(a) != v}))
    }
}

object Store {
  def empty[Addr : Address, Abs : JoinLattice]: Store[Addr, Abs] = empty[Addr, Abs](JoinLattice[Abs].counting)
  def empty[Addr : Address, Abs : JoinLattice](counting: Boolean): Store[Addr, Abs] = if (counting) {
    CountingStore(Map())
  } else {
    BasicStore(Map())
  }
  def initial[Addr : Address, Abs : JoinLattice](values: Iterable[(Addr, Abs)]): Store[Addr, Abs] = if (JoinLattice[Abs].counting) {
    CountingStore(values.map({ case (a, v) => (a, (CountOne, v)) }).toMap)
  } else {
    BasicStore(values.toMap)
  }

  implicit def monoid[Addr : Address, Abs : JoinLattice]: Monoid[Store[Addr, Abs]] =
    new Monoid[Store[Addr, Abs]] {
      def append(s1: Store[Addr, Abs], s2: => Store[Addr, Abs]): Store[Addr, Abs] = s1.join(s2)
      def zero: Store[Addr, Abs] = empty[Addr, Abs]
    }
}
