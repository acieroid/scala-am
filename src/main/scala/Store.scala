import scalaz.Scalaz._
import scalaz.Semigroup

abstract class Store[Addr : Address, Abs : JoinLattice] {
  val abs = implicitly[JoinLattice[Abs]]
  val addr = implicitly[Address[Addr]]
  /** Gets all the keys of the store */
  def keys: Iterable[Addr]
  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((Addr, Abs)) => Boolean): Boolean
  /** Looks up a value in the store, raising an error if it's not there */
  def lookup(a: Addr): Abs
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
  /** Return a delta of the changes made on this store. None if the store doesn't support store deltas */
  def delta: Option[Map[Addr, Abs]] = None
  /** Add a delta to the store. This clears the current delta */
  def addDelta(delta: Map[Addr, Abs]): Store[Addr, Abs] = throw new Exception("Store doesn't support deltas")
}

/** Basic store with no fancy feature, just a map from addresses to values */
case class BasicStore[Addr : Address, Abs : JoinLattice](content: Map[Addr, Abs]) extends Store[Addr, Abs] {
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  def keys = content.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({ case (a, v) => p(a, v) })
  def lookup(a: Addr): Abs = content.get(a) match {
    case None => throw new Exception(s"Unbound address (should not happen): $a")
    case Some(v) => v
  }
  def lookupBot(a: Addr): Abs = content.get(a).getOrElse(abs.bottom)
  def extend(a: Addr, v: Abs): Store[Addr, Abs] = {
    //println(s"$this.extend($a, $v)")
    content.get(a) match {
    case None => this.copy(content = content + (a -> v))
    case Some(v2) => this.copy(content = content + (a -> abs.join(v2, v)))
    }
  }
  def update(a: Addr, v: Abs): Store[Addr, Abs] = extend(a, v)
  def updateOrExtend(a: Addr, v: Abs): Store[Addr, Abs] = extend(a, v)
  def join(that: Store[Addr, Abs]): Store[Addr, Abs] =
    if (that.isInstanceOf[BasicStore[Addr, Abs]]) {
      this.copy(content = content |+| that.asInstanceOf[BasicStore[Addr, Abs]].content)
    } else {
      throw new Exception(s"Incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
    }
  def subsumes(that: Store[Addr, Abs]): Boolean =
    that.forall((binding: (Addr, Abs)) => abs.subsumes(lookupBot(binding._1), binding._2))
  def diff(that: Store[Addr, Abs]): Store[Addr, Abs] =
    this.copy(content = content.filter({ case (a, v) => that.lookupBot(a) != v}))
}

/** A store that supports store deltas. Many operations are not implemented because they are not needed. */
case class DeltaStore[Addr : Address, Abs : JoinLattice](content: Map[Addr, Abs], d: Map[Addr, Abs]) extends Store[Addr, Abs] {
  def this() = this(Map(), Map())
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  def keys = content.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({ case (a, v) => p(a, v) })
  def lookup(a: Addr): Abs = d.get(a) match {
    case None => content.get(a) match {
      case None => throw new Exception(s"Unbound address (should not happen): $a")
      case Some(v) => v
    }
    case Some(v) => v /* information in the delta should always be as broad as the information in the store itself */
  }
  def lookupBot(a: Addr): Abs = d.get(a) match {
    case None => content.get(a).getOrElse(abs.bottom)
    case Some(v) => v
  }
  def extend(a: Addr, v: Abs): Store[Addr, Abs] = d.get(a) match {
    case Some(v2) => this.copy(d = d + (a -> abs.join(v2, v)))
    case None => content.get(a) match {
      case None => this.copy(d = d + (a -> v))
      case Some(v2) if v2 == v || abs.subsumes(v2, v) => this
      case Some(v2) => this.copy(d = d + (a -> abs.join(v2, v)))
    }
  }
  def update(a: Addr, v: Abs): Store[Addr, Abs] = extend(a, v)
  def updateOrExtend(a: Addr, v: Abs): Store[Addr, Abs] = extend(a, v)
  def join(that: Store[Addr, Abs]): Store[Addr, Abs] = if (that.isInstanceOf[DeltaStore[Addr, Abs]]) {
    throw new Exception("DeltaStore does not support join")
  } else {
    throw new Exception(s"Incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
  }
  def subsumes(that: Store[Addr, Abs]): Boolean = throw new Exception("DeltaStore does not support subsumes")
  def diff(that: Store[Addr, Abs]): Store[Addr, Abs] = throw new Exception("DeltaStore does not support diff")
  override def delta = Some(d)
  override def addDelta(delta: Map[Addr, Abs]) = this.copy(content = content |+| delta, d = Map())
}

/* Count values for counting store */
trait Count {
  def inc: Count
}
case object CountOne extends Count {
  def inc = CountInfinity
}
case object CountInfinity extends Count {
  def inc = CountInfinity
}

object Count {
  /* We need it to form a semigroup to use |+| to join stores */
  implicit val isSemigroup  = new Semigroup[Count] {
    def append(x: Count, y: => Count) = CountInfinity
  }
}

case class CountingStore[Addr : Address, Abs : JoinLattice](content: Map[Addr, (Count, Abs)]) extends Store[Addr, Abs] {
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  def keys = content.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({ case (a, (_, v)) => p(a, v) })
  def lookup(a: Addr): Abs = content.get(a) match {
    case None => throw new Exception(s"Unbound address (should not happen): $a")
    case Some(v) => v._2
  }
  def lookupBot(a: Addr): Abs = content.get(a).map(_._2).getOrElse(abs.bottom)
  def extend(a: Addr, v: Abs): Store[Addr, Abs] = content.get(a) match {
    case None => this.copy(content = content + (a -> (CountOne, v)))
    case Some((n, v2)) => this.copy(content = content + (a -> (n.inc, abs.join(v2, v))))
  }
  def update(a: Addr, v: Abs): Store[Addr, Abs] = content.get(a) match {
    case None => throw new RuntimeException("Updating store at an adress not used")
    case Some((CountOne, _)) => this.copy(content = content + (a -> (CountOne, v)))
    case _ => extend(a, v)
  }
  def updateOrExtend(a: Addr, v: Abs): Store[Addr, Abs] = content.get(a) match {
    case None => extend(a, v)
    case Some(_) => update(a, v)
  }
  def join(that: Store[Addr, Abs]): Store[Addr, Abs] =
    if (that.isInstanceOf[CountingStore[Addr, Abs]]) {
      this.copy(content = content |+| that.asInstanceOf[CountingStore[Addr, Abs]].content)
    } else {
      throw new Exception(s"Incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
    }
  def subsumes(that: Store[Addr, Abs]): Boolean =
    that.forall((binding: (Addr, Abs)) => abs.subsumes(lookupBot(binding._1), binding._2))
  def diff(that: Store[Addr, Abs]): Store[Addr, Abs] =
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
  def empty[Addr : Address, Abs : JoinLattice]: Store[Addr, Abs] = empty[Addr, Abs](implicitly[JoinLattice[Abs]].counting)
  def empty[Addr : Address, Abs : JoinLattice](counting: Boolean): Store[Addr, Abs] = if (counting) {
    CountingStore(Map())
  } else {
    BasicStore(Map())
  }
  def initial[Addr : Address, Abs : JoinLattice](values: Iterable[(Addr, Abs)]): Store[Addr, Abs] = if (implicitly[JoinLattice[Abs]].counting) {
    CountingStore(values.map({ case (a, v) => (a, (CountOne, v)) }).toMap)
  } else {
    BasicStore(values.toMap)
  }
}
