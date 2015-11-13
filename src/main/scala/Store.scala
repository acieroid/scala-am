import scalaz.Scalaz._

case class Store[Addr : Address, Abs : AbstractValue](content: Map[Addr, (Int, Abs)], counting: Boolean) {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  def keys: collection.Iterable[Addr] = content.keys
  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({
    case (a, (_, v)) => p(a, v)
  })
  def lookup(a: Addr): Abs = content.get(a) match {
    case None => throw new Exception(s"Unbound address (should not happen): $a")
    case Some(v) => v._2
  }
  /** Looks up a value in the store (returning bottom if value not present) */
  def lookupBot(a: Addr): Abs = content.getOrElse(a, (0, abs.bottom))._2
  /** Adds a new element to the store */
  def extend(a: Addr, v: Abs): Store[Addr, Abs] = content.get(a) match {
    case None => Store(content + (a -> (0, v)), counting)
    case Some((n, v2)) => Store(content + (a -> (if (counting) { n+1 } else { n }, abs.join(v2, v))), counting)
  }
  /** Updates an element in the store. Might perform a strong update if this store supports strong updates */
  def update(a: Addr, v: Abs): Store[Addr, Abs] =
    if (counting) {
      content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((0, _)) => Store(content + (a -> (0, v)), counting)
        case _ => extend(a, v)
      }
    } else {
      extend(a, v)
    }
  /** Joins two stores */
  def join(that: Store[Addr, Abs]): Store[Addr, Abs] = Store(this.content |+| that.content, counting)
  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[Addr, Abs]): Boolean =
    that.forall((binding: (Addr, Abs)) => abs.subsumes(lookupBot(binding._1), binding._2))
  /** Returns a store containing items that are not equal with the other store */
  def diff(that: Store[Addr, Abs]): Store[Addr, Abs] = {
    Store(content.filter({ case (a, (n, v)) => that.content.get(a) match {
      case Some((n2, v2)) => n != n2 && v != v2
      case None => true
    }}), counting)
  }
}

object Store {
  /* TODO: have abstract counting as a parameter of the analysis. Also, when it is
   * turned on, it prevents AAC and Free from converging. For now, it's only
   * enabled with the AbstractConcrete lattice. */
  def empty[Addr : Address, Abs : AbstractValue] =
    Store(Map[Addr, (Int, Abs)](), implicitly[AbstractValue[Abs]].name == "Concrete")
  def initial[Addr : Address, Abs : AbstractValue](values: List[(Addr, Abs)]): Store[Addr, Abs] =
    Store(values.map({ case (a, v) => (a, (0, v)) }).toMap, implicitly[AbstractValue[Abs]].name == "Concrete")
}
