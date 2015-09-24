import scalaz.Scalaz._

case class Store[Addr, Abs](content: Map[Addr, (Int, Abs)], counting: Boolean)(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr]) {
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  def keys: collection.Iterable[Addr] = content.keys
  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({
    case (a, (_, v)) => p(a, v)
  })
  /** Looks up a value in the store */
  def lookup(a: Addr): Abs = content.getOrElse(a, (0, absi.bottom))._2
  /** Adds a new element to the store */
  def extend(a: Addr, v: Abs): Store[Addr, Abs] = content.get(a) match {
    case None => Store(content + (a -> (1, v)), counting)
    case Some((n, v2)) => Store(content + (a -> (if (counting) { n+1 } else { n }, abs.join(v2, v))), counting)
  }
  /** Updates an element in the store. Might perform a strong update if this store supports strong updates */
  def update(a: Addr, v: Abs): Store[Addr, Abs] =
    if (counting) {
      content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((1, _)) => Store(content + (a -> (1, v)), counting)
        case _ => extend(a, v)
      }
    } else {
      extend(a, v)
    }
  /** Joins two stores */
  def join(that: Store[Addr, Abs]): Store[Addr, Abs] = Store(this.content |+| that.content, counting)
  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[Addr, Abs]): Boolean =
    that.forall((binding: (Addr, Abs)) => abs.subsumes(lookup(binding._1), binding._2))
}

object Store {
  /* TODO: have abstract counting as a parameter of the analysis */
  def empty[Addr : Address, Abs]()(implicit abs : AbstractValue[Abs], i : AbstractInjection[Abs]) = Store(Map[Addr, (Int, Abs)](), true)
  def initial[Addr, Abs](values: List[(Addr, Abs)])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr]): Store[Addr, Abs] = Store(values.map({ case (a, v) => (a, (1, v)) }).toMap, true)
}
