import scalaz.Scalaz._

case class Store[Addr, Abs](content: Map[Addr, Abs])(implicit abs : AbstractValue[Abs], i : AbstractInjection[Abs], addr: Address[Addr]) {
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  def keys: collection.Iterable[Addr] = content.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall(p)
  def lookup(a: Addr): Abs = content.getOrElse(a, i.bottom)
  def extend(a: Addr, v: Abs): Store[Addr, Abs] = Store(content + (a -> (abs.join(lookup(a), v))))
  def extend(values: List[(Addr, Abs)]): Store[Addr, Abs] = Store(content ++ values)
  def join(that: Store[Addr, Abs]): Store[Addr, Abs] = {
    Store(this.content |+| that.content)
  }
  def subsumes(that: Store[Addr, Abs]): Boolean =
    that.forall((binding: (Addr, Abs)) => abs.subsumes(lookup(binding._1), binding._2))
}

object Store {
  def empty[Addr : Address, Abs]()(implicit abs : AbstractValue[Abs], i : AbstractInjection[Abs]) = Store(Map[Addr, Abs]())
}
