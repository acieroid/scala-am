case class Store[Addr : Address, Abs](content: Map[Addr, Abs])(implicit abs : AbstractValue[Abs], i : AbstractInjection[Abs]) {
  def keys: collection.Iterable[Addr] = content.keys
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall(p)
  def lookup(addr: Addr): Abs = content.getOrElse(addr, i.bottom)
  def extend(addr: Addr, v: Abs): Store[Addr, Abs] = Store(content + (addr -> (abs.join(lookup(addr), v))))
  def extend(values: List[(Addr, Abs)]): Store[Addr, Abs] = Store(content ++ values)
  def subsumes(that: Store[Addr, Abs]): Boolean =
    that.forall((binding: (Addr, Abs)) => abs.subsumes(lookup(binding._1), binding._2))
}

object Store {
  def empty[Addr : Address, Abs]()(implicit abs : AbstractValue[Abs], i : AbstractInjection[Abs]) = Store(Map[Addr, Abs]())
}
