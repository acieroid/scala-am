case class Environment[Addr : Address](content: Map[String, Addr]) {
  def keys = content.keys
  def forall(p: ((String, Addr)) => Boolean) = content.forall(p)
  def lookup(name: String) = content.get(name)
  def extend(name: String, addr: Addr) = Environment(content + (name -> addr))
  def extend(values: List[(String, Addr)]) = Environment(content ++ values)
  def subsumes(that: Environment[Addr]) =
    that.forall((binding: (String, Addr)) => lookup(binding._1) match {
      case Some(addr) => implicitly[Address[Addr]].subsumes(addr, binding._2)
      case None => false
    })
}

object Environment {
  def empty[Addr : Address]() = Environment(Map[String, Addr]())
}
