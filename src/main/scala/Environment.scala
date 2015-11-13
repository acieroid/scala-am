case class Environment[Addr : Address](content: Map[String, Addr]) {
  val addr = implicitly[Address[Addr]]
  override def toString = content.filter({ case (_, a) => !addr.isPrimitive(a) }).toString
  def keys = content.keys
  def forall(p: ((String, Addr)) => Boolean) = content.forall(p)
  def lookup(name: String) = content.get(name)
  def extend(name: String, a: Addr) = Environment(content + (name -> a))
  def extend(values: List[(String, Addr)]) = Environment(content ++ values)
  def subsumes(that: Environment[Addr]) =
    that.forall((binding: (String, Addr)) => lookup(binding._1) match {
      case Some(a) => addr.subsumes(a, binding._2)
      case None => false
    })
}

object Environment {
  def empty[Addr : Address]() = Environment(Map[String, Addr]())
}
