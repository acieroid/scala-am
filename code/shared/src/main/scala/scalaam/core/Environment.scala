package scalaam.core

/** Mapping from variable name to addresses */
case class Environment[A <: Address](content: Map[String,A]) {
  /** Restrict the environment to only certain keys */
  def restrictTo(keys: Set[String]): Environment[A]         = this.copy(content = content.view.filterKeys(keys).toMap)
  /** Looks up a value in the environment */
  def lookup(name: String): Option[A] = content.get(name)
  /** Extend the environment */
  def extend(name: String, a: A): Environment[A]            = this.copy(content = content + (name -> a))
  def extend(values: Iterable[(String, A)]): Environment[A] = this.copy(content = content ++ values)
  /** Mapping over the environment */
  def mapAddrs(f: A => A): Environment[A] = this.copy(content.view.mapValues(f).toMap)

  /** Better printing. */
  override def toString: String = s"ENV{${content.keySet.toList.filter(content(_).printable).sorted.mkString(" ")} <prims>}"
}

object Environment {
  def apply[A <: Address](bds: Iterable[(String,A)]): Environment[A] = Environment(bds.toMap)
}