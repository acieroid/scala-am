package scalaam.core

case class UnboundVariable(id: Identifier) extends Error

trait Environment[A <: Address] {
  /** Gets all the keys of the environment */
  def keys: Iterable[String]
  /** Checks if a predicate is true for all elements of the environment */
  def forall(p: ((String, A)) => Boolean): Boolean
  /** Looks up a value in the environment */
  def lookup(name: String): Option[A]
  def lookupMF(name: Identifier): MayFail[A, Error] = lookup(name.name) match {
    case Some(v) => v
    case None => UnboundVariable(name)
  }
  /** Extend the environment */
  def extend(name: String, a: A): Environment[A]
  /** Extend the environment with multiple values */
  def extend(values: Iterable[(String, A)]): Environment[A]
  /** Checks whether this environment subsumes another */
  def subsumes(that: Environment[A]) =
    that.forall((binding: (String, A)) => lookup(binding._1).contains(binding._2))
}

/** Basic mapping from names to addresses */
case class BasicEnvironment[A <: Address](content: Map[String, A]) extends Environment[A] {
  override def toString = content.filter({ case (_, a) => a.printable }).toString
  def keys = content.keys
  def forall(p: ((String, A)) => Boolean) = content.forall(p)
  def lookup(name: String) = content.get(name)
  def extend(name: String, a: A) = this.copy(content = content + (name -> a))
  def extend(values: Iterable[(String, A)]) = this.copy(content = content ++ values)
}

object Environment {
  def empty[A <: Address]: Environment[A] = BasicEnvironment(Map[String, A]())
  def initial[A <: Address](values: Iterable[(String, A)]): Environment[A] = BasicEnvironment(values.toMap)
}
