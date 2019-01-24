package scalaam.core

/** Error raised when a variable is unbound when looked up in an environment.
  * @see [[scalaam.core.Error]]
  */
case class UnboundVariable(id: Identifier) extends Error

/** Mapping from variable name to addresses */
trait Environment[A <: Address] {

  /** Gets all the keys of the environment */
  def keys: Iterable[String]

  /** Checks if a predicate is true for all elements of the environment */
  def forall(p: ((String, A)) => Boolean): Boolean

  /** Looks up a value in the environment */
  def lookup(name: String): Option[A]
  def lookupMF(name: Identifier): MayFail[A, Error] = lookup(name.name) match {
    case Some(v) => MayFail.success(v)
    case None    => MayFail.failure(UnboundVariable(name))
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
  override def toString =
    "{" + content
      .filter({ case (_, a) => a.printable })
      .map({ case (k, v) => s"$k: $v" })
      .mkString(", ") + "}"
  def keys                                  = content.keys
  def forall(p: ((String, A)) => Boolean)   = content.forall(p)
  def lookup(name: String)                  = content.get(name)
  def extend(name: String, a: A)            = this.copy(content = content + (name -> a))
  def extend(values: Iterable[(String, A)]) = this.copy(content = content ++ values)
}

/* Default environment constructors */
object Environment {

  /** The empty environment */
  def empty[A <: Address]: Environment[A] = BasicEnvironment(Map[String, A]())

  /** Constructs initial environment given its initial element */
  def initial[A <: Address](values: Iterable[(String, A)]): Environment[A] =
    BasicEnvironment(values.toMap)
}
