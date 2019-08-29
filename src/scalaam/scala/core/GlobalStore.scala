package scalaam.core

case class GlobalStore[A <: Address, V: Lattice](init: Map[A, V])
    extends Store[A, V]
    with SmartHash {
  val _content: scala.collection.mutable.Map[A, V] = scala.collection.mutable.Map[A, V]() ++ init
  var _mutated: Boolean                            = false
  override def toString                            = _content.view.filterKeys(_.printable).mkString("\n")
  def content                                      = _content.toMap
  def keys                                         = content.keys
  def restrictTo(keys: Set[A]) = {
    keys.foreach(k => _content -= k)
    this
  }
  def forall(p: ((A, V)) => Boolean) = content.forall({ case (a, v) => p((a, v)) })
  def lookup(a: A)                   = content.get(a)
  def extend(a: A, v: V) = {
    content.get(a) match {
      case None =>
        _mutated = true
        _content += ((a, v))
      case Some(v2) =>
        val joined = Lattice[V].join(v, v2)
        if (joined == v2) {} else {
          _mutated = true
          _content += ((a, joined))
        }
    }
    this
  }
  def join(that: Store[A, V])     = ???
  def subsumes(that: Store[A, V]) = ???

  def mutated: Boolean     = _mutated
  def clearMutated(): Unit = { _mutated = false }
}

object GlobalStore {
  def initial[A <: Address, V: Lattice](values: Iterable[(A, V)]): GlobalStore[A, V] =
    new GlobalStore[A, V](values.toMap)
  def empty[A <: Address, V: Lattice]: GlobalStore[A, V] = initial[A, V](List())
}
