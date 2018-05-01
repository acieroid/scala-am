package scalaam.core

/** A timestamp T with contexts of type C */
trait Timestamp[T, C] {
  /** The initial value for the timestamp */
  def initial(seed: String): T
  /** A tick on the timestamp without context */
  def tick(t: T): T
  /** A tick on the timestamp with context c */
  def tick(t: T, c: C): T
}

object Timestamp {
  def apply[T, C]()(implicit t: Timestamp[T, C]): Timestamp[T, C] = t
}

trait TimestampTypeclass[C] {
  type T
  val typeclass: Timestamp[T, C]
}

/** k-CFA context sensitivity, with a given value of k.
  This sensitivity preserves information about the last k contexts */
case class KCFA[C](k: Int) extends TimestampTypeclass[C] {
  case class T(seed: String, history: List[C])

  implicit val typeclass = new Timestamp[T, C] {
    def initial(seed: String) = T(seed, List.empty)
    def tick(t: T) = t
    def tick(t: T, c: C) = T(t.seed, (c :: t.history).take(k))
  }
}

/** Similar to KCFA(0), but doesn't include the empty list */
case class ZeroCFA[C]() extends TimestampTypeclass[C] {
  case class T(seed: String)
  implicit val typeclass = new Timestamp[T, C] {
    def initial(seed: String) = T(seed)
    def tick(t: T) = t
    def tick(t: T, c: C) = t
  }
}

/** Concrete timestamps to model concrete execution */
case class ConcreteTimestamp[C]() extends TimestampTypeclass[C] {
  case class T(seed: String, n: Int)
  implicit val typeclass = new Timestamp[T, C] {
    val name = "Concrete"
    def initial(seed: String) = T(seed, 0)
    def tick(t: T) = T(t.seed, t.n + 1)
    def tick(t: T, c: C) = tick(t)
  }
}
