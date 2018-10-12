package scalaam.core

trait Timestamp[T, C] {
  def initial(seed: String): T
  def tick(t: T): T
  def tick(t: T, c: C): T = tick(t)
}

object Timestamp {
  def apply[T, C]()(implicit t: Timestamp[T, C]): Timestamp[T, C] = t
}

/** k-CFA context sensitivity, with a given value of k.
  This sensitivity preserves information about the last k contexts */
case class KCFA[C](k: Int) {
  case class T(seed: String, history: List[C])
  object T {
    implicit val typeclass = new Timestamp[T, C] {
      def initial(seed: String)     = T(seed, List.empty)
      def tick(t: T)                = t
      override def tick(t: T, c: C) = T(t.seed, (c :: t.history).take(k))
    }
  }
}

/** Similar to KCFA(0), but doesn't include the empty list */
case class ZeroCFA[C]() {
  case class T(seed: String)
  object T {
    implicit val typeclass = new Timestamp[T, C] {
      def initial(seed: String) = T(seed)
      def tick(t: T)            = t
    }
  }
}

/** Concrete timestamps to model concrete execution */
case class ConcreteTimestamp[C]() {
  case class T(seed: String, n: Int)
  object T {
    implicit val typeclass = new Timestamp[T, C] {
      def initial(seed: String) = T(seed, 0)
      def tick(t: T)            = T(t.seed, t.n + 1)
    }
  }
}
