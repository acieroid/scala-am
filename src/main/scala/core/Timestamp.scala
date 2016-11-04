trait Timestamp[T] {
  def name: String
  def initial(seed: String): T
  def tick(t: T): T
  def tick[Exp](t: T, e: Exp): T
}

object Timestamp {
  def apply[T : Timestamp]: Timestamp[T] = implicitly[Timestamp[T]]
}

trait TimestampWrapper {
  type T
  val isTimestamp: Timestamp[T]
}

case class KCFA(k: Int) extends TimestampWrapper {
  trait T
  case class Time[Exp](seed: String, history: List[Exp]) extends T

  implicit val isTimestamp = new Timestamp[T] {
    def name = "$k-CFA"
    def initial(seed: String) = Time(seed, List())
    def tick(t: T) = t
    def tick[Exp](t: T, e: Exp) = t match {
      case (t : Time[Exp] @unchecked) => Time[Exp](t.seed, (e :: t.history).take(k))
    }
  }
}

/* Similar to KCFA(0), but doesn't include the empty list */
object ZeroCFA extends TimestampWrapper {
  trait T
  case class Time(seed: String) extends T
  implicit val isTimestamp = new Timestamp[T] {
    def name = "0-CFA"
    def initial(seed: String) = Time(seed)
    def tick(t: T) = t
    def tick[Exp](t: T, e: Exp) = t
  }
}

object ConcreteTimestamp extends TimestampWrapper {
  trait T
  case class Time(seed: String, n: Int) extends T {
    override def toString = if (seed.isEmpty) { n.toString } else { s"$seed-$n" }
  }
  implicit val isTimestamp = new Timestamp[T] {
    def name = "Concrete"
    def initial(seed: String) = Time(seed, 0)
    def tick(t: T) = t match {
      case Time(seed, n) => Time(seed, n+1)
    }
    def tick[Exp](t: T, e: Exp) = tick(t)
  }
}
