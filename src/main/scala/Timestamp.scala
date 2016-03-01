trait Timestamp[T] {
  def name: String
  def initial(seed: String): T
  def tick(t: T): T
  def tick[Exp](t: T, e: Exp): T
}

trait TimestampWrapper {
  type T
  val isTimestamp: Timestamp[T]
}

case class KCFA(k: Int) extends TimestampWrapper {
  trait T
  case class Time[Exp](seed: String, history: List[Exp]) extends T

  val isTimestamp = new Timestamp[T] {
    def name = "$k-CFA"
    def initial(seed: String) = Time(seed, List())
    def tick(t: T) = t
    def tick[Exp](t: T, e: Exp) = t match {
      case t : Time[Exp] => Time[Exp](t.seed, (e :: t.history).take(k))
    }
  }
}

object ConcreteTimestamp extends TimestampWrapper {
  trait T
  case class Time(seed: String, n: Int) extends T {
    override def toString = if (seed.isEmpty) { n.toString } else { s"$seed-$n" }
  }
  val isTimestamp = new Timestamp[T] {
    def name = "Concrete"
    def initial(seed: String) = Time(seed, 0)
    def tick(t: T) = t match {
      case Time(seed, n) => Time(seed, n+1)
    }
    def tick[Exp](t: T, e: Exp) = tick(t)
  }
}
