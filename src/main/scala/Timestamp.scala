trait Timestamp[T] {
  def name: String
  def initial(seed: String): T
  def tick(t: T): T
  def tick[Exp](t: T, e: Exp): T
}

case class KCFA(k: Int) {
  trait KCFATimestamp
  object KCFATimestamp {
    case class Time[Exp](seed: String, history: List[Exp]) extends KCFATimestamp
    implicit object KCFATimestampTimestamp extends Timestamp[KCFATimestamp] {
      def name = "$k-CFA"
      def initial(seed: String) = Time(seed, List())
      def tick(t: KCFATimestamp) = t
      def tick[Exp](t: KCFATimestamp, e: Exp) = t match {
        case t : Time[Exp] => Time[Exp](t.seed, (e :: t.history).take(k))
      }
    }
  }
}

trait ConcreteTimestamp
object ConcreteTimestamp {
  case class Time(seed: String, n: Int) extends ConcreteTimestamp {
    override def toString = if (seed.isEmpty) { n.toString } else { s"$seed-$n" }
  }
  implicit object ConcreteTimestampTimestamp extends Timestamp[ConcreteTimestamp] {
    def name = "Concrete"
    def initial(seed: String) = Time(seed, 0)
    def tick(t: ConcreteTimestamp) = t match {
      case Time(seed, n) => Time(seed, n+1)
    }
    def tick[Exp](t: ConcreteTimestamp, e: Exp) = tick(t)
  }
}

object CFA {
  val ZeroCFA = KCFA(0)
  type ZeroCFA = ZeroCFA.KCFATimestamp
  val OneCFA = KCFA(1)
  type OneCFA = OneCFA.KCFATimestamp
}
