trait Timestamp[T] {
  def name: String
  def initial: T
  def tick(t: T): T
  def tick[Exp](t: T, e: Exp): T
}

case class KCFA(k: Int) {
  trait KCFATimestamp
  object KCFATimestamp {
    case class Time[Exp](history: List[Exp]) extends KCFATimestamp
    implicit object KCFATimestampTimestamp extends Timestamp[KCFATimestamp] {
      def name = "$k-CFA"
      def initial = Time(List())
      def tick(t: KCFATimestamp) = t
      def tick[Exp](t: KCFATimestamp, e: Exp) = t match {
        case t : Time[Exp] => Time[Exp]((e :: t.history).take(k))
      }
    }
  }
}

trait ConcreteTimestamp
object ConcreteTimestamp {
  case class Time(n: Int) extends ConcreteTimestamp
  implicit object ConcreteTimestampTimestamp extends Timestamp[ConcreteTimestamp] {
    def name = "Concrete"
    def initial = Time(0)
    def tick(t: ConcreteTimestamp) = t match {
      case Time(n) => Time(n+1)
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
