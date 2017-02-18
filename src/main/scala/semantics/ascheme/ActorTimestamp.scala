trait ActorTimestamp[T] extends Timestamp[T] {
  def actorBecome[Actd](t: T, actd: Actd): T
  def actorCreated[PID](t: T, pid: PID): T
  def messageReception[PID, Abs](t: T, sender: PID, tag: String, args: List[Abs]): T
  def messageSent[PID, Abs](t: T, target: PID, tag: String, args: List[Abs]): T
}

object ActorTimestamp {
  def apply[T : ActorTimestamp]: ActorTimestamp[T] = implicitly
  import scala.language.implicitConversions
  implicit def fromTime(tw: TimestampWrapper): ActorTimestampWrapper = new ActorTimestampWrapper {
    type T = tw.T
    implicit val isTimestamp = tw.isTimestamp
    implicit val isActorTimestamp = new ActorTimestamp[T] {
      def name = Timestamp[T].name
      def initial(seed: String) = Timestamp[T].initial(seed)
      def tick(t: T) = Timestamp[T].tick(t)
      def tick[Exp](t: T, e: Exp) = Timestamp[T].tick(t, e)
      def actorBecome[Actd](t: T, actd: Actd) = Timestamp[T].tick(t)
      def actorCreated[PID](t: T, pid: PID) = Timestamp[T].tick(t)
      def messageReception[PID, Abs](t: T, sender: PID, tag: String, args: List[Abs]) = Timestamp[T].tick(t)
      def messageSent[PID, Abs](t: T, target: PID, tag: String, args: List[Abs]) = Timestamp[T].tick(t)
    }
  }
}

trait ActorTimestampWrapper extends TimestampWrapper {
  val isActorTimestamp: ActorTimestamp[T]
}

case class KMessageTagSensitivity(k: Int) extends ActorTimestampWrapper {
  trait T
  case class Time(seed: String, history: List[String]) extends T

  implicit val isActorTimestamp = new ActorTimestamp[T] {
    def name = s"$k-MSG"
    def initial(seed: String) = Time(seed, List())
    def tick(t: T) = t
    def tick[Exp](t: T, e: Exp) = t
    def actorBecome[Actd](t: T, actd: Actd) = t
    def actorCreated[PID](t: T, pid: PID) = t
    def messageReception[PID, Abs](t: T, sender: PID, tag: String, args: List[Abs]) = t match {
      case (t: Time) =>
        // println(s"new history: ${(tag :: t.history).take(k)}")
        Time(t.seed, (tag :: t.history).take(k))
    }
    def messageSent[PID, Abs](t: T, target: PID, tag: String, args: List[Abs]) = t
  }
  implicit val isTimestamp: Timestamp[T] = isActorTimestamp
}
