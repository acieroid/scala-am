package scalaam.util

object Timeout {
  import scala.concurrent.duration.Duration

  case class T(startingTime: Long, timeout: Option[Long]) {
    def reached: Boolean = timeout.exists(System.nanoTime - startingTime > _)
    def time: Double     = (System.nanoTime - startingTime) / Math.pow(10, 9)
  }

  def start(timeout: Option[Long]): T = T(System.nanoTime, timeout)
  def start(timeout: Duration): T =
    T(System.nanoTime, if (timeout.isFinite) {
      Some(timeout.toNanos)
    } else None)

  val Infinity = T(System.nanoTime, None)

  def minutes(n: Int): T = start(Some(n.toLong * 60 * 1000 * 1000 * 1000))
  def seconds(n: Int): T = start(Some(n.toLong * 1000 * 1000 * 1000))
  def duration(d: Duration): T = start(Some(d.toNanos))
}
