package scalaam.core

object Timeout {
  import scala.concurrent.duration.Duration

  case class T(startingTime: Long, timeout: Option[Long]) {
    def reached: Boolean = timeout.map(System.nanoTime - startingTime > _).getOrElse(false)
    def time: Double = (System.nanoTime - startingTime) / Math.pow(10, 9)
  }

  def start(timeout: Option[Long]): T = T(System.nanoTime, timeout)
  def start(timeout: Duration): T = T(System.nanoTime, if (timeout.isFinite) { Some(timeout.toNanos) } else None)

  val Infinity = T(System.nanoTime, None)
}
