package scalaam.util.benchmarks

object Timeout {
  import scala.concurrent.duration.Duration

  case class T(startingTime: Long, private var timeout: Option[Long]) {
    def reached : Boolean      = timeout.exists(System.nanoTime - startingTime > _)
    def time    : Double       = (System.nanoTime - startingTime) / Math.pow(10, 9)
    def timeLeft: Option[Long] = timeout.map { duration =>
      val deadline = startingTime + duration
      deadline - System.nanoTime
    }
    // Allows to change the timeout after its creation, but not if the timeout has already passed.
    def map(f: Long => Long): T = {
      timeout = timeout.map(f)
      this }
  }

  def start(timeout: Duration): T = T(System.nanoTime, if (timeout.isFinite) Some(timeout.toNanos) else None)
  def none: T = T(System.nanoTime, None)
}

object Timer {

  @inline
  def time[A](block: => A): (Long, A) = {
    val t1:   Long = System.nanoTime()
    val ans:     A = block
    val t2:   Long = System.nanoTime()
    val time: Long = t2 - t1
    (time, ans)
  }

  @inline
  def timeOnly[A](block: => A): Long = time(block)._1
}