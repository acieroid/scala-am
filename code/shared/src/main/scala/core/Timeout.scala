package scalaam.core

import java.util.concurrent._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Timeout {
  import scala.concurrent.duration.Duration

  case class T(startingTime: Long, timeout: Option[Long]) {
    def reached: Boolean = timeout.map(System.nanoTime - startingTime > _).getOrElse(false)
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
}

/** Inspired by https://viktorklang.com/blog/Futures-in-Scala-protips-6.html */
object Interruptable {

  class NotInterruptedException extends RuntimeException

  /**
   * Executes a given block. If the execution of the block exceeds the given timeout, an exception is thrown.
   * Uses multithreaded computations, meaning that the actual computation time may be (slightly) longer than the given duration.
   * @param block   The block to be executed.
   * @param timeout The maximal duration for the execution of the block.
   * @tparam T      The return type of the given block.
   * @return        The return value of the block if its computation finishes in time.
   * @throws        {@link TimeoutException} if the execution of the block exceeds the given duration and this is noticed.<br>
   *                {@link NotInterruptedException} if the execution of the block exceeds the given duration but its computation is still continuing in another thread.
   *                Any exception thrown by the execution of the block.
   */
  def inFuture[T](block: => T, timeout: Duration): T = {
    var thread: Thread = null
    try {
      val fut = Future { thread = Thread.currentThread(); block}
      Await.result(fut, timeout)
    } catch {
      case e: java.util.concurrent.ExecutionException => throw e.getCause // Unbox boxed errors.
      case _: TimeoutException if thread != null =>
        thread.interrupt()
        throw new TimeoutException()
      case _: TimeoutException => // Should not happen! Might only happen when `fut` has performed no work.
        throw new NotInterruptedException // Since we do not have a reference to the thread, we cannot interrupt it.
    }
  }
}