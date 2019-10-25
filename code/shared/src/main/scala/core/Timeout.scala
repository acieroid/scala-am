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
  def inFuture[T](block: => T, timeout: Duration): T = {
    var thread: Thread = null
    try {
      val fut = Future { thread = Thread.currentThread(); block}
      Await.result(fut, timeout)
    } catch {
      case e: java.util.concurrent.ExecutionException => throw e.getCause // Unbox boxed errors.
      case _: TimeoutException =>
        thread.interrupt()
        throw new TimeoutException()
    }
  }
}