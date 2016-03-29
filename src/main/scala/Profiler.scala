import scala.collection.mutable.{Map => MutableMap}
object Profiler {
  val data: MutableMap[String, Long] = MutableMap.empty.withDefaultValue(0)
  def profile[A](name: String)(block: => A): A = {
    val start = System.nanoTime
    val res = block
    data += (name -> (data(name) + (System.nanoTime - start)))
    res
  }
  def print: Unit = {
    data.foreach({ case (k, v) =>
      println(s"$k: ${v / Math.pow(10, 9)}")
    })
  }
  def log[A](message: String)(block: => A): A = {
    println(s"Starting: $message")
    val res = block
    println(s"Done: $message")
    res
  }
  def logRes[A](message: String)(block: => A)(tostr: A => String): A = {
    println(s"Starting: $message")
    val res = block
    println(s"Done: $message")
    println(s"Result of $message is ${tostr(res)}")
    res
  }
}
