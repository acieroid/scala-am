import scala.collection.mutable.{Map => MutableMap}
object Profiler {
  val timedata: MutableMap[String, Long] = MutableMap.empty.withDefaultValue(0)
  def profile[A](name: String)(block: => A): A = {
    val start = System.nanoTime
    val res = block
    timedata += (name -> (timedata(name) + (System.nanoTime - start)))
    res
  }
  val countdata: MutableMap[String, Int] = MutableMap.empty.withDefaultValue(0)
  def count(name: String, n: Int = 1): Unit = {
    countdata += (name -> (countdata(name) + n))
  }
  def print: Unit = {
    timedata.keySet.toList.sorted.foreach(k => {
      val v = timedata(k)
      println(s"$k: ${v / Math.pow(10, 9)}")
    })
    countdata.keySet.toList.sorted.foreach(k => {
      val v = countdata(k)
      println(s"$k: $v")
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
