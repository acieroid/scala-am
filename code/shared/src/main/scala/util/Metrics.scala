package scalaam.util

object Metrics {

  type Min = Long
  type Max = Long
  type Mea = Long
  type Med = Long
  type StD = Long

  case class M(min: Min, max: Max, mea: Mea, med: Med, std: StD) {
    override def toString: String = {
      s"* Values in [$min,$max]\n" ++
      s"* Mean: $mea , Median: $med\n" ++
      s"* Standard deviation: $std"
    }
  }

  def mean(l: List[Long]): Long = l.sum / l.length

  def median(l: List[Long]): Long = {
    val s = l.sorted
    val split: Int = s.length / 2
    if (s.length % 2 == 0) (s(split - 1) + s(split))/2 else s(split)
  }

  def stddev(l: List[Long]): Long = {
    if (l.length == 1) {
      0
    } else {
      val  mea = mean(l)
      val sums = l.map(v => (v - mea) * (v - mea))
      scala.math.sqrt(sums.sum/(l.length-1)).toLong
    }
  }

  def all(l: List[Long]): M = M(l.min, l.max, mean(l), median(l), stddev(l))

  // Expect a list of tuples (weight, value).
  def weightedAverage(l: List[(Long, Long)]): Double = {
    val num = l.map(t => t._1 * t._2).sum
    val den = l.foldLeft(0.0)((acc, t) => acc + t._1)
    num / den
  }

}
