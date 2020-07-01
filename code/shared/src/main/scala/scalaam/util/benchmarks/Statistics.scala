package scalaam.util.benchmarks

// Helpers to compute statistics over sets of data
object Statistics {

  // Bundles multiple statitistical measures in one object
  case class M(min: Double, max: Double, mea: Double, med: Double, std: Double) {
    override def toString: String = {
      s"* Values in [$min,$max]\n" ++
      s"* Mean: $mea , Median: $med\n" ++
      s"* Standard deviation: $std"
    }
  }

  def mean(l: List[Double]): Double = l.sum / l.length

  def median(l: List[Double]): Double = {
    val s = l.sorted
    val split: Int = s.length / 2
    if (s.length % 2 == 0) (s(split - 1) + s(split))/2 else s(split)
  }

  def stddev(l: List[Double]): Double = {
    if (l.length == 1) {
      0
    } else {
      val  mea = mean(l)
      val sums = l.map(v => (v - mea) * (v - mea))
      scala.math.sqrt(sums.sum/(l.length-1)).toDouble
    }
  }

  // Constructs an M object from a list of values
  def all(l: List[Double]): M = M(l.min, l.max, mean(l), median(l), stddev(l))

  // Expect a list of tuples (weight, value).
  def weightedAverage(l: List[(Double, Double)]): Double = {
    val num = weightedSum(l)
    val den = l.foldLeft(0.0)((acc, t) => acc + t._1)
    num / den
  }

  // Expect a list of tuples (weight, value).
  def weightedSum(l: List[(Double, Double)]): Double = {
    l.foldLeft(0.0)((acc, t) => acc + t._1 * t._2)
  }

}
