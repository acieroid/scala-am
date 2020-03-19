package util

object Metrics {

  type Min = Long
  type Max = Long
  type Mea = Long
  type Med = Long
  type StD = Long

  def mean(l: List[Long]): Long = l.sum / l.length

  def median(l: List[Long]): Long = {
    val s = l.sorted
    val split: Int = s.length / 2
    if (s.length % 2 == 0) (s(split - 1) + s(split))/2 else s(split)
  }

  def stddev(l: List[Long]): Long = {
    val  mea = mean(l)
    val sums = l.map(v => (v - mea) * (v - mea))
    sums.sum/(l.length-1)
  }

  def metrics(l: List[Long]): (Min, Max, Mea, Med, StD) = (l.min, l.max, mean(l), median(l), stddev(l))

}
