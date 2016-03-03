object SchemeOps {
  def modulo(n1: Int, n2: Int): Int =
    if (scala.math.signum(n1) * scala.math.signum(n2) < 0) {
      /* different sign, behaviour not the same between Scheme and Scala, adjust it */
      (scala.math.abs(n2) - scala.math.abs(n1) % scala.math.abs(n2)) % scala.math.abs(n2) * (if (n2 < 0) -1 else 1)
    } else {
      /* same sign, same behaviour */
      n1 % n2
    }
  def random(n: Int): Int = scala.math.abs(scala.util.Random.nextInt % n)
  def random(n: Float): Float = scala.math.abs(scala.util.Random.nextFloat % n)
}
