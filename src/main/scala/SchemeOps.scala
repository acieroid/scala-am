object SchemeOps {
  def modulo(n1: Int, n2: Int): Int =
    if (n1 * n2 < 0) {
      /* different sign, behaviour not the same between Scheme and Scala, adjust it */
      (Math.abs(n2) - Math.abs(n1) % Math.abs(n2)) % Math.abs(n2) * (if (n2 < 0) -1 else 1)
    } else {
      /* same sign, same behaviour */
      n1 % n2
    }
  def random(n: Int): Int = Math.abs(scala.util.Random.nextInt % n)
  def random(n: Float): Float = Math.abs(scala.util.Random.nextFloat % n)
}
