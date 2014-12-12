import scala.math.BigInt

object PrimeGenerator {
  def naturalsFrom(n: BigInt): Stream[BigInt] = n #:: naturalsFrom(n+1)
  val naturals: Stream[BigInt] = naturalsFrom(BigInt(0))

  def sieve(s: Stream[BigInt]): Stream[BigInt] = s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  val primes: Stream[BigInt] = sieve(naturalsFrom(BigInt(2)))

  val probablePrimes: Stream[BigInt] = naturalsFrom(2).filter(_.isProbablePrime(20))
}
