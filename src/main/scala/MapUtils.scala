import scalaz.Semigroup
import scalaz.Scalaz._

object MapUtils {
  implicit class JoinMap[A, B : Semigroup](m: Map[A, B]) {
    val semigroup = implicitly[Semigroup[B]]
    def join(key: A, value: B): Map[A, B] = m.get(key) match {
      case Some(old) => m + (key -> semigroup.append(old, value))
      case None => m + (key -> value)
    }
  }
}
