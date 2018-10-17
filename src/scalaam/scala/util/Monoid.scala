package scalaam.util

trait Monoid[M] {
  def append(x: M, y: => M): M
  def zero: M
}
object Monoid {
  def apply[M : Monoid]: Monoid[M] = implicitly
}

object MonoidInstances {
  import scalaam.core._
  def mayFail[M](implicit monoid: Monoid[M]): Monoid[MayFail[M, Error]] =
    new Monoid[MayFail[M, Error]] {
      def append(x: MayFail[M, Error], y: => MayFail[M, Error]): MayFail[M, Error] = (x, y) match {
        case (MayFailSuccess(x), MayFailSuccess(y)) => MayFailSuccess(monoid.append(x, y))
        case (MayFailSuccess(x), MayFailError(errs)) => MayFailBoth(x, errs)
        case (MayFailSuccess(x), MayFailBoth(y, errs)) => MayFailBoth(monoid.append(x, y), errs)
        case (MayFailError(errs), MayFailSuccess(x)) => MayFailBoth(x, errs)
        case (MayFailError(errs1), MayFailError(errs2)) => MayFailError(errs1 ++ errs2)
        case (MayFailError(errs1), MayFailBoth(x, errs2)) => MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs), MayFailSuccess(y)) => MayFailBoth(monoid.append(x, y), errs)
        case (MayFailBoth(x, errs1), MayFailError(errs2)) => MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs1), MayFailBoth(y, errs2)) => MayFailBoth(monoid.append(x, y), errs1 ++ errs2)
      }
      def zero: MayFail[M, Error] = MayFailSuccess(monoid.zero)
    }
  def setMonoid[M]: Monoid[Set[M]] = new Monoid[Set[M]] {
    def append(x: Set[M], y: => Set[M]): Set[M] = x ++ y
    def zero: Set[M] = Set[M]()
  }
  val boolOrMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x || y
    def zero: Boolean = false
  }
  val boolAndMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x && y
    def zero: Boolean = true
  }
}
