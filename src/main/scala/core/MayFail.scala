import scalaz.Scalaz._
import scalaz._

trait MayFail[L] {
  def addError(err: SemanticError): MayFail[L] = this match {
    case MayFailSuccess(l) => MayFailBoth(l, List(err))
    case MayFailBoth(l, errs) => MayFailBoth(l, errs :+ err)
    case MayFailError(errs) => MayFailError(errs :+ err)
  }
  /* We want to deconstruct in for loops, so we need filter (see https://issues.scala-lang.org/browse/SI-1336) */
  def withFilter(f: L => Boolean) = this
  def extract: Option[L] = this match {
    case MayFailSuccess(l) => Some(l)
    case MayFailBoth(l, _) => Some(l)
    case MayFailError(_) => None
  }
}
case class MayFailSuccess[L](l: L) extends MayFail[L]
case class MayFailError[L](errs: List[SemanticError]) extends MayFail[L]
case class MayFailBoth[L](l: L, errs: List[SemanticError]) extends MayFail[L]

object MayFail {
  implicit def monoid[A](implicit monoid: Monoid[A]): Monoid[MayFail[A]] =
    new Monoid[MayFail[A]] {
      def append(x: MayFail[A], y: => MayFail[A]): MayFail[A] = (x, y) match {
        case (MayFailSuccess(x), MayFailSuccess(y)) => MayFailSuccess(monoid.append(x, y))
        case (MayFailSuccess(x), MayFailError(errs)) => MayFailBoth(x, errs)
        case (MayFailSuccess(x), MayFailBoth(y, errs)) => MayFailBoth(monoid.append(x, y), errs)
        case (MayFailError(errs1), MayFailError(errs2)) => MayFailError(errs1 ++ errs2)
        case (MayFailError(errs1), MayFailBoth(x, errs2)) => MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs1), MayFailBoth(y, errs2)) => MayFailBoth(monoid.append(x, y), errs1 ++ errs2)
        case (x, y) => append(y, x)
      }
      def zero: MayFail[A] = MayFailSuccess(monoid.zero)
    }

  implicit val isMonad: Monad[MayFail] = new Monad[MayFail] {
    def bind[A, B](fa: MayFail[A])(f: (A) => MayFail[B]): MayFail[B] = fa match {
      case MayFailSuccess(l) => f(l)
      case MayFailError(errs) => MayFailError(errs)
      case MayFailBoth(l, errs) => f(l) match {
        case MayFailSuccess(l) => MayFailBoth(l, errs)
        case MayFailError(errs2) => MayFailError(errs ++ errs2)
        case MayFailBoth(l, errs2) => MayFailBoth(l, errs ++ errs2)
      }
    }
    def point[A](a: => A): MayFail[A] = MayFailSuccess[A](a)
  }

  implicit val isFunctor: Functor[MayFail] = new Functor[MayFail] {
    def map[A, B](fa: MayFail[A])(f: (A) => B): MayFail[B] = fa match {
      case MayFailSuccess(l) => MayFailSuccess(f(l))
      case MayFailError(errs) => MayFailError(errs)
      case MayFailBoth(l, errs) => MayFailBoth(f(l), errs)
    }
  }

  import scala.language.implicitConversions
  implicit def semErr[A](err: SemanticError): MayFail[A] = MayFailError(List(err))
  implicit def success[A](a: A): MayFail[A] = MayFailSuccess(a)
}
