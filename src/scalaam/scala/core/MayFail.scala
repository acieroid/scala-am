package scalaam.core

import scalaz.{Monoid, Monad, Functor}

sealed trait MayFail[A, E] {
  def addError(err: E): MayFail[A, E] = this match {
    case MayFailSuccess(a) => MayFailBoth(a, List(err))
    case MayFailBoth(a, errs) => MayFailBoth(a, errs :+ err)
    case MayFailError(errs) => MayFailError(errs :+ err)
  }
  /* We want to deconstruct in for loops, so we need filter (see https://issues.scala-lang.org/browse/SI-1336) */
  def withFilter(f: A => Boolean) = this
  def extract: Option[A] = this match {
    case MayFailSuccess(a) => Some(a)
    case MayFailBoth(a, _) => Some(a)
    case MayFailError(_) => None
  }

  def flatMap[B](f: A => MayFail[B, E]): MayFail[B, E] = this match {
    case MayFailSuccess(a) => f(a)
    case MayFailError(errs) => MayFailError(errs)
    case MayFailBoth(a, errs) => f(a) match {
      case MayFailSuccess(a) => MayFailBoth(a, errs)
      case MayFailError(errs2) => MayFailError(errs ++ errs2)
      case MayFailBoth(a, errs2) => MayFailBoth(a, errs ++ errs2)
    }
  }
  def map[B](f: A => B): MayFail[B, E] = this match {
    case MayFailSuccess(a) => MayFailSuccess(f(a))
    case MayFailError(errs) => MayFailError(errs)
    case MayFailBoth(a, errs) => MayFailBoth(f(a), errs)
  }
}

case class MayFailSuccess[A, E](a: A) extends MayFail[A, E]
case class MayFailError[A, E](errs: List[E]) extends MayFail[A, E]
case class MayFailBoth[A, E](a: A, errs: List[E]) extends MayFail[A, E]

object MayFail {
  implicit def monoid[A, E](implicit monoid: Monoid[A]): Monoid[MayFail[A, E]] =
    new Monoid[MayFail[A, E]] {
      def append(x: MayFail[A, E], y: => MayFail[A, E]): MayFail[A, E] = (x, y) match {
        case (MayFailSuccess(x), MayFailSuccess(y)) => MayFailSuccess(monoid.append(x, y))
        case (MayFailSuccess(x), MayFailError(errs)) => MayFailBoth(x, errs)
        case (MayFailSuccess(x), MayFailBoth(y, errs)) => MayFailBoth(monoid.append(x, y), errs)
        case (MayFailError(errs1), MayFailError(errs2)) => MayFailError(errs1 ++ errs2)
        case (MayFailError(errs1), MayFailBoth(x, errs2)) => MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs1), MayFailBoth(y, errs2)) => MayFailBoth(monoid.append(x, y), errs1 ++ errs2)
        case (x, y) => append(y, x)
      }
      def zero: MayFail[A, E] = MayFailSuccess(monoid.zero)
    }

  implicit def isMonad[E]: Monad[MayFail[?, E]] = new Monad[MayFail[?, E]] {
    def bind[A, B](fa: MayFail[A, E])(f: (A) => MayFail[B, E]): MayFail[B, E] = fa match {
      case MayFailSuccess(a) => f(a)
      case MayFailError(errs) => MayFailError(errs)
      case MayFailBoth(a, errs) => f(a) match {
        case MayFailSuccess(a) => MayFailBoth(a, errs)
        case MayFailError(errs2) => MayFailError(errs ++ errs2)
        case MayFailBoth(a, errs2) => MayFailBoth(a, errs ++ errs2)
      }
    }
    def point[A](a: => A): MayFail[A, E] = MayFailSuccess[A, E](a)
  }

  implicit def isFunctor[E]: Functor[MayFail[?, E]] = new Functor[MayFail[?, E]] {
    def map[A, B](fa: MayFail[A, E])(f: (A) => B): MayFail[B, E] = fa match {
      case MayFailSuccess(a) => MayFailSuccess(f(a))
      case MayFailError(errs) => MayFailError(errs)
      case MayFailBoth(a, errs) => MayFailBoth(f(a), errs)
    }
  }

  import scala.language.implicitConversions
  implicit def failure[A, E](err: E): MayFail[A, E] = MayFailError(List(err))
  implicit def success[A, E](a: A): MayFail[A, E] = MayFailSuccess(a)
}
