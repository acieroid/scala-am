package scalaam.core

sealed trait MayFail[A, E] {
  def addError(err: E): MayFail[A, E] = addErrors(Set(err))
  def addErrors(errs: Set[E]): MayFail[A, E] = this match {
    case MayFailSuccess(a)     => MayFailBoth(a, errs)
    case MayFailBoth(a, errs2) => MayFailBoth(a, errs2 ++ errs)
    case MayFailError(errs2)   => MayFailError(errs2 ++ errs)
  }
  /* We want to deconstruct in for loops, so we need filter (see https://issues.scala-lang.org/browse/SI-1336) */
  def withFilter(f: A => Boolean) = this
  def join(that: MayFail[A, E], joinA: (A, => A) => A): MayFail[A, E] = (this, that) match {
    case (MayFailSuccess(a1), MayFailSuccess(a2))     => MayFailSuccess(joinA(a1, a2))
    case (MayFailSuccess(a1), MayFailBoth(a2, errs2)) => MayFailBoth(joinA(a1, a2), errs2)
    case (MayFailSuccess(a1), MayFailError(errs2))    => MayFailBoth(a1, errs2)
    case (MayFailBoth(a1, errs1), MayFailSuccess(a2)) => MayFailBoth(joinA(a1, a2), errs1)
    case (MayFailBoth(a1, errs1), MayFailBoth(a2, errs2)) =>
      MayFailBoth(joinA(a1, a2), errs1 ++ errs2)
    case (MayFailBoth(a1, errs1), MayFailError(errs2)) => MayFailBoth(a1, errs1 ++ errs2)
    case (MayFailError(errs1), MayFailSuccess(a2))     => MayFailBoth(a2, errs1)
    case (MayFailError(errs1), MayFailBoth(a2, errs2)) => MayFailBoth(a2, errs1 ++ errs2)
    case (MayFailError(errs1), MayFailError(errs2))    => MayFailError(errs1 ++ errs2)
  }

  def flatMap[B](f: A => MayFail[B, E]): MayFail[B, E] = this match {
    case MayFailSuccess(a)  => f(a)
    case MayFailError(errs) => MayFailError(errs)
    case MayFailBoth(a, errs) =>
      f(a) match {
        case MayFailSuccess(a)     => MayFailBoth(a, errs)
        case MayFailError(errs2)   => MayFailError(errs ++ errs2)
        case MayFailBoth(a, errs2) => MayFailBoth(a, errs ++ errs2)
      }
  }
  def >>=[B](f: A => MayFail[B, E]): MayFail[B, E] = flatMap[B](f)

  def map[B](f: A => B): MayFail[B, E] = this match {
    case MayFailSuccess(a)    => MayFailSuccess(f(a))
    case MayFailError(errs)   => MayFailError(errs)
    case MayFailBoth(a, errs) => MayFailBoth(f(a), errs)
  }

  def mapSet[B](fa: A => B)(fe: E => B): Set[B] = this match {
    case MayFailSuccess(a)    => Set(fa(a))
    case MayFailError(errs)   => errs.map(fe)
    case MayFailBoth(a, errs) => Set(fa(a)) ++ errs.map(fe)
  }
}

case class MayFailSuccess[A, E](a: A)            extends MayFail[A, E]
case class MayFailError[A, E](errs: Set[E])      extends MayFail[A, E]
case class MayFailBoth[A, E](a: A, errs: Set[E]) extends MayFail[A, E]

object MayFail {
  def failure[A, E](err: E): MayFail[A, E] = MayFailError(Set(err))
  def success[A, E](a: A): MayFail[A, E]   = MayFailSuccess(a)
  def fromOption[A, E](opt: Option[A])(alt: => E): MayFail[A, E] = opt match {
    case Some(a) => success(a)
    case None    => failure(alt)
  }
}
