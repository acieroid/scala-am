package scalaam.core

import scala.language.higherKinds

/** A worklist is an abstract set of elements */
trait WorkList[L[_]] {
  /** An item can be picked from a worklist */
  def pick[A](w: L[A]): Option[(A, L[A])]
  /** A number of items can be added to a worklist */
  def append[A](w: L[A], s: Set[A]): L[A]
}
object WorkList {
  def apply[WL[_] : WorkList]: WorkList[WL] = implicitly

  import scala.collection.immutable.Seq
  /** Any Scala Seq is a worklist */
  implicit object SeqWorkList extends WorkList[Seq] {
    def pick[A](w: Seq[A]) = w.headOption.map(x => (x, w.tail))
    def append[A](w: Seq[A], s: Set[A]) = w ++ s
  }
}
