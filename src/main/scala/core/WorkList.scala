import scala.language.higherKinds
trait WorkList[L[_]] {
  def pick[A](w: L[A]): Option[(A, L[A])]
  def append[A](w: L[A], s: Set[A]): L[A]
}
object WorkList {
  def apply[WL[_] : WorkList]: WorkList[WL] = implicitly
  import scala.collection.immutable.Seq
  implicit object SeqWorkList extends WorkList[Seq] {
    def pick[A](w: Seq[A]) = w.headOption.map(x => (x, w.tail))
    def append[A](w: Seq[A], s: Set[A]) = w ++ s
  }
}
