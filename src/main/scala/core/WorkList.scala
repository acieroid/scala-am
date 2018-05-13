import scala.language.higherKinds
trait WorkList[L[_]] {
  def pick[A](w: L[A]): Option[(A, L[A])]
  def pick[A](w: L[A], priority: A => Boolean): Option[(A, L[A])]
  def append[A](w: L[A], s: Set[A]): L[A]
}
object WorkList {
  def apply[WL[_] : WorkList]: WorkList[WL] = implicitly
//  import scala.collection.immutable.Seq
  implicit object SetWorkList extends WorkList[Set] {
    def pick[A](w: Set[A]) = w.headOption.map(x => (x, w.tail))
    def pick[A](w: Set[A], priority: A => Boolean) = w.find(priority).fold(pick(w))(a => Option((a, w - a)))
    def append[A](w: Set[A], s: Set[A]) = w ++ s
  }
}
