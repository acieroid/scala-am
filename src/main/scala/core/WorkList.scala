import scala.language.higherKinds
trait WorkList[L[_]] {
  def empty[A]: L[A]
  def single[A](a: A): L[A]
  def pick[A](w: L[A]): Option[(A, L[A])]
  def append[A](w: L[A], s: Set[A]): L[A]
}
object WorkList {
  def apply[WL[_] : WorkList]: WorkList[WL] = implicitly
  implicit val setWorkList = new WorkList[Set] {
    def empty[A] = Set.empty
    def single[A](a: A) = Set(a)
    def pick[A](w: Set[A]) = w.headOption.map(x => (x, w.tail))
    def append[A](w: Set[A], s: Set[A]) = w ++ s
  }

  implicit val vecWorkList = new WorkList[Vector] {
    def empty[A] = Vector.empty
    def single[A](a: A) = Vector(a)
    def pick[A](w: Vector[A]) = w.headOption.map(x => (x, w.tail))
    def append[A](w: Vector[A], s: Set[A]) = s.toVector ++ w
  }
}
