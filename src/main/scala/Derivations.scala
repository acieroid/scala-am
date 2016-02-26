import scalaz.{Order, Ordering, Show}
import scalaz.Scalaz._
import shapeless._

object OrderDerive {
  implicit val hnilOrder: Order[HNil] = new Order[HNil] {
    def order(x: HNil, y: HNil): Ordering = Ordering.EQ
  }
  implicit def hconsOrder[H, T <: HList](implicit sh: Order[H], st: Order[T]) = new Order[H :: T] {
    def order(x: H :: T, y: H :: T): Ordering = sh.order(x.head, y.head) |+| st.order(x.tail, y.tail)
  }
  implicit val cnilOrder = new Order[CNil] {
    def order(x: CNil, y: CNil): Ordering = Ordering.EQ
  }
  implicit def coproductOrder[L, R <: Coproduct](implicit sl: Order[L], sr: Order[R]) = new Order[L :+: R] {
    def order(x: L :+: R, y: L :+: R): Ordering = (x, y) match {
      case (Inl(x), Inl(y)) => sl.order(x, y)
      case (Inl(_), _) => Ordering.GT
      case (_, Inl(_)) => Ordering.LT
      case (Inr(x), Inr(y)) => sr.order(x, y)
    }
  }

  implicit def instanceOrder[F, G](implicit gen: Generic.Aux[F, G], reprOrder: Lazy[Order[G]]): Order[F] = new Order[F] {
    def order(x: F, y: F): Ordering = reprOrder.value.order(gen.to(x), gen.to(y))
  }
}
