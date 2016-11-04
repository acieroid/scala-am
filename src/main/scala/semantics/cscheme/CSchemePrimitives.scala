import scalaz._
import scalaz.Scalaz._

class CSchemePrimitives[Addr : Address, Abs : IsCSchemeLattice] extends SchemePrimitives[Addr, Abs] {
  val cabs = implicitly[IsCSchemeLattice[Abs]]

  object NewLock extends Primitive[Addr, Abs] {
    val name = "new-lock"
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = args match {
      case Nil =>
        val a = Address[Addr].cell(fexp, t)
        MayFailSuccess((cabs.lock(a), store.extend(a, cabs.unlockedValue), Set()))
      case l => MayFailError(List(ArityError(name, 0, l.size)))
    }
  }

  override def all = NewLock :: super.all
}
