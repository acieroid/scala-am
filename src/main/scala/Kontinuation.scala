import scalaz.Scalaz._

trait Frame {
  def subsumes(that: Frame): Boolean
}
trait KontAddress[A]

case class Kont[KontAddr: KontAddress](frame: Frame, next: KontAddr) {
  def subsumes(that: Kont[KontAddr]) = that match {
    case Kont(frame2, next2) => frame.subsumes(frame2) && next.equals(next2)
    case _ => false
  }
}

case class KontStore[KontAddr : KontAddress](content: Map[KontAddr, Set[Kont[KontAddr]]]) {
  def this() = this(Map())
  def lookup(a: KontAddr): Set[Kont[KontAddr]] = content.getOrElse(a, Set())
  def extend(a: KontAddr, κ: Kont[KontAddr]): KontStore[KontAddr] =
    KontStore[KontAddr](content + (a -> (lookup(a) + κ)))
  def join(that: KontStore[KontAddr]): KontStore[KontAddr] = KontStore[KontAddr](content |+| that.content)
  def forall(p: ((KontAddr, Set[Kont[KontAddr]])) => Boolean) = content.forall(p)
  def subsumes(that: KontStore[KontAddr]): Boolean =
    that.forall({ case (a, ks) =>
      ks.forall((k1) => lookup(a).exists(k2 => k2.subsumes(k1)))
    })
}

object KontStore {
  def empty[KontAddr : KontAddress] =
    new KontStore[KontAddr]()
}
