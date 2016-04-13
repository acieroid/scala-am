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

abstract class KontStore[KontAddr : KontAddress] {
  def lookup(a: KontAddr): Set[Kont[KontAddr]]
  def extend(a: KontAddr, kont: Kont[KontAddr]): KontStore[KontAddr]
  def join(that: KontStore[KontAddr]): KontStore[KontAddr]
  def forall(p: ((KontAddr, Set[Kont[KontAddr]])) => Boolean): Boolean
  def subsumes(that: KontStore[KontAddr]): Boolean
}

case class BasicKontStore[KontAddr : KontAddress](content: Map[KontAddr, Set[Kont[KontAddr]]]) extends KontStore[KontAddr] {
  def this() = this(Map())
  def lookup(a: KontAddr) = content.getOrElse(a, Set())
  def extend(a: KontAddr, kont: Kont[KontAddr]) =
    this.copy(content = content + (a -> (lookup(a) + kont)))
  def join(that: KontStore[KontAddr]) = if (that.isInstanceOf[BasicKontStore[KontAddr]]) {
    this.copy(content = content |+| that.asInstanceOf[BasicKontStore[KontAddr]].content)
  } else {
    throw new Exception(s"Incompatible continuation stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")
  }
  def forall(p: ((KontAddr, Set[Kont[KontAddr]])) => Boolean) = content.forall(p)
  def subsumes(that: KontStore[KontAddr]) =
    that.forall({ case (a, ks) =>
      ks.forall((k1) => lookup(a).exists(k2 => k2.subsumes(k1)))
    })
}

object KontStore {
  def empty[KontAddr : KontAddress]: KontStore[KontAddr] =
    new BasicKontStore[KontAddr]()
}
