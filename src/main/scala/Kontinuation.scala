import scalaz.Scalaz._

trait Frame {
  def subsumes(that: Frame): Boolean
}

trait Kontinuation[K] {
  def subsumes(x: K, y: K): Boolean
}

trait KontAddress[A] {
}

case class KontStore[KontAddr : KontAddress, Kont : Kontinuation](content: Map[KontAddr, Set[Kont]]) {
  def kont = implicitly[Kontinuation[Kont]]
  def this() = this(Map())
  def lookup(a: KontAddr): Set[Kont] = content.getOrElse(a, Set())
  def extend(a: KontAddr, κ: Kont): KontStore[KontAddr, Kont] =
    KontStore[KontAddr, Kont](content + (a -> (lookup(a) + κ)))
  def join(that: KontStore[KontAddr, Kont]): KontStore[KontAddr, Kont] = KontStore[KontAddr, Kont](content |+| that.content)
  def forall(p: ((KontAddr, Set[Kont])) => Boolean) = content.forall(p)
  def subsumes(that: KontStore[KontAddr, Kont]): Boolean =
    that.forall({ case (a, ks) =>
      ks.forall((k1) => lookup(a).exists(k2 => kont.subsumes(k2, k1)))
    })
}

