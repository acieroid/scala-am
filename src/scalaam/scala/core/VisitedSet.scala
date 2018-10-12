package scalaam.core

import scala.language.higherKinds

trait WithKey[A] {
  type K
  def key(a: A): K
}
object WithKey {
  def apply[A: WithKey]: WithKey[A] = implicitly
}

class KeyMap[A: WithKey](val content: Map[WithKey[A]#K, Set[A]])
object KeyMap {
  def apply[A: WithKey](): KeyMap[A] =
    new KeyMap(Map[WithKey[A]#K, Set[A]]().withDefaultValue(Set.empty))
}

trait VisitedSet[L[_]] {
  def empty[A: WithKey]: L[A]
  def contains[A: WithKey](v: L[A], a: A): Boolean
  def exists[A: WithKey](v: L[A], a: A, p: A => Boolean): Boolean
  def add[A: WithKey](v: L[A], a: A): L[A]
  def append[A: WithKey](v: L[A], as: Set[A]): L[A] = as.foldLeft(v)((v, a) => add(v, a))
  def size[A: WithKey](v: L[A]): Int
}
object VisitedSet {
  def apply[VS[_]: VisitedSet]: VisitedSet[VS] = implicitly
  implicit object SetVisitedSet extends VisitedSet[Set] {
    def empty[A: WithKey]                                    = Set.empty
    def contains[A: WithKey](v: Set[A], a: A)                = v.contains(a)
    def exists[A: WithKey](v: Set[A], a: A, p: A => Boolean) = v.exists(p)
    def add[A: WithKey](v: Set[A], a: A)                     = v + a
    def size[A: WithKey](v: Set[A])                          = v.size
  }
  implicit object MapVisitedSet extends VisitedSet[KeyMap] {
    def empty[A: WithKey] = KeyMap[A]()
    def contains[A: WithKey](v: KeyMap[A], a: A) =
      v.content(WithKey[A].key(a)).contains(a)
    def exists[A: WithKey](v: KeyMap[A], a: A, p: A => Boolean) =
      v.content(WithKey[A].key(a)).exists(p)
    def add[A: WithKey](v: KeyMap[A], a: A) =
      new KeyMap(v.content + (WithKey[A].key(a) -> (v.content(WithKey[A].key(a)) + a)))
    def size[A: WithKey](v: KeyMap[A]) =
      v.content.foldLeft(0)((x, kv) => kv match { case (_, v) => x + v.size })
  }
}
