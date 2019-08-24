package scalaam.core

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

trait WithKey2[A] {
  type K1
  type K2
  def key1(a: A): K1
  def key2(a: A): K2
}
object WithKey2 {
  def apply[A: WithKey2]: WithKey2[A] = implicitly
}

class KeyMap2[A : WithKey2](val content: Map[WithKey2[A]#K1, Map[WithKey2[A]#K2, Set[A]]])
object KeyMap2 {
  def apply[A: WithKey2](): KeyMap2[A] =
    new KeyMap2(Map[WithKey2[A]#K1, Map[WithKey2[A]#K2, Set[A]]]().withDefaultValue(Map().withDefaultValue(Set.empty)))
}

trait VisitedSet[A, L[_]] {
  def empty: L[A]
  def contains(v: L[A], a: A): Boolean
  def exists(v: L[A], a: A, p: A => Boolean): Boolean
  def add(v: L[A], a: A): L[A]
  def append(v: L[A], as: Set[A]): L[A] = as.foldLeft(v)((v, a) => add(v, a))
  def size(v: L[A]): Int
}

class SetVisitedSetImpl[A] {
  object VisitedSet extends VisitedSet[A, Set]{
    type T = Set[A]
    def empty                                    = Set.empty
    def contains(v: Set[A], a: A)                = v.contains(a)
    def exists(v: Set[A], a: A, p: A => Boolean) = v.exists(p)
    def add(v: Set[A], a: A)                     = v + a
    def size(v: Set[A])                          = v.size
  }
}

class MapVisitedSetImpl[A : WithKey] {
  object VisitedSet extends VisitedSet[A, KeyMap] {
    type T = KeyMap[A]
    def empty = KeyMap[A]()
    def contains(v: KeyMap[A], a: A) =
      v.content(WithKey[A].key(a)).contains(a)
    def exists(v: KeyMap[A], a: A, p: A => Boolean) =
      v.content(WithKey[A].key(a)).exists(p)
    def add(v: KeyMap[A], a: A) =
      new KeyMap(v.content + (WithKey[A].key(a) -> (v.content(WithKey[A].key(a)) + a)))
    def size(v: KeyMap[A]) =
      v.content.foldLeft(0)((x, kv) => kv match { case (_, v) => x + v.size })
  }
}

class Map2VisitedSetImpl[A : WithKey2] {
  object VisitedSet extends VisitedSet[A, KeyMap2] {
    type T = KeyMap2[A]
    def empty = KeyMap2[A]()
    def contains(v: KeyMap2[A], a: A) =
      v.content(WithKey2[A].key1(a))(WithKey2[A].key2(a)).contains(a)
    def exists(v: KeyMap2[A], a: A, p: A => Boolean) =
      v.content(WithKey2[A].key1(a))(WithKey2[A].key2(a)).contains(a)
    def add(v: KeyMap2[A], a: A) = {
      val k1 = WithKey2[A].key1(a)
      val k2 = WithKey2[A].key2(a)
      val v1 = v.content(k1)
      val v2 = v1(k2)
      new KeyMap2(v.content + (k1 -> (v1 + (k2 -> (v2 + a)))))
    }
    def size(v: KeyMap2[A]) =
      v.content.foldLeft(0)((x, kv) => kv match {
        case (_, v) => v.foldLeft(x)((x, kv) => kv match {
          case (_, v) => x + v.size
        })
      })
  }
}
