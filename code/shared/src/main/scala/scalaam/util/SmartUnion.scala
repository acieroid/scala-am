package scalaam.util

/**
 * When using `s1 ++ s2` with the default Scala Set implementation,
 * it can be significantly more efficient to use `s2 ++ s1` if s1 is smaller than s2
 **/
object SmartUnion {
  def sunion[E](s1: Set[E], s2: Set[E]): Set[E] = if (s1.size < s2.size) s2 ++ s1 else s1 ++ s2
  def sunionList[E](s: List[Set[E]]): Set[E] = s.foldLeft(Set[E]())(sunion)
}