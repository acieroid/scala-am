package scalaam.util

/**
 * When using `s1 ++ s2` with the default Scala Set implementation,
 * it can be significantly more efficient to use `s2 ++ s1` if s1 is smaller than s2
 **/
object SmartUnion {
    def sunion[A](s1: Set[A], s2: Set[A]): Set[A] =
        if(s1.size < s2.size) {
            s2 ++ s1
        } else {
            s1 ++ s2
        }
}