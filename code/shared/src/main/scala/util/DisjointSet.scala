package scalaam.util

class DisjointSet[A] {

  var tree: Map[A,A]    = Map[A,A]().withDefault(a => a)
  var ranks: Map[A,Int] = Map[A,Int]().withDefaultValue(0)

  // Core methods

  def find(a: A): A = {
    val parent = tree(a)
    if (parent == a) {
      a
    } else {
      val root = find(parent)
      tree = tree.updated(a,root)
      root
    }
  }

  def union(r1: A, r2: A): A = 
    if (r1 == r2) {
      return r1
    } else {
      val rank1 = ranks(r1)
      val rank2 = ranks(r2)
      if (rank1 < rank2) {
        tree = tree.updated(r1, r2)
        return r2
      } else if (rank1 > rank2) {
        tree = tree.updated(r2, r1)
        return r1
      } else {
        tree  = tree.updated(r1, r2) 
        ranks = ranks.updated(r2, rank2 + 1)
        return r2
      }
    }

  def merge(e1: A, e2: A) = 
    union(find(e1),find(e2))
  def mergeAll(elms: Iterable[A]): A = 
    elms.tail.foldLeft(elms.head)(union)

  def singleton(r: A): Boolean = (ranks(r) == 0)
}