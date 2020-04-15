package scalaam.util

case class DisjointSet[A]
  (var tree: Map[A,A] = Map[A,A]().withDefault(a => a),
   val ranks: Map[A,Int] = Map[A,Int]().withDefaultValue(0)) {

  // MARK: Core methods

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

  def union(r1: A, r2: A): DisjointSet[A] = if (r1 == r2) { this } else {
    val rank1 = ranks(r1)
    val rank2 = ranks(r2)
    if (rank1 < rank2) {
      this.copy(tree = tree.updated(r1,r2))
    } else if (rank1 > rank2) {
      this.copy(tree = tree.updated(r2,r1))
    } else {
      this.copy(tree = tree.updated(r1,r2), ranks = ranks.updated(r2,rank2+1))
    }
  }

  def singleton(r: A): Boolean = (ranks(r) == 0)

}