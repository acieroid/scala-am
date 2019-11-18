package incremental

import incremental.Apted.costmodel.{PerEditOperationStringNodeDataCostModel, StringUnitCostModel}
import incremental.Apted.distance.APTED
import incremental.Apted.node.Node
import scalaam.core.{Expression, Label}
import scalaam.util.Annotations.toCheck

import scala.collection.mutable
import scala.util.control.Breaks._

/**
 * This file contains the implementations of the Gumtree code differences as presented in <br><br>
 *    Jean-Rémy Falleri, Floréal Morandat, Xavier Blanc, Matias Martinez, Martin Monperrus:<br>
 *    Fine-grained and accurate source code differencing. ASE 2014: 313-324.<br><br>
 * For clarity, comments in this code may stem from this paper.
 */
object GumTreeDiff {

  type E  = Expression
  type MP = Map[T, T]

  /** Class of AST nodes. Contains extra metadata in comparison to the plain AST used by Scala-AM. */
  case class T(self: E, parent: T) {                                        // Parent is excluded from equality (== and !=).
    val height:          Int = self.height                                  // The height of a tree is defined so that leaf nodes have height 1.
    val children:    List[T] = self.subexpressions.map(T(_, this))          // Cache direct descendants.
    val descendants: List[T] = children ::: children.flatMap(_.descendants) // Cache all descendants.
    val descSize:        Int = descendants.size
    val label:         Label = self.label                                   // Labels of nodes correspond to the name of their production rule in the grammar.

    override def toString: String = s"$self@${self.pos}"

    // TODO: use another isomorphic comparison method (paper: O(1) ?)
    // TODO: can this function be derived from s or can caching be used to increase efficiency?
    /** Returns a boolean indicating whether t1 and t2 are isomorphic. */
    def isomorphic(other: T): Boolean = label == other.label && children.zip(other.children).forall(t => t._1.isomorphic(t._2)) // Makes use of laziness of &&.
  }

  // Another layer needed since apted may modify the tree?
  //case class N(node: T) extends Node(node) {
  //  val children: List[N] = node.opened.map(N)
  //}

  /**
   * This procedure implements the GumTree differencing algorithm, specialised for the Scheme AST of Scala-AM.
   * This procedure outputs a mapping between the old and new AST. More information about the algorithm can be found here: <br>
   * <pre>
   *    Jean-Rémy Falleri, Floréal Morandat, Xavier Blanc, Matias Martinez, Martin Monperrus:
   *    Fine-grained and accurate source code differencing. ASE 2014: 313-324.
   * </pre>
   * @param E1          The source AST, the original AST before changes.
   * @param E2          The destination ADT, the AST after changes.
   * @param minHeight   The minimum height subtrees must have to be matched, default: 2.
   * @param maxSize     The maximum size subtrees may have to be used with an algorithm that finds the smallest edit scripts without move actions. RTED is currently used.
   * @param minDice     The minimum common descendant ratio for subtrees to be matched.
   * @return A mapping between nodes from the old AST to nodes from the updated AST. Nodes are represented using the class T defined within this object.
   */
  def map(E1: E, E2: E, minHeight: Int = 2, maxSize: Int = 100, minDice: Double = 0.5): MP = {
    val T1 = T(E1, null)
    val T2 = T(E2, null)
    bottomUp(T1, T2, topDown(T1, T2, minHeight), maxSize, minDice)
  }

  /**
   * Finds isomorphic subtrees of decreasing height and establishes a mapping between the nodes of these isomorphic subtrees.
   * Mappings established in this phase are callled anchor mappings.
   * @param T1          An AST.
   * @param T2          The AST to which T1 is compared.
   * @param minHeight   The minimum height subtrees must have to be matched.
   * @return A mapping between the nodes of the isomorphic subtrees.
   */
  private def topDown(T1: T, T2: T, minHeight: Int): MP = {
    // Height-indexed priority lists
    var L1 = new mutable.PriorityQueue[T]()(Ordering.by(_.height))
    var L2 = new mutable.PriorityQueue[T]()(Ordering.by(_.height))
    L1 += T1
    L2 += T2

    var A: Set[(T, T)] = Set() // Set of candidate mappings.
    var M: MP = Map()          // Mapping.

    val subs1 = T1.descendants // All subexpressions of T1.
    val subs2 = T2.descendants // All subexpressions of T2.
    breakable {
      while (true) {                                        // While the biggest subexpressions are of sufficient height.
        val n1 = L1.headOption.getOrElse(break())
        val n2 = L2.headOption.getOrElse(break())
        if (n1.height.min(n2.height) <= minHeight) break()

        if (n1.height > n2.height) {                        // If one tree is higher than the other, open all expressions of the corresponding length.
          val toOpen = L1.takeWhile(_.height == n1.height)  // toOpen = pop(L1)
          L1 = L1.drop(toOpen.length)                           // Actual pop
          toOpen.foreach(t => L1 ++= t.children)              // foreach t ∈ toOpen do open(t, L1)
        } else if (n2.height > n1.height) {
          val toOpen = L2.takeWhile(_.height == n2.height)
          L2 = L2.drop(toOpen.length)
          toOpen.foreach(t => L2 ++= t.children)
        } else {                                            // If the trees have equal height, try to match isomorphic nodes. TODO what is the difference between s(T) and t ∈ T
          val height = n1.height
          val H1 = L1.takeWhile(_.height == height).toList  // H1 = pop(L1)
          val H2 = L2.takeWhile(_.height == height).toList  // H2 = pop(L2)
          L1 = L1.drop(H1.length)                               // Actual pop
          L2 = L2.drop(H2.length)                               // Actual pop
          for (t1 <- H1) {                                  // Foreach (t1, t1) ∈ H1 x H2
            val s1 = t1.descendants
            for (t2 <- H2) {
              if (t1.isomorphic(t2)) {
                if (subs2.exists(tx => t1.isomorphic(tx) && tx != t2) || subs1.exists(tx => tx.isomorphic(t2) && tx != t1)) // TODO is this what is meant with exists tx ∈ T2
                  A = A + ((t1, t2))                        // Add t1 -> t2 to the set of candidate mappings.
                else {
                  val s2 = t2.descendants
                  s1.zip(s2).foreach(t => M = M + t)        // Add all pairs of isomorphic nodes of s(t1) and s(t2) to M. TODO is this correct or do we need the explicit cartesian product?
                }
              }
            }
          }
          val AM = A ++ M.toSet
          H1.foreach(t1 => if (!AM.exists(_._1 == t1)) L1 ++= t1.children)
          H2.foreach(t2 => if (!AM.exists(_._2 == t2)) L2 ++= t2.children)
        }
      }
    }
    var Asorted = A.toList.sortBy({case (t1, t2) => dice(t1.parent, t2.parent, M)})
    while (Asorted.nonEmpty) {
      val (t1, t2) = Asorted.head                           // (t1, t2) <- remove(A, 0)
      Asorted = Asorted.tail
      t1.descendants.zip(t2.descendants).foreach(t => M = M + t) // Add all pairs of isomorphic nodes of s(t1) and s(t2) to M. TODO is this correct or do we need the explicit cartesian product?
      A.filterNot({case (x, y) => x == t1 || y == t2})      // A <- A \ {(t1, tx) ∈ A} \ {(tx, t2) ∈ A}
    }
    M
  }

  /**
   * Matches nodes if there are a significant number of anchor mappings between their decendants (= container mappings).<br>
   * An additional mapping phase is performed on the descendants of matched nodes, resulting in recovery mappings.
   * @param T1        An AST.
   * @param T2        The AST to which T1 is compared.
   * @param m         The mapping constructed by the top-down phase.
   * @param maxSize   The maximal size for trees to be scanned for recovery mappings.
   * @param minDice   A minimum measure of similarity that is required to find mappings.
   * @return An extended mapping between T1 and T2.
   */
  private def bottomUp(T1: T, T2: T, m: MP, maxSize: Int, minDice: Double): MP = {
    var M: MP = m
    val Q = new mutable.PriorityQueue[T]()(Ordering.by((_: T).height).reverse) // Reverse the order.
    Q ++= T1.descendants.filter(t => M.get(t).isEmpty && t.children.flatMap(M.get(_)).nonEmpty) // TODO is this correct or should the tests really happen one after another (after more matches are made)?
    while (Q.nonEmpty) {                                    // Foreach t1 ∈ T1 | t1 is not matched and t1 has matched children, in post-order
      val t1 = Q.dequeue()
      candidate(t1, T2, M) match {
        case Some(t2) if dice(t1, t2, M) > minDice =>
          M = M + (t1 -> t2)
          if (t1.descSize.max(t2.descSize) < maxSize)
            opt(t1, t2).foreach{ case (ta, tb) =>
              if (   !m.contains(ta)
                  && !m.exists{case (_, t) => t == tb}
                  && ta.label == tb.label)
                M = M + (ta -> tb)
            }
        case _ =>
      }
    }
    M
  }

  /**
   * Measure for the ratio of common descendants between two nodes give a mapping.<br>
   * dice(t1, t2, M) = 2 * |{ t1 ∈ s(t1), t2 ∈ s(t2) | (t1 , t2) ∈ M }| / (|s(t1)| + |s(t2)|)
   **/
  @toCheck("This definition differs from the definition in the paper! (Assume formula in paper is not entirely correct.")
  private def dice(t1: T, t2: T, M: MP): Double = 2.0 * t1.descendants.count(t => M.contains(t) && t2.descendants.contains(M(t))).toDouble / (t1.descSize + t2.descSize).toDouble

  /**
   * Returns all possible candidate matches for t1. A node t ∈ T2 is a candidate for t1 if
   * <ul>
   *   <li>label(t1) = label(t),</li>
   *   <li>t is unmatched,</li>
   *   <li>t1 and t have some matching descendants.</li>
   * </ul>
   **/
  private def candidate(t1: T, T2: T, M: MP): Option[T] = {
    T2.descendants.filter{ t => t.label == t1.label && !M.contains(t1) && haveMatchedDescendants(t1, t, M)} match {
      case Nil => None
      case lst => Some(lst.maxBy(dice(t1, _, M)))
    }
  }

  /** Returns a boolean indicating whether t1 and t2 have matched descendants in mapping M. */
  private def haveMatchedDescendants(t1: T, t2: T, M: MP): Boolean = t1.descendants.exists(t => t2.descendants.contains(M(t)))

  // TODO - the current implementation is a dummy implementation
  /** Finds the mapping corresponding to the shortest edit script without move actions.
   *  Gumtree originally uses RTED (Pawlik and Augsten, 2011).
   **/
  private def opt(t1: T, t2: T): List[(T, T)] = for {s1 <- t1.descendants; s2 <- t2.descendants} yield (s1, s2)
  /* {
    val apted = new APTED[StringUnitCostModel, T](new StringUnitCostModel())
    apted.computeEditDistance(N(t1), N(t2))
    val map = apted.computeEditMapping()
    println(map)
    List()
  } */
}
