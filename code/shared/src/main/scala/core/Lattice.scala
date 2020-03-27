package scalaam.core

import scalaam.util.Show

/** Cardinality indicates how many elements are represented by an abstract value. */
case class Cardinality(fin: Int, inf: Int) extends Ordered[Cardinality] {
  override def toString: String = s"|$fin,$inf|"

  /** A cardinality is smaller than another one if it has
   *   - less infinite values OR
   *   - an equal amount of infinite values but less finite values.
   */
  override def compare(that: Cardinality): Int = {
    if (inf < that.inf || inf == that.inf && fin < that.fin) -1
    else if (inf == that.inf && fin == that.fin) 0
    else 1
  }

  def add(that: Cardinality): Cardinality = Cardinality(fin + that.fin, inf + that.inf)
}

/** Error raised when trying to construct the top element of a lattice which doesn't have one */
object LatticeTopUndefined extends ScalaAMException

/** A lattice typeclass.
  * It is actually a join-semi lattice as it only need a join operation and a bottom element
  */
trait Lattice[L] extends PartialOrdering[L] with Show[L] {

  /** A lattice has a bottom element */
  def bottom: L

  /** A lattice has a top element (might be undefined) */
  def top: L

  /** Elements of the lattice can be joined together */
  def join(x: L, y: => L): L

  /** Subsumption between two elements can be checked */
  def subsumes(x: L, y: => L): Boolean

  /** Equality check, returning an abstract result */
  def eql[B: scalaam.lattice.BoolLattice](x: L, y: L): B

  /** Cardinality indicates how many elements are represented by an abstract value */
  def cardinality(abs: L): Cardinality

  /** For PartialOrdering[L]: a lattice has a partial order, defined by subsumes... */
  final def lteq(x: L, y: L): Boolean = subsumes(y, x)

  /** ...and elements of the lattice can be compared */
  final def tryCompare(x: L, y: L): Option[Int] = (subsumes(x, y), subsumes(y, x)) match {
    case (true, true)   => Some(0)  // x >= y and y >= x => x = y
    case (true, false)  => Some(1)  // x >= y and x != y
    case (false, true)  => Some(-1) // y >= x and x != y
    case (false, false) => None     // not comparable
  }
}

object Lattice {
  def apply[L: Lattice]: Lattice[L] = implicitly

  implicit def SetLattice[A: Show] = new Lattice[Set[A]] {
    def show(x: Set[A]): String                                      = "{" ++ x.map(Show[A].show _).mkString(",") ++ "}"
    def top                                                          = throw LatticeTopUndefined
    def bottom: Set[A]                                               = Set.empty
    def join(x: Set[A], y: => Set[A]): Set[A]                        = x.union(y)
    def subsumes(x: Set[A], y: => Set[A]): Boolean                   = y.subsetOf(x)
    def eql[B: scalaam.lattice.BoolLattice](x: Set[A], y: Set[A])  = ???
    def cardinality(x: Set[A]): Cardinality                          = Cardinality(x.size, 0)
  }
}
