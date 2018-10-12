package scalaam.core

/** A lattice typeclass.
 * It is actually a join-semi lattice as it only need a join operation and a bottom element
 */
trait Lattice[L] extends PartialOrdering[L] {
  /** A lattice has a bottom element */
  def bottom: L
  /** Elements of the lattice can be joined together */
  def join(x: L, y: L): L
  /** Subsumption between two elements can be checked */
  def subsumes(x: L, y: L): Boolean
  /** Equality check, returning an abstract result */
  //def eql[B : BoolLattice](x: L, y: L): B


  /** For Monoid[L]: a lattice is trivially a monoid by using join as append... */
  final def append(x: L, y: => L): L = join(x, y)
  /** ...and bottom as zero */
  final def zero: L = bottom

  /** For PartialOrdering[L]: a lattice has a partial order, defined by subsumes... */
  final def lteq(x: L, y: L): Boolean = subsumes(y, x)
  /** ...and elements of the lattice can be compared */
  final def tryCompare(x: L, y: L): Option[Int] = (subsumes(x, y), subsumes(y, x)) match {
    case (true, true) => Some(0) // x >= y and y >= x => x = y
    case (true, false) => Some(1) // x >= y and x != y
    case (false, true) => Some(-1) // y >= x and x != y
    case (false, false) => None // not comparable
  }


  /** Lattice properties */
  trait LatticeLaw {
    def conditional(p: Boolean, q: => Boolean): Boolean = !p || q

    /** Bottom is the lower bound of all elements in the lattice */
    def bottomLowerBound(a: L): Boolean = subsumes(a, bottom)
    /** The join operation is commutative */
    def joinCommutative(a: L, b: L): Boolean = join(a, b) == join(b, a)
    /** The join operation is associative */
    def joinAssociatve(a: L, b: L, c: L): Boolean = join(join(a, b), c) == join(a, join(b, c))
    /** The join operation is idempotent */
    def joinIdempotent(a: L): Boolean = join(a, a) == a
    /** The join operation is compatible with subsumption */
    def joinSubsumesCompatible(a: L, b: L): Boolean =
      conditional(subsumes(b, a), join(a, b) == b)
  }
}

object Lattice {
  def apply[L : Lattice]: Lattice[L] = implicitly
}

trait LatticeTypeclass {
  type L
  val typeclass: Lattice[L]
}
