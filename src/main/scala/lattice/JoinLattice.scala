import scalaz._
import scalaz.Scalaz._

/** A (join semi-)lattice L should support the following operations */
trait JoinLattice[L] extends Monoid[L] with PartialOrdering[L] {
  def typesOf(v: L): Set[Recorder.Typ.Typ] = ???
  /** A lattice has a bottom element */
  def bottom: L
  /** Elements of the lattice can be joined together */
  def join(x: L, y: L): L
  def append(x: L, y: => L): L = join(x, y) /* A lattice is trivially a monoid by using join as append */
  def zero: L = bottom /* And bottom as zero */
  /** Subsumption between two elements can be checked */
  def subsumes(x: L, y: L): Boolean
  def lteq(x: L, y: L): Boolean = subsumes(y, x) /* A lattice has a partial order, defined by subsumes */
  def tryCompare(x: L, y: L): Option[Int] = (subsumes(x, y), subsumes(y, x)) match {
    case (true, true) => Some(0) // x >= y and y >= x => x = y
    case (true, false) => Some(1) // x >= y and x != y
    case (false, true) => Some(-1) // y >= x and x != y
    case (false, false) => None // not comparable
  }

  /** We have some more components for convenience */
  /** A name identifying the lattice */
  def name: String
  /** It should state whether it supports abstract counting or not. (TODO: this is probably not the best place for that) */
  def counting: Boolean

  /** Some elements may contain addresses in there and are therefore not considered as primitive values */
  def isPrimitiveValue(x: L): Boolean
  /** Cardinality of this value */
  def cardinality(x: L): Cardinality

  trait JoinLatticeLaw {
    import scalaz.std.boolean.conditional
    def bottomLowerBound(a: L): Boolean = subsumes(a, bottom)
    def joinCommutative(a: L, b: L): Boolean = join(a, b) == join(b, a)
    def joinAssociatve(a: L, b: L, c: L): Boolean = join(join(a, b), c) == join(a, join(b, c))
    def joinIdempotent(a: L): Boolean = join(a, a) == a
    def joinSubsumesCompatible(a: L, b: L): Boolean =
      conditional(subsumes(b, a), join(a, b) == b)
  }
}

object JoinLattice {
  def apply[L : JoinLattice]: JoinLattice[L] = implicitly
}

/** Cardinality represents how much information a lattice value represents */
sealed trait Cardinality
case object CardinalityInf extends Cardinality
case class CardinalityNumber(n: Int) extends Cardinality
/* TODO: In some cases we might not want to have cardinalities for values such
 * as numbers, strings, etc. For example, cardinality of {Int, clo(...),
 * clo(...)}  is infinity, while it would make more sense to have it as either 2
 * or 3. We might want to split cardinalities between pointer-like values
 * (closures, primitive functions, cons, etc.) to primitive-like values (ints,
 * strings, etc.). One just needs to change the following objects to case
 * classes and adapt the monoid.
 */
object CardinalityPrimitiveLikeInf {
  def apply(): Cardinality = CardinalityInf
}
object CardinalityPrimitiveLikeNumber {
  def apply(n: Int): Cardinality = CardinalityNumber(n)
}
object Cardinality {
  implicit val cardinalityAddMonoid: Monoid[Cardinality] = new Monoid[Cardinality] {
    def zero: Cardinality = CardinalityNumber(0)
    def append(x: Cardinality, y: => Cardinality): Cardinality = (x, y) match {
      case (CardinalityInf, _) | (_, CardinalityInf) => CardinalityInf
      case (CardinalityNumber(m), CardinalityNumber(n)) => CardinalityNumber(m + n)
    }
  }
}
