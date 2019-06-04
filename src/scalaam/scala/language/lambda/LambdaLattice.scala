package scalaam.language.lambda

import scalaam.core._

/**
  * The lattice we will use is a lattice with two extra functions: one to inject
  * a lambda-calculus function into the lattice, and one to retrieve the
  * closures (functions with environments) associated to a lattice value.  There
  * is some Scala-specific code going on here: our lattice type is L, and it is
  * a lattice if it implements the LambdaLattice[L, A] typeclass for some
  * address types A. We need an address type as a parameter because environments
  * are also parameterized by addresses (there are multiple implementations of
  * addresses in Scala-AM).
  */
trait LambdaLattice[L, A <: Address] extends Lattice[L] {
  /** Given a lambda expression and an environment, we can create a lattice
    * value */
  def function(e: LambdaExp, env: Environment[A]): L
  /** Given a lattice value, we can extract possibly one or more closures,
    * represented as a lambda expression and an environment */
  def closures(f: L): Set[(LambdaExp, Environment[A])]
}

/** This is a helper function for using the LambdaLattice[L, A] typeclass.
  * It allows to call a function of this typeclass as LambdaLattice[L, A].function(e, env).
  */
object LambdaLattice {
  def apply[L, A <: Address]()(implicit l: LambdaLattice[L, A]): LambdaLattice[L, A] = l
}

/** This is an implementation of the lattice trait defined above. This
  * implementation represents elements of the lattice as sets.
  */
case class LambdaSetLattice[A <: Address]() {
  /** L is an element of the lattiec, and contains closures */
  case class L(vals: Set[(LambdaExp, Environment[A])])

  object L {
    /** We now define the typeclass instance by defining the functions needed for Lattice[L] and LambdaLattice[L, A]. */
    implicit val typeclass = new LambdaLattice[L, A] {
      /** This is a to-string method */
      def show(x: L)                                  = "{" ++ x.vals.mkString(",") ++ "}"
      /** To inject a function into the lattice domain, we just wrap it inside an L element */
      def function(e: LambdaExp, env: Environment[A]) = L(Set((e, env)))
      /** To extract closures, we extract them from the L element */
      def closures(f: L)                              = f.vals

      /** The bottom (i.e., smallest) element of the lattice is the empty set */
      def bottom                  = L(Set.empty)
      /** We don't define a top (i.e., biggest) element of the lattice as it is not definable */
      def top                     = throw LatticeTopUndefined
      /** If two lattice elements have to be merged (joined), we take their union and return the resulting element. */
      def join(x: L, y: => L)     = L(x.vals.union(y.vals))
      /** One lattice element subsumes another if there is a subset relation between the two */
      def subsumes(x: L, y: => L) = y.vals.subsetOf(x.vals)

      import scalaam.lattice.BoolLattice
      /** We can define equality between two lattice elements by using the subsumes relation */
      def eql[B: BoolLattice](x: L, y: L): B = BoolLattice[B].inject(y.vals.subsetOf(x.vals))
    }
  }
}
