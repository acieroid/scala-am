package scalaam.lattice

import scalaam.core.Lattice

/** A lattice for booleans */
trait BoolLattice[B] extends Lattice[B] {
  def inject(b: Boolean): B
  def isTrue(b: B): Boolean
  def isFalse(b: B): Boolean
  def not(b: B): B
  def top: B

  trait BoolLatticeLaw extends LatticeLaw {

    /**
      * Inject preserves truthiness
      * isTrue(inject(true)) ∧ isFalse(inject(false))
      */
    def injectPreservesTruthiness: Boolean =
      isTrue(inject(true)) && isFalse(inject(false))

    /**
      * Top is both true and false (when defined)
      * isTrue(⊤) ∧ isFalse(⊤)
      */
    def topTrueAndFalse: Boolean = isTrue(top) && isFalse(top)

    /**
      * Bottom is neither true nor false
      * ¬isTrue(⊥) ∧ ¬isFalse(⊥)
      */
    def bottomNotTrueNorFalse: Boolean = !isTrue(bottom) && !isFalse(bottom)

    /**
      * Not reverses truthiness
      * ∀ a: isTrue(a) ⇒ isFalse(not(a)) ∧ isFalse(a) ⇒ isTrue(not(a))
      */
    def notReversesTruthiness(a: B): Boolean =
      conditional(isTrue(a), isFalse(not(a))) && conditional(isFalse(a), isTrue(not(a)))

    /**
      * Not is involutive
      * ∀ a: not(not(a)) == a
      */
    def notInvolutive(a: B): Boolean =
      not(not(a)) == a
  }
  val boolLatticeLaw = new BoolLatticeLaw {}
}

object BoolLattice {
  def apply[B: BoolLattice]: BoolLattice[B] = implicitly
}
