package scalaam.language.lambda

import scalaam.core._
import scalaam.lattice._

trait LambdaLattice[L, A <: Address] extends Lattice[L] {
  def function(e: LambdaExp, env: Environment[A]): L
  def closures(f: L): Set[(LambdaExp, Environment[A])]
}

object LambdaLattice {
  def apply[L, A <: Address]()(implicit l: LambdaLattice[L, A]): LambdaLattice[L, A] = l
}

trait LambdaLatticeTypeclass[A <: Address] {
  type L
  val typeclass: LambdaLattice[L, A]
}

case class LambdaSetLattice[A <: Address]() extends LambdaLatticeTypeclass[A] {
  type L = Set[(LambdaExp, Environment[A])]

  val typeclass = new LambdaLattice[L, A] {
    def function(e: LambdaExp, env: Environment[A]) = Set((e, env))
    def closures(f: L) = f

    def bottom = Set.empty
    def join(x: L, y: L) = x.union(y)
    def subsumes(x: L, y: L) = y.subsetOf(x)
  }
  val typeclassL: Lattice[L] = typeclass
}
