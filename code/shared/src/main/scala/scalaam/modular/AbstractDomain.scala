package scalaam.modular

import scalaam.core._

trait AbstractDomain[Expr <: Expression] extends ModAnalysis[Expr] {
    type Value
    implicit val lattice: Lattice[Value]
}
