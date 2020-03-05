package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._

// next to deciding *when* an adaptive (here: argument-sensitive) analysis needs to adapt ...
// ... it also needs to decide *how* to adapt (here: which arguments to drop)
trait AdaptiveArgumentSelection extends AdaptiveArgumentSensitivity {
  // the default implementation is quite stupid: it drop *all* arguments to meet the limit
  def dropArgs(clo: lattice.Closure, args: Set[ArgumentMapping], limit: Int): Set[Identifier] 
}

trait NaiveAdaptiveArgumentSelection extends AdaptiveArgumentSelection {
  def dropArgs(clo: lattice.Closure, args: Set[ArgumentMapping], limit: Int) = clo._1.args.toSet
}