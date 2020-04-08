package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._

// next to deciding *when* an adaptive (here: argument-sensitive) analysis needs to adapt ...
// ... it also needs to decide *how* to adapt (here: which arguments to drop)
trait AdaptiveArgumentSelection extends AdaptiveArgumentSensitivity {
  // the default implementation is quite stupid: it drop *all* arguments to meet the limit
  def dropArgs(clo: lattice.Closure, args: Set[ArgumentMapping], limit: Int): Set[Identifier] 
}

trait NaiveAdaptiveArgumentSelection extends AdaptiveArgumentSelection {
  // naive policy: just drop all arguments from the argument-sensitivity
  def dropArgs(clo: lattice.Closure, args: Set[ArgumentMapping], limit: Int) = clo._1.args.toSet
}

trait EagerAdaptiveArgumentSelection extends AdaptiveArgumentSelection {

  def dropArgs(clo: lattice.Closure, args: Set[ArgumentMapping], limit: Int): Set[Identifier] = {
    // compute all possible argument values per identifier
    val valuesPerArg = args.foldLeft(Map[Identifier, Set[Value]]().withDefaultValue(Set())) { 
      case (acc,argMapping) =>
        argMapping.foldLeft(acc) { case (acc,(idn,vlu)) => acc + (idn -> (acc(idn) + vlu)) }
    }
    // sort identifiers: those with the most values come first!
    val sortedIdentifiers = valuesPerArg.toList.sortBy(_._2.size).map(_._1)
    // drop arguments one after another, until limit is satisfied
    var includedIdentifiers = sortedIdentifiers
    var droppedIdentifiers = Set[Identifier]()
    var currentArgs = args
    while (currentArgs.size > limit) {
      val nextArgToDrop = includedIdentifiers.head
      includedIdentifiers = includedIdentifiers.tail
      droppedIdentifiers += nextArgToDrop
      currentArgs = currentArgs.map(_ - nextArgToDrop)
    }
    // drop arguments that have been dropped
    return droppedIdentifiers
  }
}