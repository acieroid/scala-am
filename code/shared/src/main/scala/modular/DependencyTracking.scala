package scalaam.modular

import scalaam.core._

// A common, but optional extension to ModAnalysis
// Specifically, it keeps track of:
// - which components have spawned which other components
// - which components have been spawned by the last intra-analysis
trait DependencyTracking[Expr <: Expression] extends ModAnalysis[Expr] { inter =>
  var dependencies  = Map[Component,Set[Component]]().withDefaultValue(Set.empty)
  var newComponents = Set[Component]() 
  // update some rudimentary analysis results
  trait DependencyTrackingIntra extends IntraAnalysis {
    val visited = inter.visited
    override def commit(): Unit = {
      super.commit()
      // update the bookkeeping
      newComponents = C.filterNot(visited)
      dependencies += component -> (dependencies(component) ++ C)
    }
  }
}