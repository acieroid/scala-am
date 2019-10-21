package scalaam.modular

import scalaam.core._
import scalaam.util._
import scalaam.util.MonoidImplicits._

abstract class ModAnalysis[Expr <: Exp](program: Expr) {

  // parameterized by a 'intra-component' representation
  type IntraComponent
  val initial: IntraComponent

  // an intra-analysis of a component can cause effects that impact the analysis of other components
  // an intra-analysis therefore can:
  // - register a dependency on an effect (e.g., when it reads an address)
  // - trigger an effect (e.g., when it writes to an address)
  protected trait Effect
  // here, we track which components depend on which effects
  var deps = Map[Effect,Set[IntraComponent]]().withDefaultValue(Set())
  private def addDep(component: IntraComponent, effect: Effect) =
    deps += (effect -> (deps(effect) + component))

  // parameterized by an 'intra-component analysis'
  protected def intraAnalysis(component: IntraComponent): IntraAnalysis
  protected abstract class IntraAnalysis(val component: IntraComponent) {
    // keep track of effects triggered by this intra-analysis
    private[ModAnalysis] var effects = Set[Effect]()
    protected def pushEffect(eff: Effect) = effects += eff
    protected def pullEffect(eff: Effect) = addDep(component,eff)
    // keep track of components called by this intra-analysis
    private[ModAnalysis] var components = Set[IntraComponent]()
    protected def spawn(cmp: IntraComponent) = components += cmp
    // analyses the given component
    def analyze(): Unit
  }

  // inter-analysis using a simple worklist algorithm
  var worklist = Set[IntraComponent](initial)
  var analysed = Set[IntraComponent]()
  var allComponents = Set[IntraComponent](initial)
  var componentDeps = Map[IntraComponent,Set[IntraComponent]]()
  def finished = worklist.isEmpty
  def step() = {
    // take the next component
    val current = worklist.head
    worklist -= current
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyze()
    // add the successors to the worklist
    val newComponents = intra.components.filterNot(analysed)
    val componentsToUpdate = intra.effects.flatMap(deps)
    val succs = newComponents ++ componentsToUpdate
    succs.foreach(succ => worklist += succ)
    // update the analysis
    analysed += current
    allComponents ++= newComponents
    componentDeps += (current -> intra.components)
  }
  def analyze() = while(!finished) { step() }
}

abstract class AdaptiveModAnalysis[Expr <: Exp](program: Expr) extends ModAnalysis(program) {

  // parameterized by an alpha function, which further 'abstracts' components
  // alpha can be used to drive an adaptive strategy for the analysis
  protected def alpha(cmp: IntraComponent): IntraComponent
  // based on this definition of alpha, we can induce 'compound versions' of this function
  protected def alphaSet[A](alphaA: A => A)(set: Set[A]): Set[A] = set.map(alphaA)
  protected def alphaMap[K, V : Monoid](alphaK: K => K, alphaV: V => V)(map: Map[K,V]): Map[K,V] =
    map.foldLeft(Map[K,V]().withDefaultValue(Monoid[V].zero)) { case (acc,(key,vlu)) =>
      val keyAbs = alphaK(key)
      acc + (keyAbs -> Monoid[V].append(acc(keyAbs),alphaV(vlu)))
    }
  // effects might require further abstraction too; subclasses can override this as needed ...
  protected def alphaEffect(eff: Effect): Effect = eff

  // when alpha changes, we need to call this function to update the analysis' components
  def onAlphaChange() = {
    worklist        = alphaSet(alpha)(worklist)
    analysed        = alphaSet(alpha)(analysed)
    allComponents   = alphaSet(alpha)(allComponents)
    componentDeps   = alphaMap(alpha,alphaSet(alpha))(componentDeps)
    deps            = alphaMap(alphaEffect,alphaSet(alpha))(deps)
  }
}
