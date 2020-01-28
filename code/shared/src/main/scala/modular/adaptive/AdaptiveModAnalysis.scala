package scalaam.modular.adaptive

import scalaam.core.Expression
import scalaam.modular.ModAnalysis
import scalaam.util.Monoid
import scalaam.util.MonoidImplicits._

abstract class AdaptiveModAnalysis[Expr <: Expression](program: Expr) extends ModAnalysis(program) {

  // keep a cache between a component and its most recent abstraction
  private var cache = Map[Component,Component]()
  // look in the cache first, before applying a potentially complicated alpha function
  def alpha(cmp: Component): Component = cache.get(cmp) match {
    case Some(cmpAbs) => cmpAbs
    case None =>
      val cmpAbs = alphaCmp(cmp)
      cache = cache + (cmp -> cmpAbs)
      cmpAbs
  }

  // a flag that can be used to indicate that the analysis has been adapted recently
  var adapted = false
  // parameterized by an alpha function, which further 'abstracts' components
  // alpha can be used to drive an adaptive strategy for the analysis
  def alphaCmp(cmp: Component): Component
  // dependencies might require further abstraction too; subclasses can override this as needed ...
  protected def alphaDep(dep: Dependency): Dependency = dep
  // based on this definition of alpha, we can induce 'compound versions' of this function
  def alphaSet[A](alphaA: A => A)(set: Set[A]): Set[A] = set.map(alphaA)
  def alphaMap[K,V](alphaV: V => V)(map: Map[K,V]): Map[K,V] = map.view.mapValues(alphaV).toMap
  def alphaMap[K, V : Monoid](alphaK: K => K, alphaV: V => V)(map: Map[K,V]): Map[K,V] =
    map.foldLeft(Map[K,V]().withDefaultValue(Monoid[V].zero)) { case (acc,(key,vlu)) =>
      val keyAbs = alphaK(key)
      acc + (keyAbs -> Monoid[V].append(acc(keyAbs),alphaV(vlu)))
    }

  // the adaptive analysis can decide how to adapt using a method `adaptAnalysis` ...
  protected def adaptAnalysis(): Unit
  // ... which is triggered after every step in the analysis
  override def step(): Unit = {
    super.step()
    adaptAnalysis()
  }

  // when alpha changes, we need to call this function to update the analysis' components
  def onAlphaChange(): Unit = {
    cache           = Map[Component,Component]()
    adapted         = true  // hoist the flag\
    // adapt some internal data structures
    work            = alphaSet(alpha)(work)
    visited         = alphaSet(alpha)(visited)
    allComponents   = alphaSet(alpha)(allComponents)
    dependencies    = alphaMap(alpha,alphaSet(alpha))(dependencies)
    deps            = alphaMap(alphaDep,alphaSet(alpha))(deps)
  }
}
