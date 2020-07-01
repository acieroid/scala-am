package scalaam.modular.adaptive

import scalaam.modular.components.IndirectComponents
import scalaam.core._
import scalaam.modular._
import scalaam.util._
import scalaam.util.MonoidImplicits._
import scalaam.util.benchmarks.Timeout

abstract class AdaptiveModAnalysis[Expr <: Expression](program: Expr) extends ModAnalysis(program) 
                                                                        with IndirectComponents[Expr]
                                                                        with SequentialWorklistAlgorithm[Expr]
                                                                        with DependencyTracking[Expr] {

  import scalaam.modular.components.IndirectComponents._

  // after every step, the adaptive analysis gets an opportunity to reflect on (introspection) and possible change (intercession) the analysis behaviour
  // the method `adaptAnalysis` needs to be implemented to decide when and how this is carried out
  protected def adaptAnalysis(): Unit
  override def step(timeout: Timeout.T): Unit = {
    super.step(timeout)
    adaptAnalysis()
  }
  // the scalaam.core of the adaptive analysis: one needs to implement how components are to be "adapted"
  // the idea is that the definition/results of `adaptComponent` can change during the analysis itself ...
  def adaptComponent(cmp: ComponentData): ComponentData
  // .. and that when this happens, one needs to call `updateAnalysis`
  def updateAnalysis(): Unit = {
    // update the indirection maps and calculate the "new component pointer" for every "old component pointer"
    val current = this.cMap.map({ case (addr, _) => (addr,addr) }).toMap
    val (updated, moved) = updateComponentMapping(this.cMapR, adaptComponent, current)
    this.cMap = updated.map(_.swap)
    this.cMapR = updated
    // update all components pointers in the analysis
    updateAnalysisData(cmp => ComponentPointer(moved(cmp.addr)))
  }

  @scala.annotation.tailrec
  private def updateComponentMapping(current: Map[ComponentData,Address],
                                     update: ComponentData => ComponentData,
                                     moved: Map[Address,Address]): (Map[ComponentData,Address], Map[Address,Address]) = {
    var mapping = Map[Address,Address]()
    var updated = Map[ComponentData,Address]()
    current.foreach { case (oldCmp, oldAddr) =>
      val newCmp = update(oldCmp)
      updated.get(newCmp) match {
        case None           => updated += (newCmp -> oldAddr)
        case Some(newAddr)  => mapping += (oldAddr -> newAddr)
      }
    }
    if (mapping.isEmpty) {
      (updated, moved)
    } else {
      val updateAddress = (addr: Address) => mapping.getOrElse(addr, addr)
      val updatePointer = (ptr: ComponentPointer) => ComponentPointer(updateAddress(ptr.addr))
      updateComponentMapping(updated, updateCmp(updatePointer), moved.view.mapValues(updateAddress).toMap)
    }
  }

  // ... which in turn calls `updateAnalysisData` to update the component pointers
  def updateAnalysisData(update: Component => Component) = {
    workList        = workList.map(update)
    visited         = updateSet(update)(visited)
    newComponents   = updateSet(update)(newComponents)
    dependencies    = updateMap(update,updateSet(update))(dependencies)
    deps            = updateMap(updateDep(update),updateSet(update))(deps)
  }
  // the analysis' data structures need to be updated after adaptation, as some components may now be equal
  // the following methods need to be implemented by a subclass, since they depend on the representation of 'ComponentData' and 'Dependency'
  def updateCmp(update: Component => Component)(cmp: ComponentData): ComponentData
  def updateDep(update: Component => Component)(dep: Dependency): Dependency = dep
  // the following methods are convenience methods to update standard compound data structures
  def updateSet[V](update: V => V)(set: Set[V]): Set[V] = set.map(update)
  def updateMap[K,V](update: V => V)(map: Map[K,V]): Map[K,V] = map.view.mapValues(update).toMap
  def updateMap[K, V : Monoid](updateK: K => K, updateV: V => V)(map: Map[K,V]): Map[K,V] =
    map.foldLeft(Map[K,V]().withDefaultValue(Monoid[V].zero)) { case (acc,(key,vlu)) =>
      val keyAbs = updateK(key)
      acc + (keyAbs -> Monoid[V].append(acc(keyAbs),updateV(vlu)))
    }
  def updatePair[P,Q](updateA: P => P, updateB: Q => Q)(p: (P,Q)): (P,Q) = (updateA(p._1),updateB(p._2))
}
