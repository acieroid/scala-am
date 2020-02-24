package scalaam.modular.adaptive

import scalaam.core._
import scalaam.modular._
import scalaam.util._
import scalaam.util.MonoidImplicits._

abstract class AdaptiveModAnalysis[Expr <: Expression](program: Expr) extends ModAnalysis(program) with MutableIndirectComponents[Expr] {

  import IndirectComponents._

  // after every step, the adaptive analysis gets an opportunity to reflect on (introspection) and possible change (intercession) the analysis behaviour
  // the method `adaptAnalysis` needs to be implemented to decide when and how this is carried out
  protected def adaptAnalysis(): Unit
  override def step(): Unit = {
    super.step()
    adaptAnalysis()
  }
  // the core of the adaptive analysis: one needs to implement how components are to be "adapted"
  // the idea is that the definition/results of `adaptComponent` can change during the analysis itself ...
  def adaptComponent(cmp: ComponentData): ComponentData
  // .. and that when this happens, one needs to call `updateAnalysis`
  def updateAnalysis(): Unit = {
    this.cMap = this.cMap.mapValues(adaptComponent).toMap   // adapt the components
    val current = this.cMap.keys.map(addr => (addr,addr)).toMap
    val updated = updateComponentMapping(current)          // update the indirection maps and calculate the "new component pointer" for every "old component pointer"
    updateAnalysisData(cmp => ComponentPointer(updated(cmp.addr)))  // update all components pointers in the analysis
  }

  @scala.annotation.tailrec
  private def updateComponentMapping(current: Map[Address,Address]): Map[Address,Address] = {
    var remapping = Map[Address,Address]()
    this.cMapR = this.cMap.foldLeft(Map[ComponentData,Address]()) {
      case (acc, (oldAddr, cmp)) => acc.get(cmp) match {
        case None =>
          acc + (cmp -> oldAddr)
        case Some(newAddr) =>
          remapping += (oldAddr -> newAddr)
          acc
      }
    }
    if (remapping.isEmpty) {
      //assert(this.cMapR == this.cMap.map(_.swap))
      //assert(this.cMap == this.cMapR.map(_.swap))
      current
    } else {
      val updateAddress = (addr: Address) => remapping.getOrElse(addr, addr)
      val updateComponent = (ptr: ComponentPointer) => ComponentPointer(updateAddress(ptr.addr))
      this.cMap = this.cMapR.map { case (cmp,addr) => (addr,updateCmp(updateComponent)(cmp)) }
      val updated = current.view.mapValues(updateAddress).toMap
      updateComponentMapping(updated)
    }
  }

  // ... which in turn calls `updateAnalysisData` to update the component pointers
  def updateAnalysisData(update: Component => Component) = {
    work            = updateSet(update)(work)
    visited         = updateSet(update)(visited)
    allComponents   = updateSet(update)(allComponents)
    dependencies    = updateMap(update,updateSet(update))(dependencies)
    deps            = updateMap(updateDep(update),updateSet(update))(deps)
  }
  // the analysis' data structures need to be updated after adaptation, as some components may now be equal
  // the following methods need to be implemented by a subclass, since they depend on the representation of 'ComponentData' and 'Dependency'
  def updateCmp(update: Component => Component)(cmp: ComponentData): ComponentData
  def updateDep(update: Component => Component)(dep: Dependency): Dependency = dep
  // the following methods are convenience methods to update standard compound data structures
  def updateSet[A](update: A => A)(set: Set[A]): Set[A] = set.map(update)
  def updateMap[K,V](update: V => V)(map: Map[K,V]): Map[K,V] = map.view.mapValues(update).toMap
  def updateMap[K, V : Monoid](updateK: K => K, updateV: V => V)(map: Map[K,V]): Map[K,V] =
    map.foldLeft(Map[K,V]().withDefaultValue(Monoid[V].zero)) { case (acc,(key,vlu)) =>
      val keyAbs = updateK(key)
      acc + (keyAbs -> Monoid[V].append(acc(keyAbs),updateV(vlu)))
    }



}
