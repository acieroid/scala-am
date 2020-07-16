package scalaam.modular.adaptive.scheme

import scalaam.modular.scheme.modf._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.modular.adaptive._

/** Semantics for an adaptive Scheme MODF analysis. */
trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                    with AdaptiveGlobalStore[SchemeExp]
                                    with SchemeModFSemantics
                                    with BigStepModFSemantics
                                    with ModularSchemeDomain {
  // Definition of components
  type ComponentData = SchemeModFComponent
  lazy val initialComponent: Component = { init() ; ref(Main) } // Need init to initialize reference bookkeeping information.
  def newComponent(call: Call[ComponentContext]): Component = ref(call)

  // Definition of update functions
  def updateClosure(update: Component => Component)(clo: lattice.Closure) = clo match {
    case (lambda, env) => (lambda, env.mapAddrs(updateAddr(update)))
  }
  def updateCmp(update: Component => Component)(cmp: ComponentData): ComponentData = cmp match {
    case Main => Main
    case Call(clo,nam,ctx: ComponentContext) => Call(updateClosure(update)(clo),nam,updateCtx(update)(ctx))
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext): ComponentContext
  def updateValue(update: Component => Component)(value: Value): Value = value match {
    case valueLattice.Elements(vs)  => valueLattice.Elements(vs.map(updateV(update)))
  }
  def updateV(update: Component => Component)(value: valueLattice.Value): valueLattice.Value = value match {
    case valueLattice.Pointer(ps)       => valueLattice.Pointer(ps.map(updateAddr(update)))
    case valueLattice.Clo(cs)           => valueLattice.Clo(cs.map(clo => (updateClosure(update)(clo._1), clo._2)))
    case valueLattice.Cons(car,cdr)     => valueLattice.Cons(updateValue(update)(car),updateValue(update)(cdr))
    case valueLattice.Vec(siz,els)      => valueLattice.Vec(siz,els.view.mapValues(updateValue(update)).toMap)
    case _                              => value
  }

  // callback function that can adapt the analysis whenever a new component is 'discovered'
  protected def onNewComponent(cmp: Component, call: Call[ComponentContext]): Unit = ()
  // go over all new components after each step of the analysis, passing them to `onNewComponent`
  // ensure that these new components are properly updated when an adaptation occurs using a field `toProcess` which is kept up-to-date!
  var toProcess = Set[Component]()
  override protected def adaptAnalysis() = {
    this.toProcess = this.newComponents
    while(toProcess.nonEmpty) {
      val cmp = toProcess.head
      toProcess = toProcess.tail
      val call = view(cmp).asInstanceOf[Call[ComponentContext]]
      onNewComponent(cmp, call)
    }
  }
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.toProcess = updateSet(update)(toProcess)
  }
  class AdaptiveSchemeModFIntra(cmp: Component) extends IntraAnalysis(cmp)
                                                   with BigStepModFIntra
                                                   with DependencyTrackingIntra
  override def intraAnalysis(cmp: Component) = new AdaptiveSchemeModFIntra(cmp)
}
