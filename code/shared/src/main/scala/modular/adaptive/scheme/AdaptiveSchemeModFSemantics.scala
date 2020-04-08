package scalaam.modular.adaptive.scheme

import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.modular.adaptive._

/** Semantics for an adaptive Scheme MODF analysis. */
trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                    with AdaptiveGlobalStore[SchemeExp]
                                    with AdaptiveReturnValue[SchemeExp]
                                    with SchemeModFSemantics
                                    with BigStepSemantics
                                    with AbstractDomain {

  // Definition of components
  trait ComponentData extends SchemeComponent
  case object Main extends ComponentData with MainComponent
  case class Call(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext) extends ComponentData with CallComponent
  lazy val initialComponent: Component = { init() ; ref(Main) } // Need init to initialize reference bookkeeping information.
  def newComponent(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext): Component = ref(Call(clo,nam,ctx))

  // Definition of update functions
  def updateClosure(update: Component => Component)(clo: lattice.Closure) = clo match {
    case (lambda, parent) => (lambda, update(parent))
  }
  def updateCmp(update: Component => Component)(cmp: ComponentData): ComponentData = cmp match {
    case Main => Main
    case Call(clo,nam,ctx) => Call(updateClosure(update)(clo),nam,updateCtx(update)(ctx))
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext): ComponentContext
  def updateValue(update: Component => Component)(value: Value): Value = value match {
    case valueLattice.Element(v)    => valueLattice.Element(updateV(update)(v))
    case valueLattice.Elements(vs)  => vs.map(updateV(update)) match {
      case vs if vs.size == 1 => valueLattice.Element(vs.head)
      case vs => valueLattice.Elements(vs)
    }
  }
  private def updateV(update: Component => Component)(value: valueLattice.Value): valueLattice.Value = value match {
    case valueLattice.Pointer(addr)     => valueLattice.Pointer(updateAddr(update)(addr))
    case valueLattice.Clo(lam,cmp,nam)  => valueLattice.Clo(lam,update(cmp),nam)
    case valueLattice.Cons(car,cdr)     => valueLattice.Cons(updateAddr(update)(car),updateAddr(update)(cdr))
    case valueLattice.Vec(siz,els,ini)  => valueLattice.Vec(siz,els.view.mapValues(updateValue(update)).toMap,updateValue(update)(ini))
    case _                              => value
  }

  // parameterized by a function that can adapt the analysis whenever a new component is 'discovered'
  // should return true iff the new component triggered an adaptation of the analysis
  protected def adaptOnNewComponent(cmp: Component, call: Call): Boolean
  // to "adapt" the analysis, every new component is passed on to `adaptOnNewComponent`, which may trigger adaptation
  override protected def adaptAnalysis() = {
    // for each new component, allow the analysis to be adapted
    var adapted = false
    this.newComponents.foreach(cmp => {
      val call = deref(cmp).asInstanceOf[Call]
      if(adaptOnNewComponent(cmp,call)) {
        adapted = true
      }
    })
    // if it was adapted => update the analysis!
    if(adapted) { updateAnalysis() }
  }
}
