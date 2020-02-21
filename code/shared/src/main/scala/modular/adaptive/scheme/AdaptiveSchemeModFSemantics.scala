package scalaam.modular.adaptive.scheme

import scalaam.core._
import scalaam.modular.adaptive._
import scalaam.language.scheme._
import scalaam.modular.scheme._
import scalaam.util.MonoidImplicits._

/** Semantics for an adaptive Scheme MODF analysis. */
trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                    with AdaptiveGlobalStore[SchemeExp]
                                    with AdaptiveReturnValue[SchemeExp]
                                    with SchemeModFSemantics
                                    with AbstractDomain {

  // component data are SchemeComponents
  trait ComponentData extends SchemeComponent
  // Definition of the initial component
  case object Main extends ComponentData with MainComponent
  // Definition of call components
  case class Call(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext) extends ComponentData with CallComponent
  // initial component
  lazy val initialComponent: Component = { init() ; ref(Main) } // Need init to initialize reference bookkeeping information.
  // new components
  def newComponent(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext): Component = ref(Call(clo,nam,ctx))

  // A context is a partial mapping from the closure's formal parameters to argument values
  case class ComponentContext(args: Map[Identifier, Value]) {
   override def toString = args.toList
                               .map({ case (i,v) => s"$i -> $v" })
                               .mkString(" ; ")
  }

  // It's a partial mapping, because some parameters are not included for a given closure
  // (this way, we can fine-tune argument-sensitivity to avoid scalability issues with certain parameters)
  private var excludedArgs = Map[lattice.Closure, Set[Identifier]]().withDefaultValue(Set.empty)
  private def excludeArg(clo: lattice.Closure, par: Identifier) =
   excludedArgs += (clo -> (excludedArgs(clo) + par))
  private def excludeArgs(clo: lattice.Closure, prs: Set[Identifier]) =
   prs.foreach(par => excludeArg(clo,par))
  // The context for a given closure only consists of argument values for non-excluded parameters for that closure
  def allocCtx(clo: lattice.Closure, args: List[Value]): ComponentContext =
   ComponentContext(clo._1.args.zip(args).toMap -- excludedArgs(clo))
  // Updating closures, components and values is easy
  def updateClosure(update: Component => Component)(clo: lattice.Closure) = clo match {
    case (lambda, parent) => (lambda, update(parent))
  }
  def updateCmp(update: Component => Component)(cmp: ComponentData): ComponentData = cmp match {
    case Main => Main
    case Call(clo,nam,ctx) => Call(updateClosure(update)(clo),nam,updateCtx(update)(ctx))
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext) =
    ComponentContext(updateMap(updateValue(update))(ctx.args))
  def updateValue(update: Component => Component)(value: Value): Value = value match {
    case valueLattice.Element(v)    => valueLattice.Element(updateV(update)(v))
    case valueLattice.Elements(vs)  => valueLattice.Elements(vs.map(updateV(update)))
  }
  private def updateV(update: Component => Component)(value: valueLattice.Value): valueLattice.Value = value match {
    case valueLattice.Pointer(addr)     => valueLattice.Pointer(updateAddr(update)(addr))
    case valueLattice.Clo(lam,cmp,nam)  => valueLattice.Clo(lam,update(cmp),nam)
    case valueLattice.Cons(car,cdr)     => valueLattice.Cons(updateValue(update)(car),updateValue(update)(cdr))
    case valueLattice.Vec(siz,els,ini)  => valueLattice.Vec(siz,els.view.mapValues(updateValue(update)).toMap,updateValue(update)(ini))
    case _                              => value
  }

  // To adapt an existing component, we drop the argument values for parameters that have to be excluded
  def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
    case Main => Main
    case Call(clo,nam,ctx) =>
      val adaptedCtx = ComponentContext(ctx.args -- excludedArgs(clo))
      Call(clo,nam,adaptedCtx)
  }

  // this gets called whenever new components are added to the analysis
  // it calls `adaptArguments` for every new component, and "adapts" the analysis if necessary
  override protected def adaptAnalysis() = {
    val updates = this.newComponents.foldLeft(List.empty[(lattice.Closure,Set[Identifier])]) {
      (acc, cmp) => view(cmp) match {
        case Main => throw new Exception("This should not happen!")
        case call: Call =>
          val excludedPars = adaptArguments(cmp,call)
          if (excludedPars.nonEmpty) { (call.clo, excludedPars) :: acc } else { acc }
      }
    }
    // update the argument-sensitivity policy (of course, only if there are updates) and signal that the definition of alpha has changed
    if (updates.nonEmpty) {
      updates.foreach { case (clo,prs) => excludeArgs(clo,prs) }
      updateAnalysis()
    }
  }

  // parameterized by a simple limit
  // every closure can only have at most "limit" components
  val limit: Int
  // track the number of components per closure
  private var closureCmps = Map[lattice.Closure, Set[Component]]()
  // The main logic for adaptive argument sensitivity
  // parameterized by a method `adaptArguments` to configure the concrete policy
  // This method takes a new component that is added to the analysis ...
  // ... and can decide which parameters of the corresponding closure need to be "excluded"
  def adaptArguments(cmp: Component, call: Call): Set[Identifier] = {
    val Call(clo, _, ComponentContext(bds)) = call
    // update the set of components per closure
    val prvCmps = closureCmps.get(clo).getOrElse(Set())
    val newCmps = prvCmps + cmp
    closureCmps += (clo -> newCmps)
    // if there are too many components => do something about it!
    if (limit < newCmps.size) {
      clo._1.args.toSet // naive policy: drop all of the argument-sensitivity
    } else { // otherwise, not arguments need to be excluded
      Set.empty
    }
  }

  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    excludedArgs = updateMap(updateClosure(update), (s: Set[Identifier]) => s)(excludedArgs)
    closureCmps = updateMap(updateClosure(update),updateSet(update))(closureCmps)
  }
}
