package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.core.Position._
import scalaam.language.scheme._
import scalaam.modular.adaptive.scheme._

trait AdaptiveArgumentSensitivity extends AdaptiveSchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString = args.mkString(",")
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext) =
    ComponentContext(ctx.args.map(updateValue(update)))
  // to determine how arguments need to be adapted
  private var adaptedArgs = Map[Identifier, Set[Value]]()
  protected def widenArg(par: Identifier, arg: Value) = {
    val currentAbs = adaptedArgs.getOrElse(par,Set.empty)
    val updatedAbs = currentAbs.filterNot(lattice.subsumes(arg,_)) + arg
    this.adaptedArgs += (par -> updatedAbs)
  }
  private def adaptArg(par: Identifier, arg: Value) =
    adaptedArgs.getOrElse(par,Set.empty)
               .find(lattice.subsumes(_, arg))
               .getOrElse(arg)
  private def adaptArgs(clo: lattice.Closure, args: List[Value]) =
    clo._1.args.zip(args).map { case (par,arg) => adaptArg(par,arg) }
  // The context for a given closure only consists of argument values for non-excluded parameters for that closure
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) =
    ComponentContext(adaptArgs(clo, args))
  // To adapt an existing component, we drop the argument values for parameters that have to be excluded
  def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
    case Main               => Main
    case Call(clo,nam,ctx)  => Call(clo, nam, ComponentContext(adaptArgs(clo,ctx.args)))
  }
  // HELPER FUNCTIONS FOR `joinComponents`
  // TODO: Extract and put this somewhere else
  private def extractComponentRefs(value: Value): Set[Component] = value match {
    case valueLattice.Elements(vs)  => vs.flatMap(extractComponentRefs)
    case valueLattice.Element(v)    => extractComponentRefs(v)
  }
  private def extractComponentRefs(v: valueLattice.Value): Set[Component] = v match {
    case valueLattice.Pointer(addr)     => Set(extractComponentRefs(addr))
    case valueLattice.Clo(_,cmp,_)      => Set(cmp)
    case valueLattice.Cons(car,cdr)     => Set(extractComponentRefs(car),extractComponentRefs(cdr))
    case valueLattice.Vec(_,els,ini)    => extractComponentRefs(ini) ++ els.flatMap(p => extractComponentRefs(p._2))
    case _                              => Set.empty
  }
  private def extractComponentRefs(addr: Addr): Component = addr match {
    case ComponentAddr(cmp, _)  => cmp
    case ReturnAddr(cmp)        => cmp
  }
  private def getClosure(cmp: Component): Option[lattice.Closure] = view(cmp) match {
    case Main       => None
    case call: Call => Some(call.clo)
  }
  var toJoin = Set[Set[Component]]()
  def joinComponents(cmps: Set[Component]) = {
    this.toJoin = Set(cmps)
    while (toJoin.nonEmpty) {
      val next = this.toJoin.head
      this.toJoin = this.toJoin.tail
      // look at the next closure + contexts
      val calls = next.map(view(_).asInstanceOf[Call])
      this.toJoin += calls.map(_.clo._2)
      val (pars, args) = (calls.head.clo._1.args, calls.map(_.ctx.args))
      //println("Joining the following components:")
      //calls.foreach(call => println(s"- $call"))
      // join all the argument values per parameter
      val valuesPerPos = args.toList.transpose.map(_.toSet)
      pars.zip(valuesPerPos).foreach { case (par,args) =>
        val joined = lattice.join(args)
        val refs = extractComponentRefs(joined).groupBy(getClosure(_).map(_._1.idn))
                                               .values
                                               .map(_.toSet)
        this.toJoin ++= refs
        widenArg(par, joined)
      }
      updateAnalysis()
      this.toJoin = this.toJoin.filterNot(_.size == 1).toSet   
    }
  }
  var cmpsPerFn = Map[SchemeExp, Set[Component]]()
  override def onNewComponent(cmp: Component, call: Call) = 
    cmpsPerFn += (call.body -> (cmpsPerFn.get(call.body).getOrElse(Set()) + cmp))
  // we need to update the adaptedArgs mapping when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.adaptedArgs = updateMap(updateSet(updateValue(update)))(adaptedArgs)
    this.cmpsPerFn = updateMap(updateSet(update))(cmpsPerFn)
    this.toJoin = updateSet(updateSet(update))(toJoin)
  }
}
