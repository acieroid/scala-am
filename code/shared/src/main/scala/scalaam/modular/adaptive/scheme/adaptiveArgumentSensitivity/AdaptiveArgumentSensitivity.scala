package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
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
  protected def widenArg(fexp: SchemeLambdaExp, par: Identifier, arg: Value) = {
    val otherCalls = cmpsPerFn(fexp).map { view(_).asInstanceOf[Call[ComponentContext]] }
    val otherArgVs = otherCalls.map(_.ctx.args(fexp.args.indexOf(par)))
    val widenedArg = otherArgVs.foldLeft(arg) { (acc, arg) =>
      if(lattice.subsumes(arg,acc)) { arg } else { acc }
    }
    val currentAbs = adaptedArgs.getOrElse(par,Set.empty)
    val updatedAbs = currentAbs.filterNot(lattice.subsumes(widenedArg,_)) + widenedArg
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
    case Main                                 => Main
    case Call(clo,nam,ctx: ComponentContext)  => Call(clo, nam, ComponentContext(adaptArgs(clo,ctx.args)))
  }
  // HELPER FUNCTIONS FOR `joinComponents`
  // TODO: Extract and put this somewhere else
  private def extractComponentRefs(value: Value): Set[Component] = value match {
    case modularLatticeWrapper.modularLattice.Elements(vs)  => vs.flatMap(extractComponentRefs).toSet
  }
  private def extractComponentRefs(v: modularLatticeWrapper.modularLattice.Value): Set[Component] = v match {
    case modularLatticeWrapper.modularLattice.Pointer(ps)       => ps.flatMap(extractComponentRefs)
    case modularLatticeWrapper.modularLattice.Clo(cs)           => cs.map(clo => ???)
    case modularLatticeWrapper.modularLattice.Cons(car,cdr)     => extractComponentRefs(car) ++ extractComponentRefs(cdr)
    case modularLatticeWrapper.modularLattice.Vec(_,els)        => els.flatMap(p => extractComponentRefs(p._2)).toSet
    case _                              => Set.empty
  }
  private def extractComponentRefs(addr: Addr): Set[Component] = ??? //TODO
  private def getClosure(cmp: Component): Option[lattice.Closure] = view(cmp) match {
    case Main       => None
    case call: Call[ComponentContext] => Some(call.clo)
  }
  var toJoin = List[Set[Component]]()
  def joinComponents(cmps: Set[Component]) = {
    this.toJoin = List(cmps)
    while (toJoin.nonEmpty) {
      val next :: rest = this.toJoin
      // look at the next closure + contexts
      val calls = next.map(view(_).asInstanceOf[Call[ComponentContext]])
      this.toJoin = ??? // calls.map(_.clo._2) :: rest
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
        this.toJoin = this.toJoin ::: refs.toList
        widenArg(calls.head.clo._1, par, joined)
      }
      updateAnalysis()
      this.toJoin = this.toJoin.filterNot(_.size == 1).toList   
    }
    //println("DONE")
  }
  var cmpsPerFn = Map[SchemeExp, Set[Component]]()
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = {
    // update the function to components mapping
    cmpsPerFn += (call.clo._1 -> (cmpsPerFn.get(call.clo._1).getOrElse(Set()) + cmp))
    // if any of the new arguments subsumes an existing widened argument, update that widened argument
    var adapted = false
    call.clo._1.args.zip(call.ctx.args).foreach { case (par,arg) =>
      val widened = adaptedArgs.getOrElse(par,Set())
      if(widened.exists(lattice.subsumes(arg,_))) {
        widenArg(call.clo._1, par, arg)
        adapted = true
      }
    }
    if (adapted) { updateAnalysis() }
  }
  // we need to update the adaptedArgs mapping when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.adaptedArgs = updateMap(updateSet(updateValue(update)))(adaptedArgs)
    this.cmpsPerFn = updateMap(updateSet(update))(cmpsPerFn)
    this.toJoin = toJoin.map(updateSet(update))
  }
}
