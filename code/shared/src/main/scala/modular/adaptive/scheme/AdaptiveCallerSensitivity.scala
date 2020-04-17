package scalaam.modular.adaptive.scheme

import scalaam.core.Position._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.modular.adaptive._
import scalaam.util.MonoidImplicits._

trait AdaptiveCallerSensitivity extends AdaptiveSchemeModFSemantics {
    // set a limit for the maximum number of recursive calls to a component
    val limit: Int 
    // the context is just the calling component
    type ComponentContext = Component
    def updateCtx(update: Component => Component)(ctx: ComponentContext) = update(ctx)
    // adapting the caller argument
    var adaptedCallers = Map[lattice.Closure, Set[Component]]()
    private def addAdaptedCaller(clo: lattice.Closure, caller: Component) = 
        adaptedCallers += (clo -> (adaptedCallers.getOrElse(clo,Set()) + caller))
    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) =
        adaptCaller(clo, caller)
    def adaptCaller(clo: lattice.Closure, caller: Component) = adaptedCallers.get(clo) match {
        case None => caller
        case Some(callers) => calledBy.getOrElse(caller,Set()).find(callers.contains(_)) match {
            case None => caller 
            case Some(adapted) => adapted
        } 
    }
    override def onNewComponent(cmp: Component, call: Call) = {
        val callStack = calledBy(cmp)
        val prevCmps: Set[Component] = callStack.collect(c => view(c) match {
            case cll: Call if cll.clo == call.clo => c
        })
        val updatedCmps = prevCmps + cmp
        if (updatedCmps.size > limit) {
            val firstCmp = updatedCmps.minBy(calledBy(_).size)
            val caller = view(firstCmp).asInstanceOf[Call].ctx
            addAdaptedCaller(call.clo, caller)
            updateAnalysis()
        }
    }
    def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
        case Main                   => Main
        case Call(clo,nam,caller)   => Call(clo, nam, adaptCaller(clo,caller))
    }
    // for every component, keep track of all other components that can reach it ...
    var calledBy = Map[Component, Set[Component]]()
    // ... what follows is boring boilerplate code to correctly do the bookkeeping in the `calledBy` map
    def registerCall(source: Component, target: Component) = {
        lazy val targetCalledBy = calledBy.getOrElse(target, Set())
        lazy val sourceCalledBy = calledBy.getOrElse(source, Set())
        if (!targetCalledBy.contains(source)) {
            val newComponents = (sourceCalledBy + source) -- targetCalledBy
            calledBy += (target -> (targetCalledBy ++ newComponents))
            propagate(List((target,newComponents)))
        }
    }
    @scala.annotation.tailrec
    private def propagate(worklist: List[(Component,Set[Component])]): Unit =
        if(worklist.nonEmpty) {
        val (current, newComponents) :: rest = worklist
        val cmps = dependencies.getOrElse(current,Set())
        val updatedWorklist = cmps.foldLeft(rest) { (acc,cmp) =>
            val cmpCalledBy = calledBy(cmp) // contains at least the 'current' component
            val componentsToAdd = newComponents -- cmpCalledBy
            if(componentsToAdd.isEmpty) {
                acc
            } else {
                calledBy += (cmp -> (cmpCalledBy ++ componentsToAdd))
                (cmp, componentsToAdd) :: acc
            }
        }
        propagate(updatedWorklist)
    }
    // we need to update the `calledBy` data structure whenever the analysis is adapted
    override def updateAnalysisData(update: Component => Component) = {
        super.updateAnalysisData(update)
        calledBy = updateMap(update, updateSet(update))(calledBy)
        adaptedCallers = updateMap(updateClosure(update), updateSet(update))(adaptedCallers)
    }
    // we instrument the intra-analysis to perform the necessary bookkeeping for 'calledBy' whenever a function is called
    override def intraAnalysis(cmp: Component) = new AdaptiveSchemeModFAnalysisIntra(cmp)
    class AdaptiveSchemeModFAnalysisIntra(component: Component) extends super.IntraAnalysis(component) {
        override def call(cmp: Component) = {
            registerCall(component,cmp)
            super.call(cmp)
        }
    }
}