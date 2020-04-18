package scalaam.modular.adaptive.scheme

import scalaam.core.Position._
import scalaam.language.scheme._
import scalaam.util.MonoidImplicits._

trait AdaptiveCallerSensitivity extends AdaptiveSchemeModFSemantics {
    // set a limit for the maximum number of recursive calls to a component
    val limit: Int 
    // the context is just the calling component
    type ComponentContext = (Component, Position)
    def updateCtx(update: Component => Component)(ctx: ComponentContext) = 
        (update(ctx._1), ctx._2)
    // adapting the caller argument
    var adaptedCallers = Map[lattice.Closure, Set[ComponentContext]]()
    private def addAdaptedCaller(clo: lattice.Closure, ctx: ComponentContext) = 
        adaptedCallers += (clo -> (adaptedCallers.getOrElse(clo,Set()) + ctx))
    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) =
        adaptCaller(clo, caller, call)
    def adaptCaller(clo: lattice.Closure, caller: Component, callsite: Position) = adaptedCallers.get(clo) match {
        case None => (caller, callsite)
        case Some(callers) => calledBy.getOrElse(caller,Set()).find(callers.contains(_)) match {
            case None => (caller, callsite) 
            case Some(adapted) => adapted
        } 
    }
    override def onNewComponent(cmp: Component, call: Call) = {
        val callStack = calledBy(cmp)
        val prevCmps: Set[Component] = callStack.collect(p => view(p._1) match {
            case cll: Call if cll.clo == call.clo => p._1
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
        case Main                               => Main
        case Call(clo,nam,(caller,callsite))    => Call(clo, nam, adaptCaller(clo,caller,callsite))
    }
    // for every component, keep track of all other components that can reach it ...
    var calledBy = Map[Component, Set[ComponentContext]]()
    // ... what follows is boring boilerplate code to correctly do the bookkeeping in the `calledBy` map
    def registerCall(source: (Component, Position), target: Component) = {
        lazy val targetCalledBy = calledBy.getOrElse(target, Set())
        lazy val sourceCalledBy = calledBy.getOrElse(source._1, Set())
        if (!targetCalledBy.contains(source)) {
            val newComponents = (sourceCalledBy + source) -- targetCalledBy
            calledBy += (target -> (targetCalledBy ++ newComponents))
            propagate(List((target, newComponents)))
        }
    }
    @scala.annotation.tailrec
    private def propagate(worklist: List[(Component,Set[ComponentContext])]): Unit =
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
        calledBy = updateMap(update, updateSet(updateCtx(update)))(calledBy)
        adaptedCallers = updateMap(updateClosure(update), updateSet(updateCtx(update)))(adaptedCallers)
    }
    // we instrument the intra-analysis to perform the necessary bookkeeping for 'calledBy' whenever a function is called
    override def intraAnalysis(cmp: Component) = new AdaptiveSchemeModFAnalysisIntra(cmp)
    class AdaptiveSchemeModFAnalysisIntra(component: Component) extends super.IntraAnalysis(component) {
        var currentPos: Position = null
        override def applyClosures(fun: Value, args: List[(SchemeExp,Value)], cll: Position, cmp: Component) = {
            this.currentPos = cll
            super.applyClosures(fun,args,cll,cmp)
        }
        override def call(cmp: Component) = {
            registerCall((component,currentPos),cmp)
            super.call(cmp)
        }
    }
}