package scalaam.modular.adaptive.scheme

import scalaam.core.Position._
import scalaam.language.scheme._

trait AdaptiveCallerSensitivity extends AdaptiveSchemeModFSemantics {
    // set a limit for the maximum number of recursive calls to a component
    val limit: Int 
    // the context is just the calling component
    type ComponentContext = (Component, Position)
    def updateCtx(update: Component => Component)(ctx: ComponentContext) = 
        (update(ctx._1), ctx._2)
    // adapting the caller argument
    def adaptCaller(clo: lattice.Closure, caller: Component, position: Position): ComponentContext = ???
    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) =
        adaptCaller(clo, caller, call)
    override def onNewComponent(cmp: Component, call: Call) = ???
    def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
        case Main                               => Main
        case Call(clo,nam,(caller,callsite))    => Call(clo, nam, adaptCaller(clo,caller,callsite))
    }
    def registerCall(source: (Component, Position), target: Component) = ???
    // we need to update the `calledBy` data structure whenever the analysis is adapted
    override def updateAnalysisData(update: Component => Component) = {
        super.updateAnalysisData(update)
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