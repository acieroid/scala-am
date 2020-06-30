package scalaam.modular.scheme

import scalaam.core._
import scalaam.modular._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.language.CScheme._
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.util.benchmarks.Timeout
import scalaam.lattice.ConstantPropagation

trait SchemeModConcSemantics extends ModAnalysis[SchemeExp]
                                with DedicatedSchemeSemantics
                                with ReturnValue[SchemeExp]
                                with ContextSensitiveComponents[SchemeExp] {

    // COMPONENTS

    def view(cmp: Component): SchemeModConcComponent[ComponentContext,Addr]
    def initialComponent: Component 
    def newComponent(thread: Thread[ComponentContext,Addr]): Component

    def body(cmp: Component): SchemeExp = body(view(cmp))
    def body(cmp: SchemeModConcComponent[ComponentContext,Addr]): SchemeExp = cmp match {
        case MainThread         => program
        case Thread(bdy, _, _)  => bdy
    }
    
    type ComponentContent = (SchemeExp, Env)
    def content(cmp: Component) = view(cmp) match {
        case MainThread             => (program, initialEnv)
        case Thread(bdy, env, _)    => (bdy, env)
    }
    def context(cmp: Component) = view(cmp) match {
        case MainThread             => None
        case Thread(_, _, ctx)      => Some(ctx)
    }

    class SchemeModConcIntra(cmp: Component) extends IntraAnalysis(cmp) {
        val modFAnalysis = new ModAnalysis[SchemeExp](body(cmp)) with BaseSchemeModFSemantics
                                                                 with BigStepModFSemantics
                                                                 with NoSensitivity
                                                                 with ModFConstantPropagationDomain
                                                                 with StandardSchemeModFComponents
                                                                 with FIFOWorklistAlgorithm[SchemeExp]
        def analyze(timeout: Timeout.T): Unit = {
            
        }
    }

}