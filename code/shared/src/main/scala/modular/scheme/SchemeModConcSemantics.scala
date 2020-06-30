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
                                with ContextSensitiveComponents[SchemeExp] { inter =>

    //
    // MODCONC COMPONENTS
    //

    def view(cmp: Component): SchemeModConcComponent[ComponentContext,Addr]
    def initialComponent: Component 
    def newComponent(thread: Thread[ComponentContext,Addr]): Component

    def body(cmp: Component): SchemeExp = body(view(cmp))
    def body(cmp: SchemeModConcComponent[ComponentContext,Addr]): SchemeExp = cmp match {
        case MainThread         => program
        case Thread(bdy, _, _)  => bdy
    }

    def env(cmp: Component): Env = env(view(cmp))
    def env(cmp: SchemeModConcComponent[ComponentContext,Addr]): Env = cmp match {
        case MainThread         => initialEnv
        case Thread(_, env, _)  => env
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

    //
    // MODCONC INTRA-ANALYSIS
    //

    class SchemeModConcIntra(cmp: Component) extends IntraAnalysis(cmp) { intra =>
        // TODO: create a new trait for this kind of intra analysis, then parameterize the ModConc analysis with a constructor for such analyses
        val modFAnalysis = new ModAnalysis[SchemeExp](body(cmp)) with BaseSchemeModFSemantics
                                                                 with BigStepModFSemantics
                                                                 with NoSensitivity
                                                                 with StandardSchemeModFComponents
                                                                 with FIFOWorklistAlgorithm[SchemeExp] {
            type Addr = inter.Addr
            type Value = inter.Value
            lazy val lattice = inter.lattice 
            lazy val initialEnv: Environment[Addr] = inter.env(cmp) 
            def componentAddr(cmp: Component, addr: Address) = ???
            def sharedAddr(addr: Address) = ???
            override var store: Map[Addr,Value] = ???
        }
        def analyze(timeout: Timeout.T): Unit = {
            
        }
    }

}