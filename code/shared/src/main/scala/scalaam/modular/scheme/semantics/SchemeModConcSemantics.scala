package scalaam.modular.scheme.semantics

import scalaam.core._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.modular.scheme._
import scalaam.util.benchmarks.Timeout

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

    abstract class SchemeModConcIntra(cmp: Component) extends IntraAnalysis(cmp)
                                                         with GlobalStoreIntra { intra =>
        // TODO: create a new trait for this kind of intra analysis, then parameterize the ModConc analysis with a constructor for such analyses
        val modFAnalysis = new ModAnalysis[SchemeExp](body(cmp)) with BaseSchemeModFSemantics
                                                                 with BigStepModFSemantics
                                                                 with NoSensitivity
                                                                 with StandardSchemeModFComponents
                                                                 with FIFOWorklistAlgorithm[SchemeExp] {
            // SCHEME SEMANTICS SETUP
            type Addr = inter.Addr
            type Value = inter.Value
            lazy val lattice = inter.lattice 
            lazy val initialEnv: Environment[Addr] = inter.env(cmp)
            // GLOBAL STORE SETUP 
            def componentAddr(cmp: Component, addr: Address) = intra.allocAddr(ComponentAddr(cmp,addr))
            def sharedAddr(addr: Address) = intra.allocAddr(addr)
            def read(addr: Addr) = Some(intra.readAddr(addr))
            def write(addr: Addr, value: Value) = intra.writeAddr(addr, value)
            // INTRA-ANALYSIS
            def intraAnalysis(cmp: Component) = ???
        }
        def analyze(timeout: Timeout.T): Unit = {
            
        }
    }

}