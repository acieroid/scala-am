package scalaam.modular.scheme.modconc

import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.language.CScheme._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.util.benchmarks.Timeout

import scalaam.modular.scheme.modf.EvalM._

trait SchemeModConcSemantics extends ModAnalysis[SchemeExp]
                                with ReturnValue[SchemeExp]
                                with ContextSensitiveComponents[SchemeExp]
                                with SchemeSetup { inter =>

    type Env = Environment[Addr]     

    //
    // MODCONC COMPONENTS
    //

    type Component <: TID
    def view(cmp: Component): SchemeModConcComponent
    def initialComponent: Component 
    def newComponent(thread: Thread[ComponentContext]): Component
    
    def expr(cmp: Component): SchemeExp = body(cmp)
    def body(cmp: Component): SchemeExp = body(view(cmp))
    def body(cmp: SchemeModConcComponent): SchemeExp = cmp match {
        case MainThread         => program
        case Thread(bdy, _, _)  => bdy
    }

    def env(cmp: Component): Env = env(view(cmp))
    def env(cmp: SchemeModConcComponent): Env = cmp match {
        case MainThread         => initialEnv
        case Thread(_, env, _)  => env
    }
    
    type ComponentContent = (SchemeExp, Env)
    def content(cmp: Component) = view(cmp) match {
        case MainThread             => (program, initialEnv)
        case Thread(bdy, env, _)    => (bdy, env)
    }
    type ComponentContext
    def context(cmp: Component) = view(cmp) match {
        case MainThread                     => None
        case t: Thread[ComponentContext]    => Some(t.ctx)
    }
    // parameterize to allow different sensitivities for threads
    def allocCtx(exp: SchemeExp, env: Env, modFCmp: SchemeModFComponent, caller: Component): ComponentContext

    // allocating addresses
    type AllocationContext
    def allocVar(id: Identifier, modFCmp: SchemeModFComponent, cmp: Component): VarAddr[AllocationContext]
    def allocPtr(exp: SchemeExp, modFCmp: SchemeModFComponent, cmp: Component): PtrAddr[AllocationContext]

    //
    // MODCONC INTRA-ANALYSIS
    //

    override def intraAnalysis(cmp: Component) = new SchemeModConcIntra(cmp)
    class SchemeModConcIntra(cmp: Component) extends IntraAnalysis(cmp)
                                                with GlobalStoreIntra
                                                with ReturnResultIntra {
        // create a ModF analysis to analyze the thread
        val modFAnalysis = inter.modFAnalysis(this)
        def analyze(timeout: Timeout.T): Unit = {
            modFAnalysis.analyze(timeout)
            writeResult(modFAnalysis.finalResult)
        }
    }

    // the user can choose which ModF analysis to use inside the ModConc analysis
    def modFAnalysis(intra: SchemeModConcIntra): InnerModFAnalysis 
    abstract class InnerModFAnalysis(intra: SchemeModConcIntra) extends ModAnalysis[SchemeExp](body(intra.component))
                                                                    with BaseSchemeModFSemantics
                                                                    with BigStepModFSemantics
                                                                    with StandardSchemeModFComponents {
        // SCHEME ENVIRONMENT SETUP
        lazy val baseEnv = env(intra.component)
        // SCHEME LATTICE SETUP
        type Value = inter.Value
        lazy val lattice = inter.lattice 
        lazy val primitives = inter.primitives
        // MODF ADDRESS ALLOCATION
        type AllocationContext = inter.AllocationContext
        def allocVar(id: Identifier, cmp: SchemeModFComponent) = inter.allocVar(id, cmp, intra.component)
        def allocPtr(exp: SchemeExp, cmp: SchemeModFComponent) = inter.allocPtr(exp, cmp, intra.component)                                                           
        // GLOBAL STORE SETUP 
        override def store = intra.store
        override def store_=(s: Map[Addr,Value]) = intra.store = s
        // SYNCING DEPENDENCIES
        override def register(target: Component, dep: Dependency) = {
            super.register(target, dep)
            intra.register(dep)
        }
        override def trigger(dep: Dependency) = {
            super.trigger(dep)
            intra.trigger(dep)
        }
        // MODF INTRA-ANALYSIS EXTENDED WITH SUPPORT FOR THREADS
        def intraAnalysis(cmp: Component) = new InnerModFIntra(cmp)
        class InnerModFIntra(cmp: Component) extends IntraAnalysis(cmp) with BigStepModFIntra {
            var T: Set[inter.Component] = Set.empty
            def spawnThread(t: inter.Component) = T += t 
            def readThreadResult(t: inter.Component) = readAddr(inter.returnAddr(t))            
            override def eval(exp: SchemeExp, env: Env): EvalM[Value] = exp match {
                case CSchemeFork(bdy, _)    => evalFork(bdy, env)
                case CSchemeJoin(thr, _)    => evalJoin(thr, env)
                case _                      => super.eval(exp, env)   
            }
            private def evalFork(exp: SchemeExp, env: Env): EvalM[Value] = {
                val ctx = inter.allocCtx(exp, env, component, intra.component)
                val targetCmp = inter.newComponent(Thread(exp, env, ctx))
                spawnThread(targetCmp)
                unit(lattice.thread(targetCmp))
            }
            private def evalJoin(thrExp: SchemeExp, env: Env): EvalM[Value] =
                for {
                    thrVal <- eval(thrExp, env)
                    threads = lattice.getThreads(thrVal)
                    values = threads.map(tid => inject(readThreadResult(tid.asInstanceOf[inter.Component]))(lattice))
                    res <- merge(values)(lattice)
                } yield res
            override val interpreterBridge = new SchemeInterpreterBridge[Addr] {
                def pointer(exp: SchemeExp): Addr = allocPtr(exp, component)
                def currentThread = intra.component
            }
            override def commit() = {
                super.commit()
                T.foreach(intra.spawn)
            }
        }    
    } 
}

// convenience constructor
abstract class SimpleSchemeModConcAnalysis(prg: SchemeExp) extends ModAnalysis(prg)
                                                              with SchemeModConcSemantics
                                                              with StandardSchemeModConcAllocator
                                                              with StandardSchemeModConcComponents

// examples
// TODO: put different inner ModF instantiations in different traits for ModConc
class MyModConcAnalysis1(prg: SchemeExp) extends SimpleSchemeModConcAnalysis(prg)
                                            with SchemeModConcStandardSensitivity
                                            with SchemeConstantPropagationDomain
                                            with LIFOWorklistAlgorithm[SchemeExp] {
    override def modFAnalysis(intra: SchemeModConcIntra) = new ModFAnalysis(intra)
    class ModFAnalysis(intra: SchemeModConcIntra) extends InnerModFAnalysis(intra)
                                                    with SchemeModFNoSensitivity
                                                    with RandomWorklistAlgorithm[SchemeExp]
}