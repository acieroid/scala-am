package scalaam.modular.scheme.semantics

import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.CScheme._
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

    type Component <: TID
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
    // TODO: parameterize to allow different sensitivities for threads
    type ComponentContext = ()

    //
    // MODCONC INTRA-ANALYSIS
    //

    abstract class SchemeModConcIntra(cmp: Component) extends IntraAnalysis(cmp)
                                                         with GlobalStoreIntra
                                                         with ReturnResultIntra { intra =>
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
            def store = intra.store
            def store_=(s: Map[Addr,Value]) = intra.store = s            
            def sharedAddr(addr: Address) = intra.allocAddr(GlobalAddr(addr))
            def componentAddr(cmp: Component, addr: Address) = intra.allocAddr(ComponentAddr(cmp,addr))
            // SYNCING DEPENDENCIES
            def lift(dep: Dependency): inter.Dependency = dep match {
                case ReadWriteDependency(addr) => inter.ReadWriteDependency(addr)
            }
            override def register(target: Component, dep: Dependency) = {
                super.register(target, dep)
                intra.register(lift(dep))
            }
            override def trigger(dep: Dependency) = {
                super.trigger(dep)
                intra.trigger(lift(dep))
            }
            // INTRA-ANALYSIS
            def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra {
                var T: Set[inter.Component] = Set()
                def spawnThread(t: inter.Component) = T += t 
                def readThreadResult(t: inter.Component) = 
                    readAddr(ComponentAddr(t, ReturnAddr))               
                override def eval(exp: SchemeExp, env: Env) = exp match {
                    case CSchemeFork(bdy, _)    => evalFork(bdy, env)
                    case CSchemeJoin(thr, _)    => evalJoin(thr, env)
                    case _                      => super.eval(exp, env)   
                }
                private def evalFork(exp: SchemeExp, env: Env) = {
                    val targetCmp = inter.newComponent(Thread(exp, env, ()))
                    spawnThread(targetCmp)
                    lattice.thread(targetCmp)
                }
                private def evalJoin(thrExp: SchemeExp, env: Env) = {
                    val thrVal = eval(thrExp, env)
                    lattice.getThreads(thrVal).foldLeft(lattice.bottom)((acc, tid) => 
                        lattice.join(acc, readThreadResult(tid.asInstanceOf[inter.Component]))
                    )
                }
                override def commit() = {
                    super.commit()
                    T.foreach(intra.spawn)
                }
            }
        }
        def analyze(timeout: Timeout.T): Unit = {
            modFAnalysis.analyze(timeout)
            writeResult(modFAnalysis.finalResult)
        }
    }

}