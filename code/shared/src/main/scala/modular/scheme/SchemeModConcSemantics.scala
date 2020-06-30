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
                                with DedicatedGlobalStore[SchemeExp]
                                with ReturnValue[SchemeExp]
                                with ContextSensitiveComponents[SchemeExp] {

    // SCHEME SETUP

    type Env = Environment[Addr]
    type Prim = SchemePrimitive[Value,Addr]
    lazy val primitives: SchemePrimitives[Value,Addr] = new SchemeLatticePrimitives()
    lazy val initialBds: Iterable[(String,Addr,Value)] = primitives.allPrimitives.map {
        p => (p.name, GlobalAddr(PrmAddr(p.name)), lattice.primitive(p)) 
    }
    lazy val initialEnv = Environment(initialBds.map(bnd => (bnd._1, bnd._2)))
    implicit val lattice: SchemeLattice[Value, Addr, Prim]

    // prelude other primitives and translate the program to use lexical addresses
    override val program: SchemeExp = {
        val originalProgram = super.program
        val preludedProgram = SchemePrelude.addPrelude(originalProgram)
        CSchemeUndefiner.undefine(List(preludedProgram))
    }

    primitives.allPrimitives.foreach { p =>
        val addr = GlobalAddr(PrmAddr(p.name))
        store += (addr -> lattice.primitive(p))
    }

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