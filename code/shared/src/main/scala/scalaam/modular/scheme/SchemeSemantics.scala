package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.CScheme._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.modular._
import scalaam.modular.scheme.PrmAddr

trait SchemeSemantics extends ModAnalysis[SchemeExp]
                         with GlobalStore[SchemeExp]
                         with ReturnValue[SchemeExp] {
  type Env  = Environment[Addr]
  type Prim = SchemePrimitive[Value,Addr]
  val initialEnv: Env
  implicit val lattice: SchemeLattice[Value, Addr, Prim] 
  // The final result can be found at the ReturnAddr of the initialComponent
  private lazy val returnAddr = componentAddr(initialComponent, ReturnAddr(expr(initialComponent)))
  def finalResult = store.getOrElse(returnAddr, lattice.bottom)
}

trait DedicatedSchemeSemantics extends SchemeSemantics
                                  with DedicatedGlobalStore[SchemeExp] {
  // Ensure that the program is translated to use lexical addresses first!
  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    CSchemeUndefiner.undefine(List(preludedProgram))
  }
  lazy val primitives: SchemePrimitives[Value,Addr] = new SchemeLatticePrimitives()
  lazy val initialBds: Iterable[(String,Addr,Value)] = primitives.allPrimitives.map {
    p => (p.name, sharedAddr(PrmAddr(p.name)), lattice.primitive(p)) 
  }
  lazy val initialEnv = Environment(initialBds.map(bnd => (bnd._1, bnd._2)))
  // Set up initial environment and install the primitives in the global store.
  primitives.allPrimitives.foreach { p =>
    val addr = sharedAddr(PrmAddr(p.name))
    store += (addr -> lattice.primitive(p))
  }
}
