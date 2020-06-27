package scalaam.modular.scheme

import scalaam.core._
import scalaam.modular._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.language.CScheme._
/*
trait SchemeModConcSemantics extends DedicatedGlobalStore[SchemeExp]
                                with ReturnValue[SchemeExp] {

    trait Component { def bdy: SchemeExp }
    case object MainThread extends Component { def bdy = program }
    case class Thread(bdy: SchemeExp) extends Component  // TODO: add context

    type Prim = SchemePrimitive[Value,Addr]
    lazy val primitives: SchemePrimitives[Value,Addr] = new SchemeLatticePrimitives()
    implicit val lattice: SchemeLattice[Value, Addr, Prim, Component]

    // add the primitives to the global store
    trait Addr
    case class Addr(cmp: Component)
    case class Addr(cmp: Component, addr: Address) extends Address  { def printable = addr.printable }
    def buildAddr(cmp: Thread, addr: Address) = Addr(cmp,addr)

    primitives.allPrimitives.foreach { p =>
        val addr = Addr(initialComponent, p.name)
        store += (addr -> lattice.primitive(p))
    }

    
    // prelude other primitives and translate the program to use lexical addresses
    override val program: SchemeExp = {
        val originalProgram = super.program
        val preludedProgram = SchemePrelude.addPrelude(originalProgram)
        val initialBindings = Set[String]() //primitives.allPrimitives.map(_.name).toSet
        CSchemeLexicalAddresser.translateProgram(preludedProgram, initialBindings)
    }

}

*/