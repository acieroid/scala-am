package scalaam.test.lattice.scheme

import scalaam.core._
import scalaam.test.lattice._
import org.scalacheck.{Prop, Arbitrary}
import Prop.{forAll, propBoolean}

import scalaam.language.scheme._

// inherits the standard lattice tests from `LatticeTest`
class SchemeLatticeTests[L](gen: SchemeLatticeGenerator[L])(implicit val schemeLattice: SchemeLattice[L,_,_,_]) extends LatticeTest(gen) {
    // because of higher entropy in Scheme lattice values, verify each property with more examples!
    implicit override val generatorDrivenConfig = 
        PropertyCheckConfiguration(minSuccessful = 1000)
    val schemeLaws = newProperties("Scheme") { p =>
        implicit val arb = gen.anyArb
        implicit val shr = gen.shrink
        implicit val auo = gen.arbUnop
        implicit val abo = gen.arbBinop
        import schemeLattice._
        /* */
        def convert(mf: MayFail[L,Error]): L = mf match {
            case MayFailSuccess(v) => v
            case MayFailBoth(v,_) => v
            case MayFailError(_) => bottom
        }
        /* Unary operators preserve bottom */
        p.property("∀ unop: unop(⊥) = ⊥") = forAll((unop: SchemeOps.UnaryOperator) =>
            convert(unaryOp(unop)(bottom)) == bottom
        )
        /* Unary operators are monotone */
        /* TODO: fails because of NaN
        p.property("∀ unop, a, b: a ⊑ b ⇒ unop(a) ⊑ unop(b)") = forAll { (unop: SchemeOps.UnaryOperator, b: L) =>
            forAll(gen.le(b)) { (a: L) =>
                val fa = convert(unaryOp(unop)(a)) 
                val fb = convert(unaryOp(unop)(b)) 
                println(s"f = $unop")
                println(s"f($a) = $fa") 
                println(s"f($b) = $fb")
                subsumes(b, a) ==> subsumes(fb, fa)
            }
        }
        */
        /* Binary operators preverse bottom */
        p.property("∀ binop, a: binop(⊥,a) = ⊥ = binop(a,⊥)") = forAll((binop: SchemeOps.BinaryOperator, a: L) =>
            convert(binaryOp(binop)(bottom,a)) == bottom &&
            convert(binaryOp(binop)(a,bottom)) == bottom
        )
        // return the properties
        p
    }
    checkAll(schemeLaws)
}

class ConcreteSchemeLatticeTests extends SchemeLatticeTests(ConcreteModularSchemeLattice.SchemeValueLatticeGenerator)
class ConstantSchemeLatticeTests extends SchemeLatticeTests(ConstantModularSchemeLattice.SchemeValueLatticeGenerator) 
class TypeSchemeLatticeTests extends SchemeLatticeTests(TypeModularSchemeLattice.SchemeValueLatticeGenerator) 