package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.scheme.primitives._
import scalaam.language.scheme._
import scalaam.language.sexp
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.modular._
import scalaam.util.Monoid

/**
 * Base definitions for a Scheme ModX analysis.
 */
trait SchemeModXSemantics extends ModAnalysis[SchemeExp]
                             with GlobalStore[SchemeExp]
                             with ReturnValue[SchemeExp]
                             with ContextSensitiveComponents[SchemeExp] {

  //XXXXXXXXXXXXXXXXXXXX//
  // LEXICAL ADDRESSING //
  //XXXXXXXXXXXXXXXXXXXX//

  // Ensure that the program is translated to use lexical addresses first!
  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    val initialBindings = primitives.allPrimitives.map(_.name).toSet
    SchemeLexicalAddresser.translateProgram(preludedProgram, initialBindings)
  }

  // Set up initial environment and install the primitives in the global store.
  primitives.allPrimitives.foreach { p =>
    val addr = ComponentAddr(initialComponent, PrmAddr(p.name))
    store += (addr -> lattice.primitive(p))
  }

  // Local addresses are simply made out of lexical information.
  sealed trait LocalAddr extends Address {
    def idn(): Identity
    override def toString() = this match {
      case VarAddr(id)  => s"var ($id)"
      case PtrAddr(exp) => s"ptr (${exp.idn})"
      case PrmAddr(nam) => s"prm ($nam)"
    }
  }
  case class VarAddr(id: Identifier)  extends LocalAddr { def printable = true;  def idn(): Identity =  id.idn }
  case class PtrAddr(exp: SchemeExp)  extends LocalAddr { def printable = false; def idn(): Identity =  exp.idn }
  case class PrmAddr(nam: String)     extends LocalAddr { def printable = true;  def idn(): Identity = Identity.none }

  //XXXXXXXXXXXXXXXXX//
  // ABSTRACT VALUES //
  //XXXXXXXXXXXXXXXXX//

  // Abstract values come from a Scala-AM Scheme lattice (a type lattice).
  type Prim = SchemePrimitive[Value, Addr]
  implicit val lattice: SchemeLattice[Value, Addr, Prim, Component]
  lazy val primitives: SchemePrimitives[Value, Addr] = new SchemeLatticePrimitives()

  //XXXXXXXXXXXXXXXXXXXXXXXXXX//
  // INTRA-COMPONENT ANALYSIS //
  //XXXXXXXXXXXXXXXXXXXXXXXXXX//

  // Extensions to the intraAnalysis.
  trait SchemeModXSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {

    //====================//
    // EVALUATION HELPERS //
    //====================//

    // primitives glue code
    // TODO[maybe]: while this should be sound, it might be more precise to not immediately write every value update to the global store ...
    case object StoreAdapter extends Store[Addr,Value] {
      def lookup(a: Addr): Option[Value] = Some(readAddr(a))
      def extend(a: Addr, v: Value): Store[Addr, Value] = { writeAddr(a,v) ; this }
      // all the other operations should not be used by the primitives ...
      def content                               = throw new Exception("Operation not allowed!")
      def keys                                  = throw new Exception("Operation not allowed!")
      def restrictTo(a: Set[Addr])              = throw new Exception("Operation not allowed!")
      def forall(p: ((Addr, Value)) => Boolean) = throw new Exception("Operation not allowed!")
      def join(that: Store[Addr, Value])        = throw new Exception("Operation not allowed!")
      def subsumes(that: Store[Addr, Value])    = throw new Exception("Operation not allowed!")
    }

    // Evaluate literals by in injecting them in the lattice.
    protected def evalLiteralValue(literal: sexp.Value): Value = literal match {
      case sexp.ValueInteger(n)   => lattice.number(n)
      case sexp.ValueReal(r)      => lattice.real(r)
      case sexp.ValueBoolean(b)   => lattice.bool(b)
      case sexp.ValueString(s)    => lattice.string(s)
      case sexp.ValueCharacter(c) => lattice.char(c)
      case sexp.ValueSymbol(s)    => lattice.symbol(s)
      case sexp.ValueNil          => lattice.nil
      case _ => throw new Exception(s"Unsupported Scheme literal: $literal")
    }

    // Evaluation of conditionals.
    protected def conditional[M : Monoid](prd: Value, csq: => M, alt: => M): M = {
      val csqVal = if (lattice.isTrue(prd)) csq else Monoid[M].zero
      val altVal = if (lattice.isFalse(prd)) alt else Monoid[M].zero
      Monoid[M].append(csqVal,altVal)
    }
  }
}
