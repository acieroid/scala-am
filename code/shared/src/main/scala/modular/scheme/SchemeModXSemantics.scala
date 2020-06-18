package scalaam.modular.scheme

import scalaam.core.Position.Position
import scalaam.core._
import scalaam.language.scheme.primitives._
import scalaam.language.scheme._
import scalaam.language.sexp
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.modular._
import scalaam.util.{Monoid, SmartHash}

/**
 * Base definitions for a Scheme ModX analysis.
 */
trait SchemeModXSemantics extends ModAnalysis[SchemeExp]
                             with GlobalStore[SchemeExp]
                             with ReturnValue[SchemeExp]
                             with ContextSensitiveComponents[SchemeExp] {

  //XXXXXXXXXXXXXXXXXXXXXXXXX//
  // COMPONENTS AND CONTEXTS //
  //XXXXXXXXXXXXXXXXXXXXXXXXX//

  implicit def view(c: Component): SchemeComponent
  trait SchemeComponent extends SmartHash { def body: SchemeExp }

  //XXXXXXXXXXXXXXXXXXXX//
  // LEXICAL ADDRESSING //
  //XXXXXXXXXXXXXXXXXXXX//

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
  type Env
  implicit val lattice: SchemeLattice[Value, Addr, Prim, Env]
  lazy val primitives: SchemePrimitives[Value, Addr] = new SchemeLatticePrimitives()

  //XXXXXXXXXXXXXXXXXXXXXXXXXX//
  // INTRA-COMPONENT ANALYSIS //
  //XXXXXXXXXXXXXXXXXXXXXXXXXX//

  // Extensions to the intraAnalysis.
  trait SchemeModXSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {

    //====================//
    // EVALUATION HELPERS //
    //====================//

    protected def applyClosures(fun: Value, args: List[(SchemeExp,Value)], cll: Position, cmp: Component): Value

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

    // Append two lists (used for quasiquoting).
    protected def append(appendExp: SchemeExp)(l1: (SchemeExp, Value), l2: (SchemeExp, Value)): Value = {
      //TODO [difficult]: implement append
      throw new NotImplementedError("Not yet implemented: append.")
    }

    // TODO[minor]: use foldMap instead of foldLeft
    protected def applyPrimitives(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)]): Value =
      lattice.getPrimitives(fval).foldLeft(lattice.bottom)((acc,prm) => lattice.join(acc,
        prm.call(fexp, args, StoreAdapter, allocator) match {
          case MayFailSuccess((vlu,_))  => vlu
          case MayFailBoth((vlu,_),_)   => vlu
          case MayFailError(_)          => lattice.bottom
        }
      ))

    protected def applyFun(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)], cll: Position, cmp: Component): Value =
      if(args.forall(_._2 != lattice.bottom)) {
        val fromClosures = applyClosures(fval,args, cll, cmp)
        val fromPrimitives = applyPrimitives(fexp,fval,args)
        lattice.join(fromClosures,fromPrimitives)
      } else {
        lattice.bottom
      }

    protected def splitArgs(args: List[(SchemeExp,Value)])(fn: List[(SchemeExp,Value)] => Value): Value = args match {
      case Nil                      => fn(Nil)
      // TODO[minor]: use foldMap instead of foldLeft
      case (argExp,argVal) :: rest  =>
        lattice.split(argVal).foldLeft(lattice.bottom) { (acc,argSplitted) => lattice.join(acc,
          splitArgs(rest)(restSplitted => fn((argExp,argSplitted) :: restSplitted))
        )}
    }

    //====================//
    // ALLOCATION HELPERS //
    //====================//

    protected def allocateList(elms: List[(SchemeExp,Value)]): Value = elms match {
      case Nil                => lattice.nil
      case (exp,vlu) :: rest  => allocateCons(exp)(vlu,allocateList(rest))
    }
    protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): Value = {
      val addr = allocAddr(PtrAddr(pairExp))
      val pair = lattice.cons(car,cdr)
      writeAddr(addr,pair)
      lattice.pointer(addr)
    }

    val allocator: SchemeAllocator[Addr] = new SchemeAllocator[Addr] {
      def pointer(exp: SchemeExp): Addr = allocAddr(PtrAddr(exp))
    }
  }
}
