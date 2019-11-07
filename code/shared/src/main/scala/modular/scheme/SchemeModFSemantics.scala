package scalaam.modular.scheme

import scalaam.core._
import scalaam.modular._
import scalaam.language.scheme._

trait SchemeModFSemantics extends ModAnalysis[SchemeExp]
                          with GlobalStore[SchemeExp]
                          with ReturnResult[SchemeExp] {
  // local addresses are simply made out of lexical information
  trait LocalAddr extends Address { def pos(): Position }
  case class VarAddr(offset: Int)     extends LocalAddr { def printable = true;  def pos(): Position = ??? }
  case class PtrAddr(exp: Expression) extends LocalAddr { def printable = false; def pos(): Position = exp.pos }
  // abstract values come from a Scala-AM Scheme lattice (a type lattice)
  type Prim = SchemePrimitive[Value, Addr]
  implicit val lattice: SchemeLattice[Value, Addr, Prim, IntraComponent]
  lazy val primitives = new SchemePrimitives[Value, Addr]
  // setup initial environment and install the primitives in the global store
  private def initStore() = {
    var currentOfs = 0
    primitives.allPrimitives.foreach { p =>
      val addr = ComponentAddr(MainComponent,VarAddr(currentOfs))
      store += (addr -> lattice.primitive(p))
      currentOfs += 1
    }
  }
  initStore()
  // in ModF, components are function calls in some context
  trait IntraComponent
  case object MainComponent extends IntraComponent {
    override def toString = "main"
  }
  case class CallComponent(lambda: SchemeLambdaLex,
                           parent: IntraComponent,
                           nam: Option[String],
                           ctx: Context) extends IntraComponent {
    override def toString = nam match {
      case None => s"anonymous@${lambda.pos} [${ctx.toString()}]"
      case Some(name) => s"$name [${ctx.toString()}]"
    }
  }
  lazy val initialComponent = MainComponent
  // this abstract class is parameterized by the choice of Context and allocation strategy of Contexts
  type Context
  def allocCtx(clo: lattice.Closure, args: List[Value]): Context
  // extension for the intraAnalysis
  trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {
    // primitives glue code
    // TODO[maybe]: while this should be sound, it might be more precise to not immediately write every value update to the global store ...
    case object StoreAdapter extends Store[Addr,Value] {
      def lookup(a: Addr)                       = Some(readAddr(a))
      def extend(a: Addr, v: Value)             = { writeAddr(a,v) ; this }
      // all the other operations should not be used by the primitives ...
      def content                               = throw new Exception("Operation not allowed!")
      def keys                                  = throw new Exception("Operation not allowed!")
      def restrictTo(a: Set[Addr])              = throw new Exception("Operation not allowed!")
      def forall(p: ((Addr, Value)) => Boolean) = throw new Exception("Operation not allowed!")
      def join(that: Store[Addr, Value])        = throw new Exception("Operation not allowed!")
      def subsumes(that: Store[Addr, Value])    = throw new Exception("Operation not allowed!")
    }
  }
}

trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                  with SchemeModFSemantics
                                  with AdaptiveGlobalStore[SchemeExp]
                                  with AdaptiveReturnResult[SchemeExp] {
  def alpha(cmp: IntraComponent) = ???
}
