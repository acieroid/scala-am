package scalaam.modular.scheme

import scalaam.core._
import scalaam.modular._
import scalaam.language.scheme._
import scalaam.util._

trait SchemeModFSemantics extends ModAnalysis[SchemeExp]
                          with GlobalStore[SchemeExp]
                          with ReturnResult[SchemeExp] {
  // ensure that the program is translated with lexical addresses first!
  override lazy val program = {
    val originalProgram = super.program
    val initialBindings = primitives.allPrimitives.map(_.name).toSet
    SchemeLexicalAddresser.translateProgram(originalProgram,initialBindings)
  }
  // local addresses are simply made out of lexical information
  trait LocalAddr extends Address { def pos(): Position }
  case class VarAddr(id: Identifier)  extends LocalAddr { def printable = true;  def pos(): Position = id.pos }
  case class PtrAddr(exp: Expression) extends LocalAddr { def printable = false; def pos(): Position = exp.pos }
  case class PrmAddr(nam: String)     extends LocalAddr { def printable = true;  def pos(): Position = Position.none }
  // abstract values come from a Scala-AM Scheme lattice (a type lattice)
  type Prim = SchemePrimitive[Value, Addr]
  implicit val lattice: SchemeLattice[Value, Addr, Prim, IntraComponent]
  lazy val primitives = new SchemePrimitives[Value, Addr]
  // setup initial environment and install the primitives in the global store
  primitives.allPrimitives.foreach { p =>
    val addr = ComponentAddr(MainComponent,PrmAddr(p.name))
    store += (addr -> lattice.primitive(p))
  }
  // in ModF, components are function calls in some context
  trait IntraComponent
  case object MainComponent extends IntraComponent {
    override def toString = "main"
  }
  case class CallComponent(lambda: SchemeLambda,
                           parent: IntraComponent,
                           nam: Option[String],
                           ctx: Context) extends IntraComponent {
    override def toString = nam match {
      case None => s"anonymous@${lambda.pos} [${ctx.toString}]"
      case Some(name) => s"$name [${ctx.toString}]"
    }
  }
  lazy val initialComponent = MainComponent
  // this abstract class is parameterized by the choice of Context and allocation strategy of Contexts
  type Context
  def allocCtx(clo: lattice.Closure, args: List[Value]): Context
  // extension for the intraAnalysis
  trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {
    // variable lookup
    protected def lookupVariable(lex: LexicalRef): Value =
      readAddr(resolveAddr(lex))
    protected def setVariable(lex: LexicalRef, vlu: Value) =
      writeAddr(resolveAddr(lex),vlu)
    // resolve a lexical address to the corresponding address in the store
    private def resolveAddr(lex: LexicalRef): Addr = lex match {
      case LocalRef(identifier) =>
        ComponentAddr(component,VarAddr(identifier))
      case GlobalRef(identifier) =>
        ComponentAddr(MainComponent,VarAddr(identifier))
      case PrimRef(name) =>
        ComponentAddr(MainComponent,PrmAddr(name))
      case NonLocalRef(identifier,scp) =>
        val cmp = resolveParent(component,scp)
        ComponentAddr(cmp,VarAddr(identifier))
    }
    private def resolveParent(cmp: IntraComponent, scp: Int): IntraComponent =
      if (scp == 0) { cmp } else cmp match {
        case cmp: CallComponent => resolveParent(cmp.parent, scp - 1)
        // If the program has succesfully passed the lexical translation, the lookup should never fail!
        case MainComponent => throw new Exception("This should not happen!")
      }
    // apply
    protected def applyFun(fexp: SchemeExp, fval: Value, args: List[(SchemeExp,Value)]): Value =
      if(args.forall(_._2 != lattice.bottom)) {
        val fromClosures = applyClosures(fval,args.map(_._2))
        val fromPrimitives = applyPrimitives(fexp,fval,args)
        lattice.join(fromClosures,fromPrimitives)
      } else {
        lattice.bottom
      }
    // TODO[minor]: use foldMap instead of foldLeft
    private def applyClosures(fun: Value, args: List[Value]): Value = {
      val arity = args.length
      val closures = lattice.getClosures(fun)
      closures.foldLeft(lattice.bottom)((acc,clo) => lattice.join(acc, clo match {
        case ((lam@SchemeLambda(prs,_,_), cmp), nam) if prs.length == arity =>
          val context = allocCtx((lam,cmp),args)
          val component = CallComponent(lam,cmp,nam,context)
          bindArgs(component, prs, args)
          call(component)
        case _ => lattice.bottom
      }))
    }
    private def bindArgs(component: IntraComponent, pars: List[Identifier], args: List[Value]) =
      pars.zip(args).foreach { case (par,arg) => writeAddr(VarAddr(par),arg,component) }
    private val allocator = new SchemeAllocator[Addr] {
      def pointer(exp: SchemeExp) = allocAddr(PtrAddr(exp))
    }
    // TODO[minor]: use foldMap instead of foldLeft
    private def applyPrimitives(fexp: SchemeExp, fval: Value, args: List[(SchemeExp,Value)]): Value =
      lattice.getPrimitives(fval).foldLeft(lattice.bottom)((acc,prm) => lattice.join(acc,
        prm.call(fexp, args, StoreAdapter, allocator) match {
          case MayFailSuccess((vlu,_))  => vlu
          case MayFailBoth((vlu,_),_)   => vlu
          case MayFailError(_)          => lattice.bottom
        }))
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
    // some helpers
    protected def conditional[M : Monoid](prd: Value, csq: => M, alt: => M): M = {
      val csqVal = if (lattice.isTrue(prd)) csq else Monoid[M].zero
      val altVal = if (lattice.isFalse(prd)) alt else Monoid[M].zero
      Monoid[M].append(csqVal,altVal)
    }
  }
}

trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                  with SchemeModFSemantics
                                  with AdaptiveGlobalStore[SchemeExp]
                                  with AdaptiveReturnResult[SchemeExp] {
  def alpha(cmp: IntraComponent) = cmp // TODO!
}
