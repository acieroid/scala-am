package scalaam.modular.scheme

import scalaam.core._
import scalaam.modular._
import scalaam.language.scheme._
import scalaam.language.sexp
import scalaam.util._

trait SchemeModFSemantics extends ModAnalysis[SchemeExp]
                          with GlobalStore[SchemeExp]
                          with ReturnResult[SchemeExp] {
  // ensure that the program is translated with lexical addresses first!
  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val initialBindings = primitives.allPrimitives.map(_.name).toSet
    SchemeLexicalAddresser.translateProgram(originalProgram,initialBindings)
  }
  // local addresses are simply made out of lexical information
  trait LocalAddr extends Address { def pos(): Position }
  case class VarAddr(id: Identifier)            extends LocalAddr { def printable = true;  def pos(): Position = id.pos }
  case class PtrAddr[C](exp: Expression, c: C)  extends LocalAddr { def printable = false; def pos(): Position = exp.pos }
  case class PrmAddr(nam: String)               extends LocalAddr { def printable = true;  def pos(): Position = Position.none }
  // abstract values come from a Scala-AM Scheme lattice (a type lattice)
  type Prim = SchemePrimitive[Value, Addr]
  implicit val lattice: SchemeLattice[Value, Addr, Prim, Component]
  lazy val primitives = new SchemePrimitives[Value, Addr]
  // setup initial environment and install the primitives in the global store
  primitives.allPrimitives.foreach { p =>
    val addr = ComponentAddr(MainComponent,PrmAddr(p.name))
    store += (addr -> lattice.primitive(p))
  }
  // in ModF, components are function calls in some context
  trait Component
  case object MainComponent extends Component {
    override def toString: String = "main"
  }
  case class CallComponent(clo: lattice.Closure,
                           nam: Option[String],
                           ctx: Context) extends Component {
    val (lambda, parent) = clo
    override def toString: String = nam match {
      case None => s"anonymous@${lambda.pos} [${ctx.toString}]"
      case Some(name) => s"$name [${ctx.toString}]"
    }
  }
  lazy val initialComponent: Component = MainComponent
  // this abstract class is parameterized by the choice of Context and allocation strategy of Contexts
  type Context
  def allocCtx(clo: lattice.Closure, args: List[Value]): Context
  // extension for the intraAnalysis
  trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {
    // variable lookup
    protected def lookupVariable(lex: LexicalRef): Value =
      readAddr(resolveAddr(lex))
    protected def setVariable(lex: LexicalRef, vlu: Value): Unit =
      writeAddr(resolveAddr(lex),vlu)
    protected def defineVariable(id: Identifier, vlu: Value): Unit =
      writeAddr(VarAddr(id),vlu)
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
    @scala.annotation.tailrec
    private def resolveParent(cmp: Component, scp: Int): Component =
      if (scp == 0) { cmp } else cmp match {
        case cmp: CallComponent => resolveParent(cmp.parent, scp - 1)
        // If the program has succesfully passed the lexical translation, the lookup should never fail!
        case MainComponent => throw new Exception("This should not happen!")
      }
    // apply
    protected def applyFun(fexp: SchemeExp, fval: Value, args: List[(SchemeExp,Value)]): Value =
      if(args.forall(_._2 != lattice.bottom)) {
        val fromClosures = applyClosures(fval,args)
        val fromPrimitives = applyPrimitives(fexp,fval,args)
        lattice.join(fromClosures,fromPrimitives)
      } else {
        lattice.bottom
      }
    // TODO[minor]: use foldMap instead of foldLeft
    private def applyClosures(fun: Value, args: List[(SchemeExp,Value)]): Value = {
      val arity = args.length
      val closures = lattice.getClosures(fun)
      closures.foldLeft(lattice.bottom)((acc,clo) => lattice.join(acc, clo match {
        case (clo@(SchemeLambda(prs,_,_),_), nam) if prs.length == arity =>
          val argVals = args.map(_._2)
          val context = allocCtx(clo,argVals)
          val component = CallComponent(clo,nam,context)
          bindArgs(component, prs, argVals)
          call(component)
        case (clo@(SchemeVarArgLambda(prs,vararg,_,_),_), nam) if prs.length < arity =>
          val (fixedArgs,varArgs) = args.splitAt(prs.length)
          val fixedArgVals = fixedArgs.map(_._2)
          val varArgVal = allocateList(varArgs)
          val context = allocCtx(clo, fixedArgVals :+ varArgVal)
          val component = CallComponent(clo,nam,context)
          bindArgs(component,prs,fixedArgVals)
          bindArg(component,vararg,varArgVal)
          call(component)
        case _ => lattice.bottom
      }))
    }
    protected def allocateList(elms: List[(SchemeExp,Value)]): Value = elms match {
      case Nil                => lattice.nil
      case (exp,vlu) :: rest  => allocateCons(exp)(vlu,allocateList(rest))
    }
    protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): Value = {
      val pair = lattice.cons(car,cdr)
      val addr = allocAddr(PtrAddr(pairExp,()))
      writeAddr(addr,pair)
      lattice.pointer(addr)
    }
    protected def append(appendExp: SchemeExp)(l1: (SchemeExp, Value), l2: (SchemeExp, Value)): Value = {
      val appendPrim = lattice.primitive(primitives.PrimitiveDefs.Append)
      applyFun(appendExp, appendPrim, List(l1,l2))
    }
    private def bindArg(component: Component, par: Identifier, arg: Value): Unit =
      writeAddr(VarAddr(par),arg,component)
    private def bindArgs(component: Component, pars: List[Identifier], args: List[Value]): Unit =
      pars.zip(args).foreach { case (par,arg) => bindArg(component,par,arg) }

    private val allocator: SchemeAllocator[Addr] = new SchemeAllocator[Addr] {
      def pointer[C](exp: SchemeExp, c: C): Addr = allocAddr(PtrAddr(exp,c))
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
    // evaluation helpers
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
    protected def makeClosure(lambda: SchemeLambdaExp, name: Option[String]): Value =
      // the current component serves as the lexical environment of the closure
      lattice.closure((lambda, component), name)
    // other helpers
    protected def conditional[M : Monoid](prd: Value, csq: => M, alt: => M): M = {
      val csqVal = if (lattice.isTrue(prd)) csq else Monoid[M].zero
      val altVal = if (lattice.isFalse(prd)) alt else Monoid[M].zero
      Monoid[M].append(csqVal,altVal)
    }
  }
}

trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                     with AdaptiveGlobalStore[SchemeExp]
                                     with AdaptiveReturnResult[SchemeExp]
                                     with SchemeModFSemantics
