package scalaam.modular.scheme.modf

import scalaam.core.Position._
import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.language.sexp
import scalaam.modular._
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.modular.scheme._
import scalaam.util._

/**
 * Base definitions for a Scheme MODF analysis.
 */
// TODO: Most of this can be factored out to SchemeSemantics
trait BaseSchemeModFSemantics extends ModAnalysis[SchemeExp]
                                 with GlobalStore[SchemeExp]
                                 with ReturnValue[SchemeExp]
                                 with SchemeDomain
                                 with ContextSensitiveComponents[SchemeExp] {

  // the environment in which the ModF analysis is executed
  type Env = Environment[Addr]     
  def baseEnv: Env

  // In ModF, components are function calls in some context.
  // All components used together with this Scheme MODF analysis should be viewable as SchemeComponents.

  def view(cmp: Component): SchemeModFComponent

  def expr(cmp: Component): SchemeExp = body(cmp)
  def body(cmp: Component): SchemeExp = body(view(cmp))
  def body(cmp: SchemeModFComponent): SchemeExp = cmp match {
    case Main                       => program 
    case c: Call[ComponentContext]  => SchemeBody(c.lambda.body)
  }

  type ComponentContent = Option[lattice.Closure]
  def content(cmp: Component) = view(cmp) match {
    case Main                       => None
    case c: Call[ComponentContext]  => Some(c.clo)
  }
  def context(cmp: Component): Option[ComponentContext] = view(cmp) match {
    case Main                       => None
    case c: Call[ComponentContext]  => Some(c.ctx)
  }

  /** Creates a new component, given a closure, context and an optional name. */
  def newComponent(call: Call[ComponentContext]): Component

  /** Creates a new context given a closure, a list of argument values and the position of the call site. */
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext

  /** Allocating addresses */
  type AllocationContext
  def allocVar(id: Identifier, cmp: Component): VarAddr[AllocationContext]
  def allocPtr(exp: SchemeExp, cmp: Component): PtrAddr[AllocationContext]

  //XXXXXXXXXXXXXXXXXXXXXXXXXX//
  // INTRA-COMPONENT ANALYSIS //
  //XXXXXXXXXXXXXXXXXXXXXXXXXX//

  // Extensions to the intraAnalysis.
  override def intraAnalysis(cmp: Component): SchemeModFSemanticsIntra
  trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {
    // components
    protected def fnBody: SchemeExp = body(view(component))
    protected def fnEnv: Env = view(component) match {
      case Main                           => baseEnv
      case c: Call[ComponentContext]      => 
        val extEnv = c.env.extend(c.lambda.args.map { id =>
          (id.name, allocVar(id, component))
        })
        c.lambda.varArgId match {
          case None         => extEnv
          case Some(varArg) => extEnv.extend(varArg.name, allocVar(varArg, component)) 
        }
    }
    // variable lookup: use the global store
    protected def lookup(id: Identifier, env: Env): Value = env.lookup(id.name) match {
      case None       => throw new Exception(s"Undefined variable $id") //TODO: better error reporting
      case Some(addr) => readAddr(addr)
    }
    protected def assign(id: Identifier, env: Env, vlu: Value): Unit = env.lookup(id.name) match {
      case None       => throw new Exception(s"Undefined variable $id") //TODO: better error reporting
      case Some(addr) => writeAddr(addr, vlu)   
    }
    protected def assign(bds: List[(Identifier,Value)], env: Env): Unit = 
      bds.foreach { case (id,vlu) => assign(id,env,vlu) }
    protected def bind(id: Identifier, env: Env, vlu: Value): Env = {
      val addr = allocVar(id, component)
      val env2 = env.extend(id.name,addr)
      writeAddr(addr,vlu)
      env2
    }
    protected def bind(bds: List[(Identifier,Value)], env: Env): Env =
      bds.foldLeft(env)((env2, bnd) => bind(bnd._1, env2, bnd._2))
    protected def applyFun(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)], cll: Position): Value =
      if(args.forall(_._2 != lattice.bottom)) {
        val fromClosures = applyClosures(fval,args,cll)
        val fromPrimitives = applyPrimitives(fexp,fval,args)
        lattice.join(fromClosures,fromPrimitives)
      } else {
        lattice.bottom
      }
    // TODO[minor]: use foldMap instead of foldLeft
    protected def applyClosures(fun: Value, args: List[(SchemeExp,Value)], cll: Position): Value = {
      val arity = args.length
      val closures = lattice.getClosures(fun)
      closures.foldLeft(lattice.bottom)((acc,clo) => lattice.join(acc, clo match {
        case (clo@(SchemeLambda(prs,_,_),_), nam) if prs.length == arity =>
          val argVals = args.map(_._2)
          val context = allocCtx(nam, clo, argVals, cll, component)
          val targetCall = Call(clo,nam,context)
          val targetCmp = newComponent(targetCall)
          bindArgs(targetCmp, prs, argVals)
          call(targetCmp)
        case (clo@(SchemeVarArgLambda(prs,vararg,_,_),_), nam) if prs.length <= arity =>
          val (fixedArgs,varArgs) = args.splitAt(prs.length)
          val fixedArgVals = fixedArgs.map(_._2)
          val varArgVal = allocateList(varArgs)
          val context = allocCtx(nam, clo, fixedArgVals :+ varArgVal, cll, component)
          val targetCall = Call(clo,nam,context)
          val targetCmp = newComponent(targetCall)
          bindArgs(targetCmp,prs,fixedArgVals)
          bindArg(targetCmp,vararg,varArgVal)
          call(targetCmp)
        case _ => lattice.bottom
      }))
    }
    protected def allocateList(elms: List[(SchemeExp,Value)]): Value = elms match {
      case Nil                => lattice.nil
      case (exp,vlu) :: rest  => allocateCons(exp)(vlu,allocateList(rest))
    }
    protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): Value = {
      val addr = allocPtr(pairExp, component)
      val pair = lattice.cons(car,cdr)
      writeAddr(addr,pair)
      lattice.pointer(addr)
    }
    protected def append(appendExp: SchemeExp)(l1: (SchemeExp, Value), l2: (SchemeExp, Value)): Value = {
      //TODO [difficult]: implement append
      throw new Exception("NYI -- append")
    }
    private def bindArg(component: Component, par: Identifier, arg: Value): Unit =
      writeAddr(allocVar(par, component), arg)
    private def bindArgs(component: Component, pars: List[Identifier], args: List[Value]): Unit =
      pars.zip(args).foreach { case (par,arg) => bindArg(component,par,arg) }

    protected val interpreterBridge: SchemeInterpreterBridge[Addr] = new SchemeInterpreterBridge[Addr] {
      def pointer(exp: SchemeExp): Addr = allocPtr(exp, component)
      def currentThread = throw new Exception("Concurrency not available in ModF")
    }
    // TODO[minor]: use foldMap instead of foldLeft
    protected def applyPrimitives(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)]): Value =
      lattice.getPrimitives(fval).foldLeft(lattice.bottom)((acc,prm) => lattice.join(acc,
        prm.call(fexp, args, StoreAdapter, interpreterBridge) match {
          case MayFailSuccess((vlu,_))  => vlu
          case MayFailBoth((vlu,_),_)   => vlu
          case MayFailError(_)          => lattice.bottom
        }
      ))
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
    // The current component serves as the lexical environment of the closure.
    protected def newClosure(lambda: SchemeLambdaExp, env: Env, name: Option[String]): Value =
      lattice.closure((lambda, env.restrictTo(lambda.fv)), name)

    // other helpers
    protected def conditional[M : Monoid](prd: Value, csq: => M, alt: => M): M = {
      val csqVal = if (lattice.isTrue(prd)) csq else Monoid[M].zero
      val altVal = if (lattice.isFalse(prd)) alt else Monoid[M].zero
      Monoid[M].append(csqVal,altVal)
    }
  }
}

trait SchemeModFSemantics extends BaseSchemeModFSemantics
                             with StandardSchemeModFAllocator
                             with SchemeSetup {
  lazy val baseEnv = initialEnv
}

// for convenience, since most Scheme analyses don't need this much parameterization
abstract class SimpleSchemeModFAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) 
                                                            with StandardSchemeModFComponents
                                                            with SchemeModFSemantics
                                                            with BigStepModFSemantics {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
}