package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.sexp
import scalaam.language.scheme._

import scala.concurrent.TimeoutException

trait BigStepSchemeModFSemantics extends SchemeModFSemantics {
  // defining the intra-analysis
  override def intraAnalysis(cmp: IntraComponent) = new IntraAnalysis(cmp)
  class IntraAnalysis(component: IntraComponent) extends super.IntraAnalysis(component) with SchemeModFSemanticsIntra {
    def registerDependency(dep: Dependency): Unit = addDep(component, dep)
    var timeout: Timeout.T = _ // Avoid having to add the timeout to every call.
    // analysis entry point
    def analyze(to: Timeout.T): Unit = {
      timeout = to
      writeResult(component match {
        case MainComponent =>
          eval(program)
        case CallComponent(SchemeLambda(pars,body,_),_,_,_) =>
          bindPars(pars)
          evalSequence(body)
      })
    }
    // simple big-step eval
    private var env = component.env
    private def eval(exp: SchemeExp): Value = {
      if (timeout.reached) throw new TimeoutException()
      exp match {
        case SchemeValue(value, _) => evalLiteralValue(value)
        case lambda: SchemeLambda => lattice.closure((lambda, env), None)
        case SchemeVar(id) => lookupVariable(id.name)
        case SchemeBegin(exps, _) => evalSequence(exps)
        case SchemeDefineVariable(id, vexp, _) => evalDefineVariable(id, vexp)
        case SchemeDefineFunction(id, args, body, pos) => evalDefineFunction(id, args, body, pos)
        case SchemeSet(name, variable, _) => evalSet(name, variable)
        case SchemeIf(prd, csq, alt, _) => evalIf(prd, csq, alt)
        case SchemeLet(bindings, body, _) => evalLet(bindings, body)
        case SchemeLetStar(bindings, body, _) => evalLetStar(bindings, body)
        case SchemeLetrec(bindings, body, _) => evalLetrec(bindings, body)
        case SchemeNamedLet(name, bindings, body, pos) => evalNamedLet(name, bindings, body, pos)
        case SchemeFuncall(fun, args, _) => evalCall(fun, args)
        case SchemeAnd(exps, _) => evalAnd(exps)
        case SchemeOr(exps, _) => evalOr(exps)
        case SchemeQuoted(quo, _) => evalQuoted(quo)
        case _ => throw new Exception(s"Unsupported Scheme expression: $exp")
      }
    }
    private def evalLiteralValue(literal: sexp.Value): Value = literal match {
      case sexp.ValueInteger(n)   => lattice.number(n)
      case sexp.ValueReal(r)      => lattice.real(r)
      case sexp.ValueBoolean(b)   => lattice.bool(b)
      case sexp.ValueString(s)    => lattice.string(s)
      case sexp.ValueCharacter(c) => lattice.char(c)
      case sexp.ValueSymbol(s)    => lattice.symbol(s)
      case sexp.ValueNil          => lattice.nil
      case _ => throw new Exception(s"Unsupported Scheme literal: $literal")
    }
    private def evalQuoted(quoted: sexp.SExp): Value = quoted match {
      case sexp.SExpId(id)          => lattice.symbol(id.name)
      case sexp.SExpValue(vlu,_)    => evalLiteralValue(vlu)
      case sexp.SExpPair(car,cdr,_) =>
        val carv = evalQuoted(car)
        val cdrv = evalQuoted(cdr)
        val pair = lattice.cons(carv,cdrv)
        val addr = allocAddr(PtrAddr(quoted))
        writeAddr(addr,pair)
        lattice.pointer(addr)
      case sexp.SExpQuoted(q,pos)   =>
        evalQuoted(sexp.SExpPair(sexp.SExpId(Identifier("quote",pos)),sexp.SExpPair(q,sexp.SExpValue(sexp.ValueNil,pos),pos),pos))
    }
    private def lookupVariable(name: String): Value = env.lookup(name) match {
      case Some(addr) => readAddr(addr)
      case None => lattice.bottom
    }
    private def evalDefineVariable(id: Identifier, exp: SchemeExp): Value = {
      val addr = bind(id,lattice.bottom)
      val value = eval(exp)
      writeAddr(addr,value)
      value
    }
    private def evalDefineFunction(id: Identifier, pars: List[Identifier], body: List[SchemeExp], pos: Position): Value = {
      val addr = bind(id,lattice.bottom)
      val value = lattice.closure((SchemeLambda(pars,body,pos),env),Some(id.name))
      writeAddr(addr,value)
      value
    }
    private def evalSequence(exps: List[SchemeExp]): Value =
      exps.foldLeft(lattice.bottom)((_,exp) => eval(exp))
    private def evalSet(id: Identifier, exp: SchemeExp): Value = env.lookup(id.name) match {
      case Some(addr) =>
        val newValue = eval(exp)
        writeAddr(addr,newValue)
        newValue
      case None => lattice.bottom
    }
    private def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): Value = {
      val prdVal = eval(prd)
      thunkify { conditional(prdVal,eval(csq),eval(alt)) }
    }
    private def evalLet(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp]): Value = {
      val vals = bindings.map(bnd => eval(bnd._2))
      thunkify {
        val vars = bindings.map(_._1)
        vars.zip(vals).foreach { case (vrb,vlu) => bind(vrb,vlu) }
        evalSequence(body)
      }
    }
    private def evalLetStar(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp]): Value = thunkify {
      bindings.foreach { case (id,exp) =>
        val vlu = eval(exp)
        bind(id,vlu)
      }
      evalSequence(body)
    }
    private def evalLetrec(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp]): Value = thunkify {
      val addrs = bindings.map(bnd => bind(bnd._1, lattice.bottom))
      val vals = bindings.map(bnd => eval(bnd._2))
      addrs.zip(vals).foreach( { case (addr,vlu) => writeAddr(addr,vlu) })
      evalSequence(body)
    }
    private def evalNamedLet(id: Identifier, bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], pos: Position): Value =
      thunkify {
        val addr = bind(id,lattice.bottom)
        val closure = lattice.closure((SchemeLambda(bindings.map(_._1),body,pos),env),Some(id.name))
        writeAddr(addr,closure)
        val args = bindings.map(bnd => eval(bnd._2))
        applyClosures(closure,args)
      }
    // R5RS specification: if all exps are 'thruty', then the value is that of the last expression
    private def evalAnd(exps: List[SchemeExp]): Value =
      if (exps.isEmpty) { lattice.bool(true) } else { evalAndLoop(exps) }
    private def evalAndLoop(exps: List[SchemeExp]): Value = (exps: @unchecked) match {
      case exp :: Nil => eval(exp)
      case exp :: rst => conditional(eval(exp),evalAndLoop(rst),lattice.bool(false))
    }
    private def evalOr(exps: List[SchemeExp]): Value = exps.foldRight(lattice.bool(false)) { (exp,acc) =>
      val vlu = eval(exp)
      conditional(vlu,vlu,acc)
    }
    private def evalCall(fun: SchemeExp, args: List[SchemeExp]): Value = {
      val funVal = eval(fun)
      val argVals = args.map(eval)
      applyFun(fun,funVal,args.zip(argVals))
    }
    // apply
    private def applyFun(fexp: SchemeExp, fval: Value, args: List[(SchemeExp,Value)]): Value =
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
        case ((lambda@SchemeLambda(prs,_,_),env),nam) if prs.length == arity =>
          val context = allocCtx(lambda,env,args)
          val component = CallComponent(lambda,env,nam,context)
          bindArgs(component,prs,args)
          call(component)
        case _ => lattice.bottom
      }))
    }
    // TODO[minor]: use foldMap instead of foldLeft
    private def applyPrimitives(fexp: SchemeExp, fval: Value, args: List[(SchemeExp,Value)]): Value = {
      val primitives = lattice.getPrimitives[schemeSemantics.Primitive](fval)
      primitives.foldLeft(lattice.bottom)((acc,prm) => lattice.join(acc,
        prm.call(fexp, args, StoreAdapter, this) match {
          case MayFailSuccess((vlu,_))  => vlu
          case MayFailBoth((vlu,_),_)   => vlu
          case MayFailError(_)          => lattice.bottom
        }))
    }
    // some helpers
    private def thunkify(f: => Value): Value = {
      val savedEnv = env
      val result = f
      env = savedEnv
      result
    }
    private def conditional(prd: Value, csq: => Value, alt: => Value): Value = {
      val csqVal = if (lattice.isTrue(prd)) csq else lattice.bottom
      val altVal = if (lattice.isFalse(prd)) alt else lattice.bottom
      lattice.join(csqVal,altVal)
    }
    private def bind(vrb: Identifier, vlu: Value): Addr  = {
      val addr = allocAddr(VarAddr(vrb))
      env = env.extend(vrb.name,addr)
      writeAddr(addr,vlu)
      addr
    }
    private def bindPars(pars: List[Identifier]): Unit = pars.foreach { par =>
      val addr = allocAddr(VarAddr(par))
      env = env.extend(par.name,addr)
    }
    private def bindArgs(component: IntraComponent, pars: List[Identifier], args: List[Value]): Unit = pars.zip(args).foreach { case (par,arg) =>
      val localAddr = VarAddr(par)
      writeAddr(localAddr,arg,component)
    }
  }
}

//abstract class AdaptiveSchemeModFAnalysis(program: SchemeExp) extends AdaptiveModAnalysis(program)
//                                                              with AdaptiveSchemeModFSemantics
