package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.sexp
import scalaam.language.scheme._

trait BigStepSchemeModFSemantics extends SchemeModFSemantics {
  // defining the intra-analysis
  override def intraAnalysis(cmp: IntraComponent) = new IntraAnalysis(cmp)
  class IntraAnalysis(component: IntraComponent) extends super.IntraAnalysis(component) with SchemeModFSemanticsIntra {
    // analysis entry point
    def analyze(ignore: Timeout.T) = writeResult(component match {
      case MainComponent      => eval(program)
      case cmp: CallComponent => evalSequence(cmp.lambda.body)
    })
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
    // simple big-step eval
    private def eval(exp: SchemeExp): Value = exp match {
      case SchemeValue(value, _)                    => evalLiteralValue(value)
      case lambda: SchemeLambda                     => lattice.closure((lambda, component), None)
      case SchemeVarLex(_, lex)                     => lookupVariable(lex)
      case SchemeBegin(exps, _)                     => evalSequence(exps)
      case SchemeDefineVariable(id, vexp, _)        => evalDefineVariable(id, vexp)
      case SchemeDefineFunction(id, prs, bdy, pos)  => evalDefineFunction(id, prs, bdy, pos)
      case SchemeSetLex(_, lex, variable, _)        => evalSet(lex, variable)
      case SchemeIf(prd, csq, alt, _)               => evalIf(prd, csq, alt)
      case SchemeLet(bindings, body, _)             => evalLetExp(bindings, body)
      case SchemeLetStar(bindings, body, _)         => evalLetExp(bindings, body)
      case SchemeLetrec(bindings, body, _)          => evalLetExp(bindings, body)
      case SchemeNamedLet(name,bindings,body,pos)   => evalNamedLet(name,bindings,body,pos)
      case SchemeFuncall(fun, args, _)              => evalCall(fun, args)
      case SchemeAnd(exps, _)                       => evalAnd(exps)
      case SchemeOr(exps, _)                        => evalOr(exps)
      case SchemeQuoted(quo, _)                     => evalQuoted(quo)
      case _ => throw new Exception(s"Unsupported Scheme expression: $exp")
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
    private def lookupVariable(lex: LexicalRef): Value =
      readAddr(resolveAddr(lex))
    private def evalDefineVariable(id: Identifier, exp: SchemeExp): Value = {
      val value = eval(exp)
      writeAddr(VarAddr(id),value)
      value
    }
    private def evalDefineFunction(id: Identifier, prs: List[Identifier], body: List[SchemeExp], pos: Position): Value = {
      val lambda = SchemeLambda(prs,body,pos)
      val value = lattice.closure((lambda,component),Some(id.name))
      writeAddr(VarAddr(id),value)
      value
    }
    private def evalSequence(exps: List[SchemeExp]): Value =
      exps.foldLeft(lattice.bottom)((_,exp) => eval(exp))
    private def evalSet(lex: LexicalRef, exp: SchemeExp): Value = {
      val addr = resolveAddr(lex)
      val newValue = eval(exp)
      writeAddr(addr,newValue)
      newValue
    }
    private def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): Value =
      conditional(eval(prd), eval(csq), eval(alt))
    private def evalLetExp(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp]): Value = {
      evalLetBindings(bindings)
      evalSequence(body)
    }
    private def evalNamedLet(id: Identifier, bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], pos: Position): Value = {
      val (prs,ags) = bindings.unzip
      val lambda = SchemeLambda(prs,body,pos)
      val closure = lattice.closure((lambda,component),Some(id.name))
      writeAddr(VarAddr(id),closure)
      val argVals = ags.map(eval)
      applyClosures(closure,argVals)
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
        case ((lam@SchemeLambda(prs,_,_), cmp), nam) if prs.length == arity =>
          val context = allocCtx((lam,cmp),args)
          val component = CallComponent(lam,cmp,nam,context)
          bindArgs(component, prs, args)
          call(component)
        case _ => lattice.bottom
      }))
    }
    val allocator = new SchemeAllocator[Addr] {
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
    // some helpers
    private def conditional(prd: Value, csq: => Value, alt: => Value): Value = {
      val csqVal = if (lattice.isTrue(prd)) csq else lattice.bottom
      val altVal = if (lattice.isFalse(prd)) alt else lattice.bottom
      lattice.join(csqVal,altVal)
    }
    private def evalLetBindings(bindings: List[(Identifier,SchemeExp)]) =
      bindings.foreach { case (id,exp) => writeAddr(VarAddr(id), eval(exp)) }
    private def bindArgs(component: IntraComponent, pars: List[Identifier], args: List[Value]) =
      pars.zip(args).foreach { case (par,arg) => writeAddr(VarAddr(par),arg,component) }
  }
}

//abstract class AdaptiveSchemeModFAnalysis(program: SchemeExp) extends AdaptiveModAnalysis(program)
//                                                              with AdaptiveSchemeModFSemantics
