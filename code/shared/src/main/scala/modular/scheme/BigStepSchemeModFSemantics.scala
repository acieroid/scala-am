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
    private def resolveAddr(lex: LexicalAddr): Addr = lex match {
      case LocalVar(ofs) =>
        ComponentAddr(component,VarAddr(ofs))
      case GlobalVar(ofs) =>
        ComponentAddr(MainComponent,VarAddr(ofs))
      case NonLocalVar(scp,ofs) =>
        val cmp = resolveParent(component,scp)
        ComponentAddr(cmp,VarAddr(ofs))
    }
    private def resolveParent(cmp: IntraComponent, scp: Int): IntraComponent =
      if (scp == 0) { cmp } else cmp match {
        case cmp: CallComponent => resolveParent(cmp.parent, scp - 1)
        // If the program has succesfully passed the lexical translation, the lookup should never fail!
        case MainComponent => throw new Exception("This should not happen!")
      }
    // simple big-step eval
    private def eval(exp: SchemeExp): Value = exp match {
      case SchemeValue(value, _)                      => evalLiteralValue(value)
      case lambda: SchemeLambdaLex                    => lattice.closure((lambda, component), None)
      case SchemeVarLex(_,lex)                        => lookupVariable(lex)
      case SchemeBegin(exps, _)                       => evalSequence(exps)
      case SchemeDefineVariableLex(_, ofs, vexp, _)   => evalDefineVariable(ofs, vexp)
      case defExp: SchemeDefineFunctionLex            => evalDefineFunction(defExp)
      case SchemeSetLex(_, lex, variable, _)          => evalSet(lex, variable)
      case SchemeIf(prd, csq, alt, _)                 => evalIf(prd, csq, alt)
      case SchemeLetLex(_, bindings, body, _, _)      => evalLetExp(bindings, body)
      case SchemeLetStarLex(_, bindings, body, _, _)  => evalLetExp(bindings, body)
      case SchemeLetrecLex(_, bindings, body, _, _)   => evalLetExp(bindings, body)
      case namedLet: SchemeNamedLetLex                => evalNamedLet(namedLet)
      case SchemeFuncall(fun, args, _)                => evalCall(fun, args)
      case SchemeAnd(exps, _)                         => evalAnd(exps)
      case SchemeOr(exps, _)                          => evalOr(exps)
      case SchemeQuoted(quo, _)                       => evalQuoted(quo)
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
    private def lookupVariable(lex: LexicalAddr): Value =
      readAddr(resolveAddr(lex))
    private def evalDefineVariable(ofs: Int, exp: SchemeExp): Value = {
      val value = eval(exp)
      writeAddr(VarAddr(ofs),value)
      value
    }
    private def evalDefineFunction(defExp: SchemeDefineFunctionLex): Value = {
      val SchemeDefineFunctionLex(id,ofs,pars,body,frmSiz,frmOfs,pos) = defExp
      val lambda = SchemeLambdaLex(pars,body,frmSiz,frmOfs,pos)
      val value = lattice.closure((lambda,component),Some(id.name))
      writeAddr(VarAddr(ofs),value)
      value
    }
    private def evalSequence(exps: List[SchemeExp]): Value =
      exps.foldLeft(lattice.bottom)((_,exp) => eval(exp))
    private def evalSet(lex: LexicalAddr, exp: SchemeExp): Value = {
      val addr = resolveAddr(lex)
      val newValue = eval(exp)
      writeAddr(addr,newValue)
      newValue
    }
    private def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): Value =
      conditional(eval(prd), eval(csq), eval(alt))
    private def evalLetExp(bindings: List[(Int,SchemeExp)], body: List[SchemeExp]): Value = {
      evalLetBindings(bindings)
      evalSequence(body)
    }
    private def evalNamedLet(namedLet: SchemeNamedLetLex): Value = {
      val SchemeNamedLetLex(id,ofs,bds,ags,bdy,fSiz,fOfs,pos) = namedLet
      val lambda = SchemeLambdaLex(bds.map(_._1),bdy,fSiz,fOfs,pos)
      val closure = lattice.closure((lambda,component),Some(id.name))
      writeAddr(VarAddr(ofs),closure)
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
        case (clo@(lam: SchemeLambdaLex, cmp), nam) if lam.args.length == arity =>
          val context = allocCtx(clo,args)
          val component = CallComponent(lam,cmp,nam,context)
          bindArgs(component, args)
          call(component)
        case _ => lattice.bottom
      }))
    }
    // TODO[minor]: use foldMap instead of foldLeft
    val allocator = new SchemeAllocator[Addr] {
      def pointer(exp: SchemeExp) = allocAddr(PtrAddr(exp))
    }
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
    private def evalLetBindings(bindings: List[(Int,SchemeExp)]) =
      bindings.foreach { case (ofs,exp) => writeAddr(VarAddr(ofs),eval(exp)) }
    private def bindArgs(component: IntraComponent, args: List[Value]) = {
      var ofs = 0
      args.foreach(arg => {
        writeAddr(VarAddr(ofs),arg,component)
        ofs = ofs + 1
      })
    }
  }
}

//abstract class AdaptiveSchemeModFAnalysis(program: SchemeExp) extends AdaptiveModAnalysis(program)
//                                                              with AdaptiveSchemeModFSemantics
