package scalaam.modular.scheme.modf

import scalaam.core._
import scalaam.language.scheme._
import scalaam.util.MonoidImplicits._
import scalaam.util.benchmarks.Timeout

trait BigStepModFSemantics extends BaseSchemeModFSemantics {
  // defining the intra-analysis
  override def intraAnalysis(cmp: Component): BigStepModFIntra
  trait BigStepModFIntra extends IntraAnalysis with SchemeModFSemanticsIntra {
    // analysis entry point
    def analyze(timeout: Timeout.T = Timeout.none): Unit = // Timeout is just ignored here.
      writeResult(eval(fnBody, fnEnv)) 
    // simple big-step eval
    protected def eval(exp: SchemeExp, env: Env): Value = exp match {
      case SchemeValue(value, _)                                  => evalLiteralValue(value)
      case lambda: SchemeLambdaExp                                => newClosure(lambda, env, None)
      case SchemeVar(nam)                                         => lookup(nam, env)
      case SchemeBegin(exps, _)                                   => evalSequence(exps, env)
      case SchemeSet(id, vexp, _)                                 => evalSet(id, vexp, env)
      case SchemeIf(prd, csq, alt, _)                             => evalIf(prd, csq, alt, env)
      case SchemeLet(bindings, body, _)                           => evalLet(bindings, body, env)
      case SchemeLetStar(bindings, body, _)                       => evalLetStar(bindings, body, env)
      case SchemeLetrec(bindings, body, _)                        => evalLetRec(bindings, body, env)
      case SchemeNamedLet(name,bindings,body,pos)                 => evalNamedLet(name,bindings,body,env,pos)
      case call@SchemeFuncall(fun, args, _)                       => evalCall(call, fun, args, env)
      case SchemeAnd(exps, _)                                     => evalAnd(exps, env)
      case SchemeOr(exps, _)                                      => evalOr(exps, env)
      case pair: SchemePair                                       => evalPair(pair, env)
      case pair: SchemeSplicedPair                                => evalSplicedPair(pair, env)
      case _ => throw new Exception(s"Unsupported Scheme expression: $exp")
    }
    private def evalSequence(exps: List[SchemeExp], env: Env): Value =
      exps.foldLeft(lattice.void)((_,exp) => eval(exp,env))
    private def evalSet(id: Identifier, exp: SchemeExp, env: Env): Value = {
      val newValue = eval(exp,env)
      assign(id,env,newValue)
      lattice.void
    }
    private def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp, env: Env): Value = 
      conditional(eval(prd,env), eval(csq,env), eval(alt,env))
    private def evalLet(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env): Value = {
      val bdsVals = bindings.map { case (id, exp) => (id, eval(exp,env)) }
      val extEnv = bind(bdsVals, env)
      evalSequence(body, extEnv)
    }
    private def evalLetStar(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env): Value = {
      val extEnv = bindings.foldLeft(env) { case (env2, (id, exp)) => 
        bind(id, env2, eval(exp,env2)) 
      }
      evalSequence(body, extEnv)
    }
    private def evalLetRec(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env): Value = {
      val extEnv = bindings.foldLeft(env) { case (env2, (id, _)) => bind(id, env2, lattice.bottom) }
      val bdsVals = bindings.map { case (id, exp) => (id, exp match {
        //TODO: maybe there are better ways to preserve lambda names?
        case lambda: SchemeLambdaExp => newClosure(lambda, extEnv, Some(id.name))
        case _ => eval(exp, extEnv)
      })}
      assign(bdsVals, extEnv)
      evalSequence(body, extEnv)
    }
    private def evalNamedLet(id: Identifier, bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env, idn: Identity): Value = {
      val (prs,ags) = bindings.unzip
      val lambda = SchemeLambda(prs, body, idn)
      val extEnv = bind(id, env, lattice.bottom)
      val closure = newClosure(lambda, extEnv, Some(id.name))
      assign(id, extEnv, closure)
      val call = SchemeFuncall(lambda,ags,idn)
      val argsVals = ags.map(argExp => (argExp, eval(argExp,env)))
      applyFun(call,closure,argsVals,id.idn.pos)
    }
    // R5RS specification: if all exps are 'thruty', then the value is that of the last expression
    private def evalAnd(exps: List[SchemeExp], env: Env): Value =
      if (exps.isEmpty) { lattice.bool(true) } else { evalAndLoop(exps,env) }
    private def evalAndLoop(exps: List[SchemeExp], env: Env): Value = (exps: @unchecked) match {
      case exp :: Nil => eval(exp,env)
      case exp :: rst => conditional(eval(exp,env),evalAndLoop(rst,env),lattice.bool(false))
    }
    private def evalOr(exps: List[SchemeExp], env: Env): Value = exps match {
      case Nil        => lattice.bool(false)
      case exp :: rst =>
        val vlu = eval(exp,env)
        conditional(vlu,vlu,evalOr(rst,env))
    }
    private def evalCall(exp: SchemeFuncall, fun: SchemeExp, args: List[SchemeExp], env: Env): Value = {
      val funVal = eval(fun,env)
      val argVals = args.map(arg => eval(arg,env))
      applyFun(exp,funVal,args.zip(argVals),fun.idn.pos)
    }
    private def evalPair(pairExp: SchemePair, env: Env): Value = {
      val carv = eval(pairExp.car, env)
      val cdrv = eval(pairExp.cdr, env)
      allocateCons(pairExp)(carv,cdrv)
    }
    private def evalSplicedPair(pairExp: SchemeSplicedPair, env: Env): Value = {
      val splicev = eval(pairExp.splice, env)
      val cdrv = eval(pairExp.cdr, env)
      append(pairExp)((pairExp.splice, splicev), (pairExp.cdr, cdrv))
    }
  }
}
