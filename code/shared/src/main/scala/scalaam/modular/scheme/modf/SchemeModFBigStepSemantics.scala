package scalaam.modular.scheme.modf

import scalaam.core._
import scalaam.language.scheme._
import scalaam.util.MonoidImplicits._
import scalaam.util.benchmarks.Timeout
import scalaam.util._

object EvalM {
  /* EvalM allows for big-step computations in "monadic" style */
  sealed trait EvalM[+X] {
    def flatMap[Y](f: X => EvalM[Y]): EvalM[Y]
    def map[Y](f: X => Y): EvalM[Y] 
  }
  case object Bottom extends EvalM[Nothing] {
    def flatMap[Y](f: Nothing => EvalM[Y]): EvalM[Y] = Bottom
    def map[Y](f: Nothing => Y): EvalM[Y] = Bottom
  }
  case class Result[X](res: X) extends EvalM[X] {
    def flatMap[Y](f: X => EvalM[Y]): EvalM[Y] = f(res)
    def map[Y](f: X => Y): EvalM[Y] = Result(f(res))
  }
  def unit[X](x: X): EvalM[X] = Result(x)
  def guard(cnd: Boolean): EvalM[Unit] = if(cnd) { Result(()) } else { Bottom }
  implicit class MonadicOps[X](xs: Iterable[X]) {
    def foldLeftM[Y](y: Y)(f: (Y,X) => EvalM[Y]): EvalM[Y] = xs match {
      case Nil      => unit(y)
      case x :: xs  => f(y,x).flatMap(acc => xs.foldLeftM(acc)(f))
    }
    def mapM[Y](f: X => EvalM[Y]): EvalM[List[Y]] = xs match {
      case Nil      => unit(Nil)
      case x :: xs  => for {
        fx    <- f(x)
        rest  <- xs.mapM(f)
      } yield fx :: rest
    }
    def mapM_(f: X => EvalM[()]): EvalM[()] = xs match {
      case Nil => unit(())
      case x :: xs => f(x).flatMap(_ => xs.mapM_(f))
    }
  }  
  def inject[X: Lattice](x: X): EvalM[X] = if (x == Lattice[X].bottom) { Bottom } else { Result(x) }
  def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] = (x,y) match {
    case (Bottom, _)                  => y
    case (_, Bottom)                  => x
    case (Result(res1), Result(res2)) => Result(Lattice[X].join(res1,res2))
  }
  def merge[X: Lattice](xs: Iterable[EvalM[X]]): EvalM[X] =
    xs.foldLeft[EvalM[X]](Bottom)((acc,x) => merge(acc,x)) 
}

trait BigStepModFSemantics extends BaseSchemeModFSemantics {

  import EvalM._

  private def cond(prd: Value, csq: => EvalM[Value], alt: => EvalM[Value]): EvalM[Value] = {
    val csqValue = guard(lattice.isTrue(prd)).flatMap(_ => csq)
    val altValue = guard(lattice.isFalse(prd)).flatMap(_ => alt)
    merge(csqValue, altValue)
  }

  // defining the intra-analysis
  override def intraAnalysis(cmp: Component): BigStepModFIntra
  trait BigStepModFIntra extends IntraAnalysis with SchemeModFSemanticsIntra {
    // analysis entry point
    def analyze(timeout: Timeout.T = Timeout.none): Unit = // Timeout is just ignored here.
      eval(fnBody, fnEnv).map(res => writeResult(res))
    // simple big-step eval
    protected def eval(exp: SchemeExp, env: Env): EvalM[Value] = exp match {
      case SchemeValue(value, _)                                  => unit(evalLiteralValue(value))
      case lambda: SchemeLambdaExp                                => unit(newClosure(lambda, env, None))
      case SchemeVar(nam)                                         => inject(lookup(nam, env))
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
    private def evalSequence(exps: List[SchemeExp], env: Env): EvalM[Value] =
      exps.foldLeftM(lattice.void)((_,exp) => eval(exp,env))
    private def evalSet(id: Identifier, exp: SchemeExp, env: Env): EvalM[Value] =
      eval(exp, env).map { newValue =>
        assign(id, env, newValue)
        lattice.void
      }
    private def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp, env: Env): EvalM[Value] =
      eval(prd, env).flatMap { prdValue => cond(prdValue, eval(csq,env), eval(alt,env)) }
    private def evalLet(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env): EvalM[Value] = 
      for {
        bdsVals <- bindings.mapM { case (id, exp) => 
          for { value <- eval(exp,env) } yield (id, value) 
        }
        extEnv = bind(bdsVals, env)
        res <- evalSequence(body, extEnv)
      } yield res
    private def evalLetStar(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env): EvalM[Value] = 
      for {
        extEnv <- bindings.foldLeftM(env) { case (env2, (id, exp)) => 
          for { value <- eval(exp,env2) } yield bind(id, env2, value) 
        }
        res <- evalSequence(body, extEnv)
      } yield res
    private def evalLetRec(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env): EvalM[Value] = {
      val extEnv = bindings.foldLeft(env) { case (env2, (id, _)) => bind(id, env2, lattice.bottom) }
      for {
        _ <- bindings.mapM_ { 
          case (id, lam: SchemeLambdaExp) =>
            assign(id, extEnv, newClosure(lam,extEnv,Some(id.name)))
            unit(())
          case (id, exp) =>
            eval(exp, extEnv).map(value => assign(id, extEnv, value))
        }
        res <- evalSequence(body, extEnv)
      } yield res
    }
    private def evalNamedLet(id: Identifier, bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], env: Env, idn: Identity): EvalM[Value] = {
      val (prs,ags) = bindings.unzip
      val lambda = SchemeLambda(prs, body, idn)
      val extEnv = bind(id, env, lattice.bottom)
      val closure = newClosure(lambda, extEnv, Some(id.name))
      assign(id, extEnv, closure)
      val call = SchemeFuncall(lambda,ags,idn)
      for {
        argVals <- ags.mapM(exp => for { vlu <- eval(exp,env) } yield (exp,vlu))
      } yield applyFun(call,closure,argVals,id.idn.pos)
    }
    // R5RS specification: if all exps are 'thruty', then the value is that of the last expression
    private def evalAnd(exps: List[SchemeExp], env: Env): EvalM[Value] =
      if (exps.isEmpty) { unit(lattice.bool(true)) } else { evalAndLoop(exps,env) }
    private def evalAndLoop(exps: List[SchemeExp], env: Env): EvalM[Value] = (exps: @unchecked) match {
      case exp :: Nil => eval(exp,env)
      case exp :: rst => eval(exp,env).flatMap { vlu => 
        cond(vlu, evalAndLoop(rst,env), unit(lattice.bool(false)))
      }
    }
    private def evalOr(exps: List[SchemeExp], env: Env): EvalM[Value] = exps match {
      case Nil        => unit(lattice.bool(false))
      case exp :: rst => eval(exp,env).flatMap { vlu =>
        cond(vlu, unit(vlu), evalOr(rst,env))
      }
    }
    private def evalCall(exp: SchemeFuncall, fun: SchemeExp, args: List[SchemeExp], env: Env): EvalM[Value] =
      for {
        funVal    <- eval(fun, env)
        argVals   <- args.mapM(arg => eval(arg, env))
        returned  = applyFun(exp,funVal,args.zip(argVals),fun.idn.pos)
        result    <- inject(returned) 
      } yield result
    private def evalPair(pairExp: SchemePair, env: Env): EvalM[Value] = 
      for {
        carv <- eval(pairExp.car, env)
        cdrv <- eval(pairExp.cdr, env)
      } yield allocateCons(pairExp)(carv,cdrv)
    private def evalSplicedPair(pairExp: SchemeSplicedPair, env: Env): EvalM[Value] = 
      for {
        splicev <- eval(pairExp.splice, env)
        cdrv    <- eval(pairExp.cdr, env)
      } yield append(pairExp)((pairExp.splice, splicev), (pairExp.cdr, cdrv))
  }
}
