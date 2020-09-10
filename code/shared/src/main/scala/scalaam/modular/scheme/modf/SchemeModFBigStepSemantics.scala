package scalaam.modular.scheme.modf

import scalaam.core._
import scalaam.language.scheme._
import scalaam.util.benchmarks.Timeout

object EvalM {
  /* EvalM allows for big-step computations in "monadic" style */
  case class EvalM[+X](run: Environment[Address] => Option[X]) {
    def flatMap[Y](f: X => EvalM[Y]): EvalM[Y]  = EvalM { env => run(env).flatMap(res => f(res).run(env)) }
    def map[Y](f: X => Y): EvalM[Y]             = EvalM { env => run(env).map(f) }
  } 
  def unit[X](x: X): EvalM[X] = EvalM(_ => Some(x))
  def mzero[X]: EvalM[X]      = EvalM(_ => None)
  def guard(cnd: Boolean): EvalM[Unit] = if(cnd) EvalM(_ => Some(())) else mzero
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
  def getEnv: EvalM[Environment[Address]] = EvalM { env => Some(env) }  
  // TODO: withExtendedEnv would make more sense
  def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => EvalM[X]): EvalM[X] = 
    EvalM { env => ev.run(f(env)) }
  def inject[X: Lattice](x: X): EvalM[X] = if (x == Lattice[X].bottom) mzero else unit(x)
  def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] = EvalM { env => 
    (x.run(env), y.run(env)) match {
      case (None, yres)             => yres
      case (xres, None)             => xres
      case (Some(res1), Some(res2)) => Some(Lattice[X].join(res1,res2))
    }
  }
  def merge[X: Lattice](xs: Iterable[EvalM[X]]): EvalM[X] =
    xs.foldLeft[EvalM[X]](mzero)((acc,x) => merge(acc,x)) 
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
      eval(fnBody).run(fnEnv).map(res => writeResult(res))
    // simple big-step eval
    protected def eval(exp: SchemeExp): EvalM[Value] = exp match {
      case SchemeValue(value, _)                    => unit(evalLiteralValue(value))
      case lambda: SchemeLambdaExp                  => evalClosure(lambda)
      case SchemeVar(nam)                           => evalVariable(nam)
      case SchemeBegin(exps, _)                     => evalSequence(exps)
      case SchemeSet(id, vexp, _)                   => evalSet(id, vexp)
      case SchemeIf(prd, csq, alt, _)               => evalIf(prd, csq, alt)
      case SchemeLet(bindings, body, _)             => evalLet(bindings, body)
      case SchemeLetStar(bindings, body, _)         => evalLetStar(bindings, body)
      case SchemeLetrec(bindings, body, _)          => evalLetRec(bindings, body)
      case SchemeNamedLet(name,bindings,body,pos)   => evalNamedLet(name,bindings,body,pos)
      case call@SchemeFuncall(fun, args, _)         => evalCall(call, fun, args)
      case SchemeAnd(exps, _)                       => evalAnd(exps)
      case SchemeOr(exps, _)                        => evalOr(exps)
      case pair: SchemePair                         => evalPair(pair)
      case pair: SchemeSplicedPair                  => evalSplicedPair(pair)
      case _ => throw new Exception(s"Unsupported Scheme expression: $exp")
    }
    private def evalVariable(id: Identifier): EvalM[Value] =
      getEnv.flatMap(env => inject(lookup(id,env)))
    private def evalClosure(lam: SchemeLambdaExp): EvalM[Value] =
      for { env <- getEnv } yield newClosure(lam, env, None)
    private def evalSequence(exps: List[SchemeExp]): EvalM[Value] =
      exps.foldLeftM(lattice.void)((_, exp) => eval(exp))
    private def evalSet(id: Identifier, exp: SchemeExp): EvalM[Value] =
      for {
        rhs <- eval(exp)
        env <- getEnv
        _ = assign(id, env, rhs)
      } yield lattice.void
    private def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
      for {
        prdVal <- eval(prd)
        resVal <- cond(prdVal, eval(csq), eval(alt))
      } yield resVal
    private def evalLet(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp]): EvalM[Value] = 
      for {
        bds <- bindings.mapM { case (id, exp) => eval(exp).map(vlu => (id,vlu)) }
        res <- withEnv(env => bind(bds, env)) { 
          evalSequence(body) 
        }
      } yield res
    private def evalLetStar(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
      bindings match {
        case Nil => evalSequence(body)
        case (id, exp) :: restBds => 
          eval(exp).flatMap { rhs =>
            withEnv(env => bind(id,env,rhs)) {
              evalLetStar(restBds, body)
            }
          }
      }
    private def evalLetRec(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
      withEnv(env => bindings.foldLeft(env) { case (env2, (id, _)) => bind(id, env2, lattice.bottom) }) {
        for {
          extEnv <- getEnv
          _ <- bindings.mapM_ { 
            case (id, lam: SchemeLambdaExp) => 
              assign(id, extEnv, newClosure(lam,extEnv,Some(id.name)))
              unit(())
            case (id, exp) =>
              eval(exp).map(value => assign(id, extEnv, value))
          }
          res <- evalSequence(body)
        } yield res
      }
    private def evalNamedLet(id: Identifier, bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], idn: Identity): EvalM[Value] =
      for {
        env <- getEnv
        (prs,ags) = bindings.unzip
        lambda = SchemeLambda(prs, body, idn)
        extEnv = bind(id, env, lattice.bottom)
        closure = newClosure(lambda, extEnv, Some(id.name))
        _ = assign(id, extEnv, closure)
        call = SchemeFuncall(lambda,ags,idn)
        argVals <- ags.mapM(exp => for { vlu <- eval(exp) } yield (exp,vlu))
      } yield applyFun(call,closure,argVals,id.idn.pos)
    // R5RS specification: if all exps are 'thruty', then the value is that of the last expression
    private def evalAnd(exps: List[SchemeExp]): EvalM[Value] =
      if (exps.isEmpty) { unit(lattice.bool(true)) } else { evalAndLoop(exps) }
    private def evalAndLoop(exps: List[SchemeExp]): EvalM[Value] = (exps: @unchecked) match {
      case exp :: Nil => eval(exp)
      case exp :: rst => for {
        vlu <- eval(exp)
        res <- cond(vlu, evalAndLoop(rst), unit(lattice.bool(false)))
      } yield res
    }
    private def evalOr(exps: List[SchemeExp]): EvalM[Value] = exps match {
      case Nil        => unit(lattice.bool(false))
      case exp :: rst => for {
        vlu <- eval(exp)
        res <- cond(vlu, unit(vlu), evalOr(rst))
      } yield res
    }
    private def evalCall(exp: SchemeFuncall, fun: SchemeExp, args: List[SchemeExp]): EvalM[Value] =
      for {
        funVal    <- eval(fun)
        argVals   <- args.mapM(eval)
        returned  = applyFun(exp,funVal,args.zip(argVals),fun.idn.pos)
        result    <- inject(returned) 
      } yield result
    private def evalPair(pairExp: SchemePair): EvalM[Value] = 
      for {
        carv <- eval(pairExp.car)
        cdrv <- eval(pairExp.cdr)
      } yield allocateCons(pairExp)(carv,cdrv)
    private def evalSplicedPair(pairExp: SchemeSplicedPair): EvalM[Value] = 
      for {
        splicev <- eval(pairExp.splice)
        cdrv    <- eval(pairExp.cdr)
      } yield append(pairExp)((pairExp.splice, splicev), (pairExp.cdr, cdrv))
  }
}
