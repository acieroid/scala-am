package scalaam.language.lambda

import scalaam.core.{Position, Identifier}
import scalaam.language.sexp._

object LambdaCompiler {
  import scala.util.control.TailCalls._

  def compile(exp: SExp): LambdaExp = compileT(exp).result

  def compileArgsT(args: SExp): TailRec[List[Identifier]] = args match {
    case SExpPair(SExpId(id), rest, _) =>
      for {
        restv <- compileArgsT(rest)
      } yield id :: restv
      // tailcall(compileArgsT(rest)).map(restv => id :: restv)
    case SExpValue(ValueNil, _) => done(Nil)
    case _ => throw new Exception(s"Invalid parameter list: $args (${args.pos})")
  }

  def compileListT(args: SExp): TailRec[List[LambdaExp]] = args match {
    case SExpPair(exp, rest, _) =>
      for {
        expv <- compileT(exp)
        restv <- compileListT(rest)
      } yield expv :: restv
//      tailcall(compileT(exp)).flatMap(expv =>
//        tailcall(compileListT(rest)).map(restv => expv :: restv))
    case SExpValue(ValueNil, _) => done(Nil)
    case _ => throw new Exception(s"Invalid argument list: $args (${args.pos})")
  }


  def compileT(exp: SExp): TailRec[LambdaExp] = exp match {
    case SExpPair(SExpId(Identifier("lambda", _)),
      SExpPair(args,
        SExpPair(body, SExpValue(ValueNil, _), _), _), _) =>
      for {
        argsv <- compileArgsT(args)
        bodyv <- compileT(body)
      } yield LambdaFun(argsv, bodyv, exp.pos)
//      tailcall(compileArgsT(args)).flatMap(argsv =>
//        tailcall(compileT(body)).map(bodyv =>
//          LambdaFun(argsv, bodyv, exp.pos)))
    case SExpPair(f, args, _) =>
      for {
        fv <- compileT(f)
        argsv <- compileListT(args)
      } yield LambdaCall(fv, argsv, exp.pos)
//      tailcall(compileT(f)).flatMap(fv =>
//        tailcall(compileListT(args)).map(argsv =>
//          LambdaCall(fv, argsv, exp.pos)))
    case SExpId(id) =>
      done(LambdaVar(id))
    case _ => throw new Exception(s"Invalid lambda-calculus expression: $exp (${exp.pos})")
  }
}

object LambdaParser {
  def compile(exp: SExp): LambdaExp = LambdaCompiler.compile(exp)
  def parse(s: String): LambdaExp = SExpParser.parse(s) match {
    case sexp :: Nil => compile(sexp)
    case _ => throw new Exception(s"Invalid lambda-calculus program: $s")
  }
}
