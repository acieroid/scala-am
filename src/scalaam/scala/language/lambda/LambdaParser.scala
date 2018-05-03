package scalaam.language.lambda

import scalaam.core.{Position, Identifier}
import scalaam.language.sexp._

trait LambdaExp {
  val pos: Position
}

case class LambdaFun(args: List[Identifier], body: LambdaExp, pos: Position) extends LambdaExp {
  override def toString = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
}

case class LambdaCall(f: LambdaExp, args: List[LambdaExp], pos: Position) extends LambdaExp {
  override def toString = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
}

case class LambdaVar(id: Identifier) extends LambdaExp {
  val pos = id.pos
  override def toString = id.name
}

object LambdaCompiler {
  import scalaz.Free.Trampoline
  import scalaz.Trampoline

  def compile(exp: SExp): LambdaExp = compileT(exp).run

  def compileArgsT(args: SExp): Trampoline[List[Identifier]] = args match {
    case SExpPair(SExpId(id), rest, _) =>
      Trampoline.suspend(compileArgsT(rest)).map(restv => id :: restv)
    case SExpValue(ValueNil, _) => Trampoline.done(Nil)
    case _ => throw new Exception(s"Invalid parameter list: $args (${args.pos})")
  }

  def compileListT(args: SExp): Trampoline[List[LambdaExp]] = args match {
    case SExpPair(exp, rest, _) =>
      Trampoline.suspend(compileT(exp)).flatMap(expv =>
        Trampoline.suspend(compileListT(rest)).map(restv => expv :: restv))
    case SExpValue(ValueNil, _) => Trampoline.done(Nil)
    case _ => throw new Exception(s"Invalid argument list: $args (${args.pos})")
  }


  def compileT(exp: SExp): Trampoline[LambdaExp] = exp match {
    case SExpPair(SExpId(Identifier("lambda", _)),
      SExpPair(args,
        SExpPair(body, SExpValue(ValueNil, _), _), _), _) =>
      Trampoline.suspend(compileArgsT(args)).flatMap(argsv =>
        Trampoline.suspend(compileT(body)).map(bodyv =>
          LambdaFun(argsv, bodyv, exp.pos)))
    case SExpPair(f, args, _) =>
      Trampoline.suspend(compileT(f)).flatMap(fv =>
        Trampoline.suspend(compileListT(args)).map(argsv =>
          LambdaCall(fv, argsv, exp.pos)))
    case SExpId(id) =>
      Trampoline.done(LambdaVar(id))
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
