package primitiveCompilation

import scalaam.core.{Identifier, Identity}
import scalaam.language.scheme._
import scalaam.primitiveCompilation.PrimCompiler.IllegalExpressionException

object ANFCompiler {

  // Based on https://github.com/acieroid/scala-am/blob/e5d16e78418e71cedbc76048013480ac16f5c407/src/main/scala/exp/anf/ANF.scala
  // and on https://github.com/acieroid/scala-am/blob/24f21a0157dcc5320f6e15d1720d1eb035f8db7b/ANF.scala
  def toANF(e: SchemeExp): SchemeExp = {

    var c: Int = -1

    def newId(): Identifier = {
      val name = s"ID${c.toString}"
      c = c - 1
      Identifier(name, Identity.none)
    }

    def compile(e: SchemeExp): SchemeExp = e match {
      case SchemeLambda(args, body, idn) => SchemeLambda(args, List(compileBody(body, idn)), idn)
      //case SchemeVarArgLambda(args, vararg, body, idn) =>
      case SchemeFuncall(f, Nil, idn) => SchemeFuncall(compile(f), Nil, idn)
      case SchemeFuncall(f, arg :: Nil, idn) =>
        val id = newId()
        val id2 = newId()
        SchemeLet(List((id, compile(f))), List(SchemeLet(List((id, compile(arg))), List(SchemeFuncall(SchemeVar(id), List(SchemeVar(id2)), idn)), idn)), idn)
      case SchemeFuncall(f, args, idn) =>
        val id = newId()
        SchemeLet(List((id, compile(f))), List(???), idn) // TODO FIX THIS
        //SchemeFuncall(compile(f), args.map(compile), idn)
      case SchemeIf(cond, cons, alt, idn) =>
        val id = newId()
        SchemeLet(List((id, compile(cond))), List(SchemeIf(SchemeVar(id), compile(cons), compile(alt), idn)), idn)
      case SchemeLet(bindings, body, idn) => bindings.foldRight(compileBody(body, idn))((binding, e) => SchemeLet(List((binding._1, compile(binding._2))), List(e), idn))
      case SchemeLetStar(bindings, body, idn) => bindings.foldRight(compileBody(body, idn))((binding, e) => SchemeLet(List((binding._1, compile(binding._2))), List(e), idn))
      //case SchemeLetrec(bindings, body, idn) =>
      //case SchemeNamedLet(name, bindings, body, idn) =>
      case SchemeSet(variable, value, idn) => SchemeSet(variable, compile(value), idn)
      case SchemeSetLex(variable, lexAddr, value, idn) => SchemeSetLex(variable, lexAddr, compile(value), idn)
      case SchemeBegin(exps, idn) => compileBody(exps, idn)
      case SchemeAnd(exps, idn) => ??? // TODO
      case SchemeOr(exps, idn) => ??? // TODO
      //case SchemeDefineVariable(name, value, idn) =>
      //case SchemeDefineFunction(name, args, body, idn) =>
      //case SchemeDefineVarArgFunction(name, args, vararg, body, idn) =>
      case v@SchemeVar(id) => v
      case v@SchemeVarLex(id, lexAdr) => v
      //case SchemePair(car, cdr, idn) =>
      //case SchemeSplicedPair(splice, cdr, idn) =>
      case v@SchemeValue(value, idn) => v
      case e =>
        System.err.println(e)
        throw IllegalExpressionException(e)
    }

    def compileBody(body: List[SchemeExp], idn: Identity): SchemeExp = body match {
      case Nil => throw new Exception("Encountered empty function body.")
      case e :: Nil => compile(e)
      case e :: rest => SchemeLet(List((newId(), compile(e))), List(compileBody(rest, idn)), idn)
    }

    e match {
      case SchemeDefineFunction(name, args, body, idn) => compile(SchemeLambda(args, body, idn)) match {
        case SchemeLambda(args, body, idn) => SchemeDefineFunction(name, args, body, idn)
      }
      case e => throw IllegalExpressionException(e)
    }
  }
}
