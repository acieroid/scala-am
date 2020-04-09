package scalaam.primitiveCompilation

import scalaam.core.{Identifier, Identity}
import scalaam.language.sexp._
import scalaam.language.scheme._
import scalaam.primitiveCompilation.PrimCompiler.IllegalExpressionException

object ANFCompiler {

  // Based on https://github.com/acieroid/scala-am/blob/e5d16e78418e71cedbc76048013480ac16f5c407/src/main/scala/exp/anf/ANF.scala
  // and on https://github.com/acieroid/scala-am/blob/24f21a0157dcc5320f6e15d1720d1eb035f8db7b/ANF.scala
  def toANF(e: SchemeExp): SchemeExp = {

    var varCount = -1
    def newVar() = {
      varCount = varCount + 1
      s"_$varCount"
    }

    /** Return a non-atomic value to a continuation, storing the result in a
      * let-bound variable. Don't call the continuation if tail is true. */
    def ret(e: SchemeExp, tail: Boolean, k: SchemeExp => SchemeExp): SchemeExp = {
      if (tail) {
        e
      } else {
        val v = newVar()
        SchemeLet(List((Identifier(v, e.idn), e)), List(k(SchemeVar(Identifier(v, e.idn)))), e.idn)
      }
    }
    def identity: SchemeExp => SchemeExp = e => e

    def compileBody(body: List[SchemeExp], idn: Identity, k: SchemeExp => SchemeExp): SchemeExp = body match {
      case Nil => throw new Exception("Cannot compile empty body")
      case e :: Nil => compile(e, true, identity)
      case e :: rest => compile(e, false, a => {
        val v = newVar
        SchemeLet(List((Identifier(v, e.idn), a)), List(compileBody(rest, idn, identity)), idn)
      })
    }

    def compileList(exps: List[SchemeExp], k: List[SchemeExp] => SchemeExp): SchemeExp = compileListHelper(exps, k, List())
    def compileListHelper(exps: List[SchemeExp], k: List[SchemeExp] => SchemeExp, acc: List[SchemeExp]): SchemeExp = exps match {
      case Nil => k(acc.reverse)
      case exp :: rest => compile(exp, false, e => compileListHelper(rest, k, e :: acc))
    }

    def compile(exp: SchemeExp, tail: Boolean = true, k: SchemeExp => SchemeExp = identity): SchemeExp = exp match {
      case SchemeLambda(args, body, idn) =>
        k(SchemeLambda(args, List(compileBody(body, idn, identity)), idn))
      case SchemeFuncall(f, args, idn) =>
        compile(f, false, a => compileList(args, as => ret(SchemeFuncall(a, as, idn), tail, k)))
      case SchemeIf(cond, cons, alt, idn) =>
        compile(cond, false, a => ret(SchemeIf(a, compile(cons), compile(alt), idn), tail, k))
      case SchemeLet(bindings, body, idn) =>
        bindings.reverse.foldLeft(compileBody(body, idn, identity))(
        (e: SchemeExp, binding: (Identifier, SchemeExp)) =>
        SchemeLet(List((binding._1, compile(binding._2))), List(e), idn))
      case SchemeLetStar(bindings, body, idn) =>
        bindings.reverse.foldLeft(compileBody(body, idn, identity))(
          (e: SchemeExp, binding: (Identifier, SchemeExp)) =>
          SchemeLet(List((binding._1, compile(binding._2))), List(e), idn))
      case SchemeLetrec(bindings, body, idn) =>
        /* TODO: we should at least warn when desugaring mutually-recursive functions,
         * as they are not supported. The best solution would be to desugar them
         * using set! to support them */
        bindings.reverse.foldLeft(compileBody(body, idn, identity))(
          (e: SchemeExp, binding: (Identifier, SchemeExp)) =>
          SchemeLetrec(List((binding._1, compile(binding._2))), List(e), idn))
      case SchemeSet(variable, value, idn) =>
        compile(value, false, a => ret(SchemeSet(variable, a, idn), tail, k))
      case SchemeBegin(body, idn) =>
        compileBody(body, idn, a => k(a))
      case SchemeDefineFunction(name, args, body, idn) =>
        compile(SchemeDefineVariable(name, SchemeLambda(args, body, idn), idn), tail, k)
      case SchemeDefineVariable(_, _, _) =>
        throw new Exception("define not supported")
      case SchemeAnd(List(), idn) =>
        k(SchemeValue(ValueBoolean(true), idn))
      case SchemeAnd(List(e), _) =>
        compile(e, tail, k)
      case SchemeAnd(e :: es, idn) =>
        compile(e, false, a => ret(SchemeIf(a, compile(SchemeAnd(es, idn)), SchemeValue(ValueBoolean(false), idn), idn), tail, k))
      case SchemeOr(List(), idn) =>
        k(SchemeValue(ValueBoolean(false), idn))
      case SchemeOr(List(e), _) =>
        compile(e, tail, k)
      case SchemeOr(e :: es, idn) =>
        compile(e, false, a => ret(SchemeIf(a, SchemeValue(ValueBoolean(true), idn), compile(SchemeOr(es, idn)), idn), tail, k))
      case SchemeVar(v) =>
        k(SchemeVar(v))
//      case SchemeQuoted(quoted, idn) =>
        /* a quoted value is not atomic, as it may require an allocation to be evaluated */
//        ret(SchemeQuoted(quoted, idn), tail, k)
      case SchemeValue(value, idn) =>
        k(SchemeValue(value, idn))
      case _ =>
        throw new Exception(s"Unhandled expression in ANF compiler: $exp")
    }

    e match {
      case SchemeDefineFunction(name, args, body, idn) => compile(SchemeLambda(args, body, idn)) match {
        case SchemeLambda(args, body, idn) => SchemeDefineFunction(name, args, body, idn)
      }
      case e => throw IllegalExpressionException(e)
    }
  }
}
