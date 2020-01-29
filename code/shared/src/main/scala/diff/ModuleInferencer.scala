package scalaam.diff

import scalaam.core.Expression
import scalaam.diff.ModuleData.ModuleInfo
import scalaam.language.scheme._

object ModuleInferencer {

  def inferModules(exp: Expression): ModuleInfo =
    ModuleInfo(Some("main"), exp, exp.subexpressions.flatMap(traverseAST))

  private def traverseAST(exp: Expression): List[ModuleInfo] = exp match {
    case l: SchemeLambda => List(ModuleInfo(None, l, l.subexpressions.flatMap(traverseAST)))
    case f: SchemeDefineFunctionExp => List(ModuleInfo(Some(f.name.toString), f, f.subexpressions.flatMap(traverseAST)))
    case e => e.subexpressions.flatMap(traverseAST)
  }
}
