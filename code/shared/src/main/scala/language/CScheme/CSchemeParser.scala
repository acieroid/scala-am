package scalaam.language.CScheme

import language.CScheme.CSchemeUndefiner
import scalaam.core.Position._
import scalaam.language.scheme._
import scalaam.language.sexp._

object CSchemeParser {

  /**
   * Compiles a s-expression into a CScheme expression.
   */
  def compile(exp: SExp): SchemeExp = CSchemeCompiler.compile(exp)

  /**
   * Replace defines in a program (a list of expressions) by a big letrec as a single expression.
   */
  def undefine(exps: List[SchemeExp]): SchemeExp = CSchemeUndefiner.undefine(exps)

  /**
   * Parse a string representing a CScheme program.
   */
  def parse(s: String, tag: PTag = noTag): SchemeExp = undefine(List(SchemeBody(SExpParser.parse(s, tag).map(compile))))
}

