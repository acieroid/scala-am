package scalaam.language.scheme

import scalaam.core.Position._
import scalaam.language.scheme.primitives.SchemePrelude
import scalaam.language.sexp._

object SchemeParser {

  /**
    * Compiles a s-expression into a scheme expression
    */
  def compile(exp: SExp): SchemeExp = SchemeCompiler.compile(exp)

  /**
    * Performs alpha-renaming to ensure that every variable has a unique name
    */
  def rename(exp: SchemeExp): SchemeExp = SchemeRenamer.rename(exp)

  /**
    * Replace defines in a program (a list of expressions) by a big letrec as a single expression
    */
  def undefine(exps: List[SchemeExp]): SchemeExp = SchemeUndefiner.undefine(exps)

  /**
   * Parse a string representing a Scheme program
   */
  def parse(s: String, tag: PTag = noTag): SchemeExp = SchemeBody(SExpParser.parse(s, tag).map(compile))

  /**
   * Parse a string representing a Scheme program and add the appropriate prelude.
   */
  def parseAddPrelude(s: String, tag: PTag = noTag): SchemeExp = SchemePrelude.addPrelude(SchemeBody(SExpParser.parse(s, tag).map(compile)))
}
