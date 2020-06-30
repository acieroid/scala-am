package language.CScheme

import scalaam.language.CScheme._
import scalaam.language.scheme._

object CSchemeLexicalAddresser extends BaseSchemeLexicalAddresser {

  override def translate(exp: SchemeExp, scope: CSchemeLexicalAddresser.Scope): SchemeExp = exp match {
    case    CSchemeFork(exp, idn) =>    CSchemeFork(translate(exp, scope), idn)
    case    CSchemeJoin(exp, idn) =>    CSchemeJoin(translate(exp, scope), idn)
    case CSchemeAcquire(exp, idn) => CSchemeAcquire(translate(exp, scope), idn)
    case CSchemeRelease(exp, idn) => CSchemeRelease(translate(exp, scope), idn)
    case _ => super.translate(exp, scope)
  }
}
