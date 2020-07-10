package scalaam.language.CScheme

import scalaam.language.change.CodeChange
import scalaam.language.scheme._

object CSchemeLexicalAddresser extends BaseSchemeLexicalAddresser {

  override def translate(exp: SchemeExp, scope: CSchemeLexicalAddresser.Scope): SchemeExp = exp match {
    case    CSchemeFork(exp, idn) =>    CSchemeFork(translate(exp, scope), idn)
    case    CSchemeJoin(exp, idn) =>    CSchemeJoin(translate(exp, scope), idn)

    case CodeChange(old, nw, idn) =>     CodeChange(translate(old, scope), translate(nw, scope), idn)

    case _                        => super.translate(exp, scope)
  }
}
