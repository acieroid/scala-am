package scalaam.language.CScheme

import scalaam.language.change.CodeChange
import scalaam.language.scheme._

object CSchemeUndefiner extends BaseSchemeUndefiner {
  import scala.util.control.TailCalls._

  override def undefineExp(exp: SchemeExp): TailRec[SchemeExp] = exp match {
    case    CSchemeFork(body, idn) => tailcall(undefine1(body)).map(   CSchemeFork(_, idn))
    case    CSchemeJoin(body, idn) => tailcall(undefine1(body)).map(   CSchemeJoin(_, idn))

    case  CodeChange(old, nw, idn) => tailcall(undefine1(old)).flatMap(oldu => tailcall(undefine1(nw)).map(newu => CodeChange(oldu, newu, idn)))

    case _ => super.undefineExp(exp)
  }
}
