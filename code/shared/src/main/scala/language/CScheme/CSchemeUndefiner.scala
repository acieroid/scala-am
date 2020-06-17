package language.CScheme

import scalaam.language.CScheme._
import scalaam.language.scheme._

object CSchemeUndefiner extends BaseSchemeUndefiner {
  import scala.util.control.TailCalls._

  override def undefineExp(exp: SchemeExp): TailRec[SchemeExp] = exp match {
    case CSchemeFork(body, idn) => tailcall(undefine1(body)).map(CSchemeFork(_, idn))
    case CSchemeJoin(body, idn) => tailcall(undefine1(body)).map(CSchemeJoin(_, idn))
    case _ => super.undefineExp(exp)
  }
}
