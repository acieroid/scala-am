package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.scheme._

trait SchemeAddr[+Context] extends Address
case class VarAddr[Context](id: Identifier, ctx: Context)   extends SchemeAddr[Context] { def printable = true;  def idn: Identity =  id.idn;       override def toString: String = s"var ($id)"        }
case class PtrAddr[Context](exp: SchemeExp, ctx: Context)   extends SchemeAddr[Context] { def printable = false; def idn: Identity =  exp.idn;      override def toString: String = s"ptr (${exp.idn})" }
case class PrmAddr(nam: String)                             extends SchemeAddr[Nothing] { def printable = false; def idn: Identity = Identity.none; override def toString: String = s"prm ($nam)"       }
