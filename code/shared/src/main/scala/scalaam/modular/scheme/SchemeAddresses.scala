package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.scheme._

trait SchemeAddr extends Address {
  def idn(): Identity
}
case class VarAddr(id: Identifier) extends SchemeAddr { def printable = true;  def idn(): Identity =  id.idn;       override def toString: String = s"var ($id)"        }
case class PtrAddr(exp: SchemeExp) extends SchemeAddr { def printable = false; def idn(): Identity =  exp.idn;      override def toString: String = s"ptr (${exp.idn})" }
case class PrmAddr(nam: String)    extends SchemeAddr { def printable = false; def idn(): Identity = Identity.none; override def toString: String = s"prm ($nam)"       }
