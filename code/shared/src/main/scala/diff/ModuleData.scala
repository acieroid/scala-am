package scalaam.diff

import scalaam.core.Expression

object ModuleData {

  case class ModuleInfo(name: Option[String], exp: Expression, children: List[ModuleInfo]) { // TODO: Verify that the right expressions are extracted.
    override def toString: String = toStringWith(2, 2)
    private def toStringWith(i: Int, inc: Int): String = children.foldLeft(name.getOrElse("lambda") ++ "\n")((acc, chd) => acc ++ "." * i ++ chd.toStringWith(i + inc, inc))
    def allDescendants(): List[ModuleInfo] = children ::: children.flatMap(_.allDescendants())
  }
}
