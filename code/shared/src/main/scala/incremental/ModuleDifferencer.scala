package incremental

import scalaam.core.Expression
import scalaam.language.scheme._

object ModuleDifferencer {

  case class ModuleInfo(name: Option[String], exp: Expression, children: List[ModuleInfo]) { // TODO: Verify that the right expressions are extracted.
    override def toString: String = toStringWith(2, 2)
    def toStringWith(i: Int, inc: Int): String = children.foldLeft(name.getOrElse("lambda") ++ "\n")((acc, chd) => acc ++ "." * i ++ chd.toStringWith(i + inc, inc))
    def allDescendants(): List[ModuleInfo] = children ::: children.flatMap(_.allDescendants())
  }

  def inferModules(exp: Expression): ModuleInfo = {
    ModuleInfo(Some("main"), exp, exp.subexpressions.flatMap(traverseAST))
  }

  def traverseAST(exp: Expression): List[ModuleInfo] = exp match {
    case l: SchemeLambda => List(ModuleInfo(None, l, l.subexpressions.flatMap(traverseAST)))
    case f: SchemeDefineFunctionExp => List(ModuleInfo(Some(f.name.toString), f, f.subexpressions.flatMap(traverseAST)))
    case e => e.subexpressions.flatMap(traverseAST)
  }

  def mapModules(source: ModuleInfo, target: ModuleInfo): Map[ModuleInfo, ModuleInfo] = {
    var mapping: Map[ModuleInfo, ModuleInfo] = Map()
    // Returns the number of modules matched.
    def traverse(s: ModuleInfo, t: ModuleInfo): Int = {
      // If the expressions are equal, then all their corresponding descendants are equal as well.
      if (s.exp.eql(t.exp)) {
        val sD = s.allDescendants()
        mapping = mapping ++ sD.zip(t.allDescendants())
        return sD.length
      }
      val sD = s.allDescendants()
      val tD = t.allDescendants()
      // Check for modules with the same name or that are entirely equal.
      (for (s <- sD) yield {
        tD.find(_.exp.eql(s.exp)) match {
          case Some(t) =>
            val sD = s.allDescendants()
            mapping = mapping ++ sD.zip(t.allDescendants())
            return sD.length
          case None =>
            tD.find(_.name == s.name) match {
              case Some(t) => traverse(s, t)
              case None => 0
            }
        }

      }).sum
    }
    traverse(source, target)
    mapping
  }

}