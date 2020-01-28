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
    // Returns true if both modules contain the same code, ignoring identities of expressions.
    def identicalExpression(a: ModuleInfo, b: ModuleInfo): Boolean = a.exp.eq(b.exp)
    // Returns the number of modules mapped.
    def traverse(source: ModuleInfo, target: ModuleInfo): Int = {
      // If the expressions are equal, then all their corresponding descendants are equal as well.
      if (identicalExpression(source, target)) {
        val sD = source.allDescendants()
        mapping = mapping ++ sD.zip(target.allDescendants())
        return sD.length
      }
      val sD = source.allDescendants()
      val tD = target.allDescendants()
      // Check for modules with the same name or that are entirely equal.
      (for (s <- sD) yield {
        tD.filter(identicalExpression(_, s)) match {
          case t :: Nil =>
            val sD = s.allDescendants()
            mapping = mapping ++ sD.zip(t.allDescendants())
            return sD.length
          case _ :: _ => 0 // TODO Multiple candidates for matching.
          case Nil =>
            tD.find(_.name == s.name) match {
              case Some(t) =>
                val mCount = traverse(s, t)
                if (mCount != 0) {
                  mapping = mapping + (s -> t)
                  mCount + 1
                } else mCount
              case None => 0
            }
        }

      }).sum
    }
    traverse(source, target)
    mapping
  }

}