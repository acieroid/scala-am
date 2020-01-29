package scalaam.incremental

import scalaam.diff.ModuleData.ModuleInfo

// WORK IN PROGRESS - Simple first attempt of mapping modules onto each other (but still buggy).
object ModuleDifferencer {

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