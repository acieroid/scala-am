package scalaam.core

/** An expression */
trait Expression extends SmartHash {

  /** The position of the expression in its source file */
  def pos: Position

  /** The set of free variables appearing in this expression */
  def fv: Set[String]
}
