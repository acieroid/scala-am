package scalaam.core

import scalaam.util.SmartHash

/** An expression */
trait Expression extends SmartHash {

  /** The position of the expression in its source file */
  def pos: Position

  /** The set of free variables appearing in this expression */
  def fv: Set[String]
}

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Position) extends Expression with SmartHash {
  def fullString        = s"$name@$pos"
  override def toString = name
  def fv                = Set(name)
}
