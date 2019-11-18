package scalaam.core

import scalaam.util.SmartHash

/** An expression */
trait Expression extends SmartHash {

  /** The position of the expression in its source file. */
  def pos: Position

  /** The set of free variables appearing in this expression. */
  def fv: Set[String]

  /** The height of the AST represented by this Scheme expression. */
  val height: Int = -1

  /** A label indicating the type of an expression. */
  val label: Label
}

trait Label
case object ID extends Label

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Position) extends Expression with SmartHash {
  def fullString:        String  = s"$name@$pos"
  override def toString: String  = name
  def fv:            Set[String] = Set(name)
  override val height:       Int = 0
  val label:               Label = ID
}