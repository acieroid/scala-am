/**
 * This trait represents a position in a source file. It's currently a wrapper
 * around scala.util.parsing.input.Position, but will probably be updated in the
 * future.
 */
sealed trait Position {
  def <(that: Position): Boolean
}

/** The actual wrapper */
case class SomePosition(p: scala.util.parsing.input.Position) extends Position {
  def <(that: Position) = that match {
    case SomePosition(p2) => p < p2
  }
}

object Position {
  def apply(p: scala.util.parsing.input.Position): Position = SomePosition(p)
  def none: Position = SomePosition(scala.util.parsing.input.NoPosition)
}

/** An identifier has a name and a position */
case class Identifier(name: String, pos: Position) {
  override def toString = name
}
