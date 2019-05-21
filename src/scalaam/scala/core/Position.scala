package scalaam.core

/** A position in a source file */
sealed trait Position {
  def column: Int
  def line: Int
  def <(that: Position): Boolean = column < that.column && line < that.line
}

/** A position with a line and column */
case class SimplePosition(line: Int, column: Int) extends Position with SmartHash {
  override def toString = s"$line:$column"
}

/** No position */
object NoPosition extends Position {
  val column = 0
  val line   = 0
  override def toString = "-1:0"
}

object Position {
  def apply(p: scala.util.parsing.input.Position): Position = SimplePosition(p.column, p.line)
  def none: Position                                        = NoPosition

  /** Positions are ordered */
  implicit val ordering: Ordering[Position] = new Ordering[Position] {
    def compare(x: Position, y: Position): Int =
      if (x < y) { -1 } else if (y > x) { 1 } else { 0 }
  }
}
