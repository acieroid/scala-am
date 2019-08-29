package scalaam.core

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Position) extends SmartHash {
  def fullString        = s"$name@$pos"
  override def toString = name
}
