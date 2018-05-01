package scalaam.core

/** An identifier. It has a name and a position */
case class Identifier(name: String, pos: Position) {
  override def toString = name
}
