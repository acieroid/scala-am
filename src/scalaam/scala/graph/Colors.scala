package scalaam.graph

case class Color(hex: String) {
  override def toString = hex
}
object Colors {
  object Yellow extends Color("#FFFFDD")
  object Green  extends Color("#DDFFDD")
  object Pink   extends Color("#FFDDDD")
  object Red    extends Color("#FF0000")
  object White  extends Color("#FFFFFF")
  object Black  extends Color("#000000")
}
