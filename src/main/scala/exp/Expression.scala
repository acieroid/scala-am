trait Expression[E] {
  def pos(e: E): Position
}

object Expression {
  def apply[E : Expression]: Expression[E] = implicitly
}
