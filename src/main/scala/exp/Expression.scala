import scala.util.parsing.input.Position
trait Expression[E] {
  def pos(e: E): Position
}

object Expression {
  implicit object ANFExpExpression extends Expression[ANFExp] {
    def pos(e: ANFExp) = e.pos
  }
  implicit object SchemeExpExpression extends Expression[SchemeExp] {
    def pos(e: SchemeExp) = e.pos
  }
  implicit object LamExpExpression extends Expression[LamExp] {
    def pos(e: LamExp) = e.pos
  }
}
