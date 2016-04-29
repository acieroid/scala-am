trait Expression[E]

object Expression {
  implicit object ANFExpExpression extends Expression[ANFExp]
  implicit object SchemeExpExpression extends Expression[SchemeExp]
  implicit object LamExpExpression extends Expression[LamExp]
}
