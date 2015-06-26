trait Expression[A] {
}

object Expression {
  implicit object ANFExpExpression extends Expression[ANFExp]
  implicit object SchemeExpExpression extends Expression[SchemeExp]
}
