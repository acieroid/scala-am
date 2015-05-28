trait Expression[A] {
}

object Expression {
  implicit object ANFExpExpression extends Expression[ANFExp]
}
