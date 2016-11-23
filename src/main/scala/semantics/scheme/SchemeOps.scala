object SchemeOps {
  /** These are the unary operations that should be supported by Scheme lattices */
  object UnaryOperator extends Enumeration {
    val
      /* Check the type of a value */
      IsNull, IsBoolean, IsCons, IsChar, IsSymbol, IsString, IsInteger, IsFloat, IsVector,
      /* Negate a value */
      Not,
      /* Unary arithmetic operations */
      Ceiling, Log, Random,
      /* Length operations */
      VectorLength, StringLength,
      /* Conversions */
      NumberToString
    = Value
  }
  type UnaryOperator = UnaryOperator.Value

  /** Binary operations that should be supported by lattices */
  object BinaryOperator extends Enumeration {
    val
      /* Arithmetic operations */
      Plus, Minus, Times, Div, Modulo,
      /* Arithmetic comparison */
      Lt,
      /* Equality checking */
      NumEq, /* number equality */
      Eq, /* physical equality */
      /* String operations */
      StringAppend
    = Value
  }
  type BinaryOperator = BinaryOperator.Value

  /** Modulo in Scheme and Scala are different. This implements the same behavior as Scheme's modulo */
  def modulo(n1: Int, n2: Int): Int =
    if (scala.math.signum(n1) * scala.math.signum(n2) < 0) {
      /* different sign, behaviour not the same between Scheme and Scala, adjust it */
      (scala.math.abs(n2) - scala.math.abs(n1) % scala.math.abs(n2)) % scala.math.abs(n2) * (if (n2 < 0) -1 else 1)
    } else {
      /* same sign, same behaviour */
      n1 % n2
    }
  def random(n: Int): Int = scala.math.abs(scala.util.Random.nextInt % n)
  def random(n: Float): Float = scala.math.abs(scala.util.Random.nextFloat % n)
}
