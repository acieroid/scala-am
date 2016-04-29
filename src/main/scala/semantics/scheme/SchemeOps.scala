object SchemeOps {
  /** These are the unary operations that should be supported by Scheme lattices */
  trait UnaryOperator
  /* Checks the type of a value */
  case object IsNull extends UnaryOperator
  case object IsCons extends UnaryOperator
  case object IsChar extends UnaryOperator
  case object IsSymbol extends UnaryOperator
  case object IsString extends UnaryOperator
  case object IsInteger extends UnaryOperator
  case object IsFloat extends UnaryOperator
  case object IsBoolean extends UnaryOperator
  case object IsVector extends UnaryOperator
  case object IsLock extends UnaryOperator
  /* Checks if a lock is considered locked */
  case object IsLocked extends UnaryOperator
  /* Negate a value */
  case object Not extends UnaryOperator
  /* Unary arithmetic operations */
  case object Ceiling extends UnaryOperator
  case object Log extends UnaryOperator
  case object Random extends UnaryOperator
  /* Length operations */
  case object VectorLength extends UnaryOperator
  case object StringLength extends UnaryOperator
  /* Conversions */
  case object NumberToString extends UnaryOperator

  /** Binary operations thatt should be supported by lattices */
  trait BinaryOperator
  /* Arithmetic operations */
  case object Plus extends BinaryOperator
  case object Minus extends BinaryOperator
  case object Times extends BinaryOperator
  case object Div extends BinaryOperator
  case object Modulo extends BinaryOperator
  /* Arithmetic comparison */
  case object Lt extends BinaryOperator
  /* Equality checking */
  case object NumEq extends BinaryOperator /* number equality */
  case object Eq extends BinaryOperator /* physical equality */
  /* String operations */
  case object StringAppend extends BinaryOperator

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
