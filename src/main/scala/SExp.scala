/**
 * S-expressions and related values
 */

sealed abstract class Value
case class ValueString(value: String) extends Value {
  override def toString() = "\"" + value + "\"" // https://issues.scala-lang.org/browse/SI-6476
}
case class ValueSymbol(sym: String) extends Value {
  override def toString() = sym
}
case class ValueInteger(value: Integer) extends Value {
  override def toString() = value.toString
}
case class ValueFloat(value: Float) extends Value {
  override def toString() = value.toString
}
case class ValueBoolean(value: Boolean) extends Value {
  override def toString() = value match {
    case true => "#t"
    case false => "#f"
  }
}
case class ValueCharacter(value: Character) extends Value {
  override def toString() = s"#\\$value" // not entirely correct (eg. newline, ...)
}
object ValueNil extends Value {
  override def toString() = "()"
}

/**
 * Abstract grammar elements for S-expressions include some positional
 * information. This serves two purposes: identify where the s-expression
 * resides in the input file, and as tagging information for the abstract
 * machine.
 */
trait SExp extends scala.util.parsing.input.Positional
/**
 * An s-expression is made of pairs, e.g., (foo bar) is represented as the pair
 * with identifier foo as car and another pair -- with identifier bar as car and
 * value nil as cdr -- as cdr. Pairs are pretty-printed when converted to
 * string. i.e., (foo bar) is stringified as (foo bar) and not (foo . (bar
 * . ()))
 */
case class SExpPair(car: SExp, cdr: SExp) extends SExp {
  override def toString() = {
    val content = toStringRest
    s"($content)"
  }
  def toStringRest(): String =
    cdr match {
      case pair: SExpPair =>
        val rest = pair.toStringRest
        s"$car $rest"
      case SExpValue(ValueNil) => s"$car"
      case _ => s"$car . $cdr"
    }
}
object SExpPair {
  /* Alternative constructor to automatically construct a bunch of pair from a
   * list of expressions */
  def apply(content: List[SExp]) = fromList(content)

  def fromList(content: List[SExp]): SExp = content match {
    case Nil => SExpValue(ValueNil)
    case head :: tail => SExpPair(head, SExpPair(tail))
  }
}
/**
 * An identifier, such as foo, bar, etc.
 */
case class SExpIdentifier(name: String) extends SExp {
  override def toString() = name
}
/**
 * A literal value, such as 1, "foo", 'foo, etc.
 */
case class SExpValue(value: Value) extends SExp {
  override def toString = value.toString
}
/**
 * A quoted element, such as 'foo, '(foo (bar)), etc.
 */
case class SExpQuoted(content: SExp) extends SExp {
  override def toString() = s"'$content"
}
