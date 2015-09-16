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
case class ValueNil() extends Value {
  override def toString() = "()"
}

sealed abstract class SExp
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
      case SExpValue(ValueNil()) => s"$car"
      case _ => s"$car . $cdr"
    }
}
object SExpPair {
  /* Alternative constructor to automatically construct a bunch of pair from a
   * list of expressions */
  def apply(content: List[SExp]) = fromList(content)

  def fromList(content: List[SExp]): SExp = content match {
    case Nil => SExpValue(ValueNil())
    case head :: tail => SExpPair(head, SExpPair(tail))
  }
}
case class SExpIdentifier(name: String) extends SExp {
  override def toString() = name
}
case class SExpValue(value: Value) extends SExp {
  override def toString = value.toString
}

case class SExpQuoted(content: SExp) extends SExp {
  override def toString() = s"'$content"
}
