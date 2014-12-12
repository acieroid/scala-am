sealed class SExp
case class SExpNil() extends SExp {
  override def toString() = "()"
}
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
      case SExpNil() => s"$car"
      case _ => s"$car . $cdr"
    }
}
object SExpPair {
  /* Alternative constructor to automatically construct a bunch of pair from a
   * list of expressions */
  def apply(content: List[SExp]) = fromList(content)

  def fromList(content: List[SExp]): SExp = content match {
    case Nil => new SExpNil
    case head :: tail => new SExpPair(head, SExpPair(tail))
  }
}
case class SExpIdentifier(name: String) extends SExp {
  override def toString() = name
}
case class SExpString(value: String) extends SExp {
  override def toString() = "\"" + value + "\"" // https://issues.scala-lang.org/browse/SI-6476
}
case class SExpInteger(value: Integer) extends SExp {
  override def toString() = value.toString
}
case class SExpFloat(value: Float) extends SExp {
  override def toString() = value.toString
}
case class SExpBoolean(value: Boolean) extends SExp {
  override def toString() =
    value match {
      case true => "#t"
      case false => "#f"
    }
}
case class SExpCharacter(value: Character) extends SExp {
  override def toString() = s"#\\$value" // not entirely correct (eg. newline, ...)
}
case class SExpQuoted(content: SExp) extends SExp {
  override def toString() = s"'$content"
}
