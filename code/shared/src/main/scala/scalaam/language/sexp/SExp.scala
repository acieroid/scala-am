package scalaam.language.sexp

import scalaam.core._
import scalaam.language.scheme._

/**
  * S-expressions and related values
  */
sealed abstract class Value
case class ValueString(value: String) extends Value {
  override def toString: String = s""""$value""""
}
case class ValueSymbol(sym: String) extends Value {
  override def toString: String = s"'$sym"
}
case class ValueInteger(value: Int) extends Value {
  override def toString: String = value.toString
}
case class ValueReal(value: Double) extends Value {
  override def toString = f"$value%e".replace(",",".") // Might not preserve full precision, but will be in a Scheme-compatible format
}
case class ValueBoolean(value: Boolean) extends Value {
  override def toString: String = if (value) "#t" else "#f"
}
case class ValueCharacter(value: Char) extends Value {
  override def toString = s"#\\$value"
}
case object ValueNil extends Value {
  override def toString = "()"
}

/**
  * Abstract grammar elements for S-expressions include some positional
  * information. This serves two purposes: identify where the s-expression
  * resides in the input file, and as tagging information for the abstract
  * machine.
  */
trait SExp extends Expression {
  val idn: Identity
  def fv: Set[String] = Set()
}

/**
  * An s-expression is made of pairs, e.g., (foo bar) is represented as the pair
  * with identifier foo as car and another pair -- with identifier bar as car and
  * value nil as cdr -- as cdr. Pairs are pretty-printed when converted to
  * string. i.e., (foo bar) is stringified as (foo bar) and not (foo . (bar
  * . ()))
  */
case class SExpPair(car: SExp, cdr: SExp, idn: Identity) extends SExp {
  override def toString: String = {
    val content = toStringRest
    s"($content)"
  }
  def toStringRest: String =
    cdr match {
      case pair: SExpPair =>
        val rest = pair.toStringRest
        s"$car $rest"
      case SExpValue(ValueNil, _) => s"$car"
      case _                      => s"$car . $cdr"
    }
  val label: Label = PAI
  def subexpressions: List[Expression] = List(car, cdr)
}

object SExpList {
  /** Alternative constructor to automatically construct a bunch of pair from a
    * list of expressions */
  def apply(content: List[SExp], end: SExp): SExp = fromList(content, end)
  def apply(content: List[SExp], idn: Identity): SExp =
    fromList(content, SExpValue(ValueNil,idn))

  def fromList(content: List[SExp], end: SExp): SExp = content match {
    case Nil          => end
    case head :: tail => SExpPair(head, SExpList(tail, end), head.idn)
  }
}

/**
  * An identifier, such as foo, bar, etc.
  */
case class SExpId(id: Identifier) extends SExp {
  val idn: Identity = id.idn
  override def toString: String = id.toString
  val label: Label = SYM
  def subexpressions: List[Expression] = List(id)
}

/**
  * A literal value, such as 1, "foo", 'foo, etc.
  */
case class SExpValue(value: Value, idn: Identity) extends SExp {
  override def toString: String = value.toString
  val label: Label = VAL
  def subexpressions: List[Expression] = List()
  override lazy val hash: Int = (label, value).hashCode()
}

/**
  * A quoted element, such as 'foo, '(foo (bar)), etc.
  */
object SExpQuoted {
  def apply(content: SExp, idn: Identity): SExp =
    SExpList(List(SExpId(Identifier("quote",idn)), content), idn)
}

/**
  * A quasiquoted element, such as `foo
  */
object SExpQuasiquoted {
  def apply(content: SExp, idn: Identity): SExp =
    SExpList(List(SExpId(Identifier("quasiquote",idn)), content), idn)
}

/**
  * An unquoted element, such as ,foo
  */
object SExpUnquoted {
  def apply(content: SExp, idn: Identity): SExp =
    SExpList(List(SExpId(Identifier("unquote",idn)), content), idn)
}

/**
  * A unquoted-splicing element, such as ,@foo
  */
object SExpUnquotedSplicing {
  def apply(content: SExp, idn: Identity): SExp =
    SExpList(List(SExpId(Identifier("unquote-splicing",idn)), content), idn)
}
