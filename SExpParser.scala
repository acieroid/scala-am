/**
  * Implementation of a simple s-expression parser, which supports some
  * Scheme-like constructs. It however doesn't fully support any RnRS standard
  * syntax.
  */

import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

trait SExpTokens extends Tokens {
  case class TIdentifier(s: String) extends Token {
    def chars = s
  }
  case class TString(s: String) extends Token {
    def chars = "\"" + s + "\""
  }
  case class TInteger(n: Integer) extends Token {
    def chars = n.toString
  }
  case class TFloat(n: Float) extends Token {
    def chars = n.toString
  }
  case class TBoolean(b: Boolean) extends Token {
    def chars = b match {
      case true => "#t"
      case false => "#f"
    }
  }
  case class TCharacter(c: Character) extends Token {
    def chars = s"#\\$c"
  }
  case class TQuote() extends Token {
    def chars = "'"
  }
  case class TLeftParen() extends Token {
    def chars = "("
  }
  case class TRightParen() extends Token {
    def chars = ")"
  }
}

class SExpLexer extends Lexical with SExpTokens {
  def whitespace: Parser[String] = rep(whitespaceChar) ^^ (_.mkString)
  def eol: Parser[Any] = acceptIf(n => n == '\n')(n => "")
  def notEol: Parser[Char] = acceptIf(n => n != '\n')(n => "")
  def comment: Parser[String] = ';' ~> rep(notEol) <~ eol ^^ (_.mkString)
  def nonRelevant: Parser[Unit] = whitespace ^^ (_ => ()) | comment ^^ (_ => ())

  def any: Parser[Char] = chrExcept()
  def chr(c: Char): Parser[Char] = elem(s"character $c", _ == c)
  def sign: Parser[Option[Char]] = opt(chr('+') | chr('-'))
  def stringContent: Parser[String] = {
    ('\\' ~ any ~ stringContent ^^ { case '\\' ~ c ~ s => "\\$c$s" } ) |
    (rep(chrExcept('\"', '\n')) ^^ (_.mkString))
  }

  def bool: Parser[Token] =
    '#' ~> ('t' ^^ (_ => TBoolean(true)) | 'f' ^^ (_ => TBoolean(false)))
  def integer: Parser[Token] =
    sign ~ rep1(digit) ^^ { case s ~ n =>
                            s match {
                              case Some('+') => TInteger(n.mkString.toInt)
                              case Some('-') => TInteger(- n.mkString.toInt)
                              case _ => TInteger(n.mkString.toInt)
                            }
                          }
  def character: Parser[Token] =
    '#' ~> '\\' ~> any ^^ (c => TCharacter(c))
  def stringEnding: Parser[String] = chrExcept('\\', '\n') ^^ (_.toString)
  def string: Parser[Token] = {
    ('\"' ~> stringContent ~ chrExcept('\\', '\n') <~ '\"' ^^ { case s ~ ending => TString(s + ending) }) |
    ('\"' ~> stringContent <~ '\"' ^^ (s => TString(s)))
  }
  def identifier: Parser[Token] =
    rep1(chrExcept('#', '\'', '\"', '(', ')', ' ', ';', '\n', '\t')) ^^ (s => TIdentifier(s.mkString))
  def quote: Parser[Token] = chr('\'') ^^ { _ => TQuote() }
  def leftParen: Parser[Token] = chr('(') ^^ { _ => TLeftParen() }
  def rightParen: Parser[Token] = chr(')') ^^ { _ => TRightParen() }
  def float: Parser[Token] =
    sign ~ rep(digit) ~ '.' ~ rep(digit) ^^ { case s ~ pre ~ _ ~ post =>
                                              val n = (pre.mkString + "." + post.mkString).toFloat
                                              s match {
                                                case Some('+') => TFloat(n)
                                                case Some('-') => TFloat(-n)
                                                case _ => TFloat(n)
                                              }
                                            }
  def token: Parser[Token] =
    (whitespace | comment) ~> {
      bool | float | integer | character | string | identifier |
      quote | leftParen | rightParen
    } <~ (whitespace | comment)
}

object SExpParser extends TokenParsers {
  type Tokens = SExpTokens
  override val lexical = new SExpLexer
  import lexical._

  def bool: Parser[Value] = elem("boolean", _.isInstanceOf[TBoolean]) ^^ {
    case TBoolean(b) => ValueBoolean(b)
  }
  def integer: Parser[Value] = elem("integer", _.isInstanceOf[TInteger]) ^^ {
    case TInteger(n) => ValueInteger(n)
  }
  def float: Parser[Value] = elem("float", _.isInstanceOf[TFloat]) ^^ {
    case TFloat(n) => ValueFloat(n)
  }
  def character: Parser[Value] = elem("character", _.isInstanceOf[TCharacter]) ^^ {
    case TCharacter(c) => ValueCharacter(c)
  }
  def string: Parser[Value] = elem("string", _.isInstanceOf[TString]) ^^ {
    case TString(s) => ValueString(s)
  }
  def nil: Parser[Value] = leftParen ~ rightParen ^^ (_ => ValueNil())

  def value: Parser[SExp] =
    (bool | float | integer | character | string | nil) ^^ (v => SExpValue(v))

  def identifier: Parser[SExp] = elem("identifier", _.isInstanceOf[TIdentifier]) ^^ {
    case TIdentifier(s) => SExpIdentifier(s)
  }

  def leftParen = elem("left parenthesis", _.isInstanceOf[TLeftParen])
  def rightParen = elem("right parenthesis", _.isInstanceOf[TRightParen])
  def quote = elem("quote", _.isInstanceOf[TQuote])
  def list: Parser[SExp] =
    leftParen ~> rep1(exp) <~ rightParen ^^ (e => SExpPair(e))
  def quoted: Parser[SExp] = quote ~> exp ^^ (e => SExpQuoted(e))

  def exp: Parser[SExp] = value | identifier | list | quoted

  def parse(s: String): SExp = exp(new lexical.Scanner(s)) match {
    case Success(res, _) => res
    case Failure(msg, _) => throw new Exception("cannot parse expression: " + msg)
    case Error(msg, _) => throw new Exception("cannot parse expression: " + msg)
  }

  def test(s: String) = float(new lexical.Scanner(s))
}
