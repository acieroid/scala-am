/**
  *  Implementation of a simple s-expression parser, which supports some
  *  Scheme-like constructs. It however doesn't fully support any RnRS standard
  *  syntax.
  */

import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

trait SExpTokens extends Tokens {
  case class TSymbol(s: String) extends Token {
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
  def chr(c: Char): Parser[Char] = elem("character $c", _ == c)
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
  def string: Parser[Token] =
    /* TODO: This one does not seem completely right. It lexes too much, eg. "foo\"
     * which is a non-terminated string. */
    '\"' ~> stringContent <~ '\"' ^^ (s => TString(s))
  def quote: Parser[Token] = chr('\'') ^^ { _ => TQuote() }
  def leftParen: Parser[Token] = chr('(') ^^ { _ => TLeftParen() }
  def rightParen: Parser[Token] = chr(')') ^^ { _ => TRightParen() }
  /* TODO: float */
  def token: Parser[Token] = whitespace ~> bool | integer | character | string | quote | leftParen | rightParen <~ whitespace
}

class SExpParser extends TokenParsers {
  type Tokens = SExpTokens
  override val lexical = new SExpLexer
  import lexical._

  def bool: Parser[SExp] = elem("boolean", _.isInstanceOf[TBoolean]) ^^ { case TBoolean(b) => SExpBoolean(b) }
  def integer: Parser[SExp] = elem("integer", _.isInstanceOf[TInteger]) ^^ { case TInteger(n) => SExpInteger(n) }
  def character: Parser[SExp] = elem("character", _.isInstanceOf[TCharacter]) ^^ { case TCharacter(c) => SExpCharacter(c) }
  def string: Parser[SExp] = elem("string", _.isInstanceOf[TString]) ^^ { case TString(s) => SExpString(s) }

  def leftParen = elem("left parenthesis", _.isInstanceOf[TLeftParen])
  def rightParen = elem("right parenthesis", _.isInstanceOf[TRightParen])
  def nil: Parser[SExp] = leftParen ~ rightParen ^^ (_ => SExpNil())
  def list: Parser[SExp] = (leftParen ~> rep1(exp) <~ rightParen) ^^ (e => SExpPair(e))


  def exp: Parser[SExp] = bool | integer | character | string | list | nil

  def parse(s: String): SExp =
    exp(new lexical.Scanner(s)) match {
      case Success(res, _) => res
      case Failure(msg, _) => throw new Exception("cannot parse expression: " + msg)
      case Error(msg, _) => throw new Exception("cannot parse expression: " + msg)
    }
}
