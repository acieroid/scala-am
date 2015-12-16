/**
  * Implementation of a simple s-expression parser, which supports some
  * Scheme-like constructs. It however doesn't fully support any RnRS standard
  * syntax.
  */

/** NOTE: How the lexer/parser works and how to debug it
 *
 * The SExpTokens trait defines the tokens of the language. The chars field of
 * each token is the textual representation of the token.
 *

 * The SExpLexer class defines a bunch of lexers. Some of them are helper
 * lexers, used in other ones. Lexers of type Parser[Token] will lex one of our
 * token. All these lexers are then assembled into the 'token' function, which
 * will parse either one of them, ignoring whitespace and comments.

 * To test a lexer, one just has to apply it, providing a Reader[Char] as
 * argument. For example, to test the character lexer:
 *   val lexical = new SExpLexer
 *   println(lexical.character(new scala.util.parsing.input.CharArrayReader("#\c".toCharArray))

 * The SExpParser class defines parsers, similarly as SExpLexer. The difference
 * is that the parser works by assembling a bunch of tokens into grammar items,
 * whereas the lexer works by assembling a bunch of characters to tokens.

 * To test a parser, similarly to the lexers, one just has to apply it,
 * providing a Reader[Token] as argument (given by the Scanner class of the
 * lexer). For example, to test the 'nil' parser:
 *   val lexical = new SExpLexer
 *   println(SExpParser.nil(new SExpParser.lexical.Scanner("()"))

 * You may ask why is SExpLexer defined as a class, and SExpParser as an
 * object. The answer is simple: I don't know, but that's apparently the idiom
 * to use. SExpParser's lexical variable *needs* to be set to the lexer
 * used. Also, having SExpLexer as a separate class seems the only way to be
 * able to import it and test it outside this file.
 */

import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._

trait SExpTokens extends Tokens {
  trait SExpToken extends Token with Positional
  case class TIdentifier(s: String) extends SExpToken {
    def chars = s
  }
  case class TString(s: String) extends SExpToken {
    def chars = "\"" + s + "\""
  }
  case class TInteger(n: Integer) extends SExpToken {
    def chars = n.toString
  }
  case class TFloat(n: Float) extends SExpToken {
    def chars = n.toString
  }
  case class TBoolean(b: Boolean) extends SExpToken {
    def chars = b match {
      case true => "#t"
      case false => "#f"
    }
  }
  case class TCharacter(c: Character) extends SExpToken {
    def chars = s"#\\$c"
  }
  case class TQuote() extends SExpToken {
    def chars = "'"
  }
  case class TLeftParen() extends SExpToken {
    def chars = "("
  }
  case class TRightParen() extends SExpToken {
    def chars = ")"
  }
}

class SExpLexer extends Lexical with SExpTokens {
  def whitespace: Parser[String] = rep(whitespaceChar) ^^ (_.mkString)
  def eol: Parser[Any] = acceptIf(n => n == '\n')(n => "")
  def notEol: Parser[Char] = acceptIf(n => n != '\n')(n => "")
  def comment: Parser[String] = ';' ~> rep(notEol) <~ eol ^^ (_.mkString)
  def nonRelevant: Parser[Unit] = (comment ~ nonRelevant | whitespace) ^^ (_ => ())

  def any: Parser[Char] = chrExcept()
  def chr(c: Char): Parser[Char] = elem(s"character $c", _ == c)
  def sign: Parser[Option[Char]] = opt(chr('+') | chr('-'))
  def stringContent: Parser[String] = {
    ('\\' ~ any ~ stringContent ^^ { case '\\' ~ c ~ s => "\\$c$s" } ) |
    (rep(chrExcept('\"', '\n')) ^^ (_.mkString))
  }

  def bool: Parser[SExpToken] =
    '#' ~> ('t' ^^ (_ => TBoolean(true)) | 'f' ^^ (_ => TBoolean(false)))
  def integer: Parser[SExpToken] =
    sign ~ rep1(digit) ^^ { case s ~ n =>
                            s match {
                              case Some('+') => TInteger(n.mkString.toInt)
                              case Some('-') => TInteger(- n.mkString.toInt)
                              case _ => TInteger(n.mkString.toInt)
                            }
                          }
  def character: Parser[SExpToken] =
    '#' ~> '\\' ~> any ^^ (c => TCharacter(c))
  def stringEnding: Parser[String] = chrExcept('\\', '\n') ^^ (_.toString)
  def string: Parser[SExpToken] = {
    ('\"' ~> stringContent ~ chrExcept('\\', '\n') <~ '\"' ^^ { case s ~ ending => TString(s + ending) }) |
    ('\"' ~> stringContent <~ '\"' ^^ (s => TString(s)))
  }
  def identifier: Parser[SExpToken] =
    rep1(chrExcept('#', '\'', '\"', '(', ')', ' ', ';', '\n', '\t')) ^^ (s => TIdentifier(s.mkString))
  def quote: Parser[SExpToken] = chr('\'') ^^ { _ => TQuote() }
  def leftParen: Parser[SExpToken] = chr('(') ^^ { _ => TLeftParen() }
  def rightParen: Parser[SExpToken] = chr(')') ^^ { _ => TRightParen() }
  def float: Parser[SExpToken] =
    sign ~ rep(digit) ~ '.' ~ rep(digit) ^^ { case s ~ pre ~ _ ~ post =>
                                              val n = (pre.mkString + "." + post.mkString).toFloat
                                              s match {
                                                case Some('+') => TFloat(n)
                                                case Some('-') => TFloat(-n)
                                                case _ => TFloat(n)
                                              }
                                            }
  def token: Parser[SExpToken] =
    nonRelevant ~> positioned ({
      bool | float | integer | character | string | identifier |
      quote | leftParen | rightParen
    }) <~ nonRelevant
}

/* TODO: bug in the parser: it fails on multi-line comments separated by empty
 * lines. The lexer seems correct, because it correctly discards the comments
 * and empty lines before any token. I have no idea why the parser fails. */
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
  def nil: Parser[Value] = leftParen ~ rightParen ^^ (_ => ValueNil)

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

  def exp: Parser[SExp] = positioned(value | identifier | list | quoted)
  def expList: Parser[List[SExp]] = rep1(exp)

  def parse(s: String): List[SExp] = expList(new lexical.Scanner(s)) match {
    case Success(res, _) => res
    case Failure(msg, _) => throw new Exception("cannot parse expression: " + msg)
    case Error(msg, _) => throw new Exception("cannot parse expression: " + msg)
  }
}
