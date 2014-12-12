import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

trait SExpTokens extends Tokens {
  case class TSymbol(s: String) extends Token {
    def chars = s
  }
  case class TString(s: String) extends Token {
    def chars = s
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
  def whitespace: Parser[Any] = rep(whitespaceChar)
  def eol: Parser[Any] = acceptIf(n => n == '\n')(n => "")
  def notEol: Parser[Char] = acceptIf(n => n != '\n')(n => "")
  def comment: Parser[String] = ';' ~> rep(notEol) <~ eol ^^ (_.mkString)
  def bool: Parser[Token] =
    '#' ~> ('t' ^^ (_ => TBoolean(true)) | 'f' ^^ (_ => TBoolean(false)))
  def token: Parser[Token] = bool
}

class SExpParser extends TokenParsers {
  type Tokens = SExpTokens
  override val lexical = new SExpLexer
  import lexical._

  def bool: Parser[SExp] = elem("boolean", _.isInstanceOf[TBoolean]) ^^ {case TBoolean(b) => new SExpBoolean(b)}

  def exp: Parser[SExp] = bool

  def parse(s: String): SExp =
    exp(new lexical.Scanner(s)) match {
      case Success(res, _) => res
      case Failure(msg, _) => throw new Exception("cannot parse expression: " + msg)
      case Error(msg, _) => throw new Exception("cannot parse expression: " + msg)
    }
}
