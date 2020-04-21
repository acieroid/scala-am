package scalaam.language.sexp

import scalaam.core.Position._
import scalaam.core._

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
    def chars = s""""$s""""
  }
  case class TInteger(n: Int) extends SExpToken {
    def chars = n.toString
  }
  case class TReal(n: Double) extends SExpToken {
    def chars = n.toString
  }
  case class TBoolean(b: Boolean) extends SExpToken {
    def chars =
      if (b) {
        "#t"
      } else {
        "#f"
      }
  }
  case class TCharacter(c: Char) extends SExpToken {
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
  case class THashParen() extends SExpToken {
    def chars = "#("
  }
  case class TBackquote() extends SExpToken {
    def chars = "`"
  }
  case class TUnquote() extends SExpToken {
    def chars = ","
  }
  case class TUnquoteSplicing() extends SExpToken {
    def chars = ",@"
  }
  case class TDot() extends SExpToken {
    def chars = "."
  }
}

class SExpLexer extends Lexical with SExpTokens {
  def whitespace: Parser[String] = rep(whitespaceChar) ^^ (_.mkString)
  def eoi: Parser[Any] = new Parser[Any] {
    def apply(in: Input) = {
      if (in.atEnd) new Success("EOI", in)
      else Failure("End of Input expected", in)
    }
  }
  def eol: Parser[Any]          = acceptIf(n => n == '\n')(n => "")
  def notEol: Parser[Char]      = acceptIf(n => n != '\n')(n => "")
  def comment: Parser[String]   = ';' ~> rep(notEol) <~ eol ^^ (_.mkString)
  def nonRelevant: Parser[Unit] = rep(comment | whitespaceChar | eol) ^^ (_ => ())

  def any: Parser[Char]          = chrExcept()
  def chr(c: Char): Parser[Char] = elem(s"character $c", _ == c)
  def sign: Parser[Option[Char]] = opt(chr('+') | chr('-'))
  def stringContentNoEscape: Parser[String] =
    rep(chrExcept('\\', '"')) ^^ (_.mkString)
  def stringContent: Parser[String] = {
    (stringContentNoEscape ~ '\\' ~ any ~ stringContent ^^ {
      case s1 ~ '\\' ~ c ~ s2 => s"$s1\\$c$s2"
    }) |
      stringContentNoEscape
  }

  // R5RS: Tokens which require implicit termination (identifiers, numbers, characters, and dot) may be terminated by any <delimiter>, but not necessarily by anything else.  */
  def delimiter: Parser[Unit] =
    (whitespaceChar | eol | eoi | chr('(') | chr(')') | chr('"') | chr(';')) ^^ (_ => ())

  def boolean: Parser[SExpToken] =
    '#' ~> ('t' ^^^ TBoolean(true) | 'f' ^^^ TBoolean(false))
  def integer: Parser[SExpToken] =
    sign ~ rep1(digit) <~ guard(delimiter) ^^ {
      case s ~ n =>
        s match {
          case Some('+') => TInteger(n.mkString.toInt)
          case Some('-') => TInteger(-n.mkString.toInt)
          case _         => TInteger(n.mkString.toInt)
        }
    }
  def character: Parser[SExpToken] =
    (('#' ~> '\\' ~> any ^^ (c => TCharacter(c))) |
      ('#' ~> '\\' ~> 's' ~> 'p' ~> 'a' ~> 'c' ~> 'e' ^^^ TCharacter(' ')) |
      ('#' ~> '\\' ~> 'n' ~> 'e' ~> 'w' ~> 'l' ~> 'i' ~> 'n' ~> 'e' ^^^ TCharacter('\n')) |
      ('#' ~> '\\' ~> 't' ~> 'a' ~> 'b' ^^^ TCharacter('\t'))) // Not present in R5RS Scheme.
  def string: Parser[SExpToken] = {
    ('"' ~> stringContent ~ chrExcept('\\') <~ '"' ^^ { case s ~ ending => TString(s + ending) }) |
      ('"' ~> stringContent <~ '"' ^^ (s => TString(s)))
  }
  def identifier: Parser[SExpToken] = {
    def specialInitial: Parser[Char] =
      (chr('!') | chr('$') | chr('%') | chr('&') | chr('*') | chr('/') | chr(':') | chr('<') | chr(
        '='
      ) | chr('>') | chr('?') | chr('^') | chr('_') | chr('~')) ^^ (x => x)
    def initial: Parser[Char]              = letter | specialInitial
    def specialSubsequent: Parser[Char]    = chr('+') | chr('-') | chr('.') | chr('@')
    def subsequent: Parser[Char]           = initial | digit | specialSubsequent
    def peculiarIdentifier: Parser[String] =
      // R5RS specifies + | - | ..., not clear what ... is supposed to be
      ((chr('+') | chr('-')) ^^ (_.toString)) |
        ((chr('1') ~ chr('+') | chr('1') ~ chr('-')) ^^ { case c1 ~ c2 => s"$c1$c2" })
    (initial ~ rep(subsequent) ^^ { case i ~ s => s"$i${s.mkString}" }
      | peculiarIdentifier) <~ guard(delimiter) ^^ (s => TIdentifier(s))
  }
  def leftParen: Parser[SExpToken]       = chr('(') ^^^ TLeftParen()
  def rightParen: Parser[SExpToken]      = chr(')') ^^^ TRightParen()
  def hashParen: Parser[SExpToken]       = chr('#') ~ chr('(') ^^^ THashParen()
  def quote: Parser[SExpToken]           = chr('\'') ^^^ TQuote()
  def backquote: Parser[SExpToken]       = chr('`') ^^^ TBackquote()
  def unquote: Parser[SExpToken]         = chr(',') ^^^ TUnquote()
  def unquoteSplicing: Parser[SExpToken] = chr(',') ~ chr('@') ^^^ TUnquoteSplicing()
  def dot: Parser[SExpToken]             = chr('.') <~ guard(delimiter) ^^^ TDot()
  def real: Parser[SExpToken] =
    sign ~ rep(digit) ~ opt('.' ~ rep1(digit)) ~ opt('e' ~ integer) <~ guard(delimiter) ^? {
      case s ~ pre ~ post ~ exp if (exp.isDefined || post.isDefined) =>
        val signstr = s.map(_.toString).getOrElse("")
        val poststr = post.map({ case _ ~ digits => s".${digits.mkString}" }).getOrElse("")
        val expstr = exp
          .map({
            case _ ~ TInteger(n) => s"e$n"
            case _               => throw new Exception(s"cannot parse real ($exp)")
          })
          .getOrElse("")
        val n = s"$signstr${pre.mkString}$poststr$expstr"
        TReal(n.toDouble)
    }
  def number: Parser[SExpToken] = real | integer
  def token: Parser[SExpToken] =
    nonRelevant ~> positioned({
      boolean | number | identifier |
        character | string |
        leftParen | rightParen | hashParen | quote | backquote |
        unquoteSplicing | unquote | dot
    }) <~ nonRelevant
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
  def real: Parser[Value] = elem("real", _.isInstanceOf[TReal]) ^^ {
    case TReal(n) => ValueReal(n)
  }
  def character: Parser[Value] = elem("character", _.isInstanceOf[TCharacter]) ^^ {
    case TCharacter(c) => ValueCharacter(c)
  }
  def string: Parser[Value] = elem("string", _.isInstanceOf[TString]) ^^ {
    case TString(s) => ValueString(s)
  }
  def nil: Parser[Value] = leftParen ~ rightParen ^^^ ValueNil

  def value(tag: PTag): Parser[SExp] = Parser { in =>
    (bool | real | integer | character | string | nil)(in) match {
      case Success(t, in1) => Success(SExpValue(t, Identity(in.pos, tag)), in1)
      case ns: NoSuccess   => ns
    }
  }

  def identifier(tag: PTag): Parser[SExp] = Parser { in =>
    elem("identifier", _.isInstanceOf[TIdentifier])(in) match {
      case Success(TIdentifier(s), in1) => Success(SExpId(Identifier(s, Identity(in.pos, tag))), in1)
      case Success(v, in1)              => Failure(s"Expected identifier, got $v", in1)
      case ns: NoSuccess                => ns
    }
  }

  def leftParen       = elem("left parenthesis",  _.isInstanceOf[TLeftParen])
  def rightParen      = elem("right parenthesis", _.isInstanceOf[TRightParen])
  def dot             = elem("dot",               _.isInstanceOf[TDot])
  def quote           = elem("quote",             _.isInstanceOf[TQuote])
  def quasiquote      = elem("quasiquote",        _.isInstanceOf[TBackquote])
  def unquote         = elem("unquote",           _.isInstanceOf[TUnquote])
  def unquoteSplicing = elem("unquote-splicing",  _.isInstanceOf[TUnquoteSplicing])

  def list(tag: PTag): Parser[SExp] = Parser { in =>
    (leftParen ~> rep1(exp(tag)) ~ opt(dot ~> exp(tag)) <~ rightParen)(in) match {
      case Success(es ~ None, in1) =>
        Success(SExpList(es, Identity(in.pos, tag)), in1)
      case Success(es ~ Some(tail), in1) =>
        Success(SExpList(es, tail), in1)
      case ns: NoSuccess => ns
    }
  }

  def withQuote(tag: PTag)(p: Parser[_], make: (SExp, Identity) => SExp) = Parser { in =>
    (p ~> exp(tag))(in) match {
      case Success(e, in1) => Success(make(e, Identity(in.pos, tag)), in1)
      case ns: NoSuccess   => ns
    }
  }

  def quoted(tag: PTag)           : Parser[SExp] = withQuote(tag)(quote,           SExpQuoted(_,_))
  def quasiquoted(tag: PTag)      : Parser[SExp] = withQuote(tag)(quasiquote,      SExpQuasiquoted(_,_))
  def unquoted(tag: PTag)         : Parser[SExp] = withQuote(tag)(unquote,         SExpUnquoted(_,_))
  def unquotedSplicing(tag: PTag) : Parser[SExp] = withQuote(tag)(unquoteSplicing, SExpUnquotedSplicing(_,_))

  def exp(tag: PTag)     : Parser[SExp]       = value(tag) | identifier(tag) | list(tag) | quoted(tag) | quasiquoted(tag) | unquoted(tag) | unquotedSplicing(tag)
  def expList(tag: PTag) : Parser[List[SExp]] = rep1(exp(tag))

  def parse(s: String, tag: PTag = noTag): List[SExp] = expList(tag)(new lexical.Scanner(s)) match {
    case Success(res, next) if next.atEnd => res
    case Success(res, next) if !next.atEnd =>
      throw new Exception(
        s"cannot fully parse expression, stopped at ${next.pos} after parsing $res"
      )
    case Failure(msg, next) => throw new Exception(s"cannot parse expression: $msg, at ${next.pos}")
    case Error(msg, next)   => throw new Exception(s"cannot parse expression: $msg, at ${next.pos}")
  }
}
