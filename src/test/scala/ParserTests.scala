import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val files = Table(
    "file",
    "blur.scm",
    "church.scm",
    "count.scm",
    "cpstak.scm",
    "eta.scm",
    "fact.scm",
    "fib.scm",
    "gcipd.scm",
    "inc.scm",
    "infinite-1.scm",
    "infinite-2.scm",
    "kcfa2.scm",
    "kcfa3.scm",
    "letrec-begin.scm",
    "loop2.scm",
    "mj09.scm",
    "mut-rec.scm",
    "rotate.scm",
    "sq.scm",
    "sym.scm",
    "widen.scm"
  )

  property("parser should parse Scheme files without error") {
    forAll (files) { (file: String) =>
      Scheme.parse("test/" + file)
    }
  }

  val lexical = new SExpLexer

  val characters = Table("character", "#\\a", "#\\b", /* "#\\u03BB", "#\\Whitespace", */  "#\\λ")
  property("lexer should lex characters without error") {
    forAll(characters) { (character: String) =>
      lexical.character(new scala.util.parsing.input.CharArrayReader(character.toCharArray)) match {
        case lexical.Success(res, next) => assert(next.atEnd); assert(res.chars == character)
        case res => throw new Exception(s"Parse failure: $res")
      }
    }
  }

  val strings = Table("string", "\"foo\"", "\"foo\\\"bar\\\"foo\"", "\"foo\nbar\"")
  property("lexer should lex strings without error") {
    forAll(strings) { (string: String) =>
      lexical.string(new scala.util.parsing.input.CharArrayReader(string.toCharArray)) match {
        case lexical.Success(res, next) => assert(next.atEnd); assert(res.chars == string)
        case res => throw new Exception(s"Parse failure: $res")
      }
    }
  }
}
