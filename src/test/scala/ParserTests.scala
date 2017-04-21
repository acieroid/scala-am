import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends FlatSpec with Matchers with BenchmarkFiles {
  benchmarks(SchemeLattices.WithCounting(true).ConcreteLattice).foreach(bench =>
      Util.fileContent(bench.file).foreach(content =>
        Scheme.parse(content).toString))
}

class LexerSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val lexical = new SExpLexer
  def check(parser: lexical.Parser[lexical.SExpToken])(input: String) =
    parser(new scala.util.parsing.input.CharArrayReader(input.toCharArray)) match {
      case lexical.Success(res, next) =>
        if (!next.atEnd) { println(s"Parsed $res from $input, incorrect") }
        assert(next.atEnd); assert(res.chars == input)
      case res => throw new Exception(s"Parse failure: $res")
    }

  val bools = Table("boolean", "#t","#f")
  property("lexer should lex booleans without error") {
    forAll(bools) { s => check(lexical.boolean)(s); check(lexical.token)(s) }
  }

  val integers = Table("integer", "100", "-231")
  property("lexer should lex integers without error") {
    forAll(integers) { s =>
      check(lexical.integer)(s); check(lexical.token)(s) }
  }

  val floats = Table("floats", "1.0", /* "1e10", "1e-5", */ "0.843" /* ".234", "-.08" */)
  property("lexer should lex floats without error") {
    forAll(floats) { s => check(lexical.float)(s); check(lexical.token)(s) }
  }

  val characters = Table("character", "#\\a", "#\\b", /* "#\\u03BB", "#\\Whitespace", */  "#\\λ")
  property("lexer should lex characters without error") {
    forAll(characters) { s => check(lexical.character)(s); check(lexical.token)(s) }
  }

  val strings = Table("string", "\"foo\"", "\"foo\\\"bar\\\"foo\"", "\"foo\nbar\"")
  property("lexer should lex strings without error") {
    forAll(strings) { s => check(lexical.string)(s); check(lexical.token)(s) }
  }

  val identifiers = Table("identifier", "foo", "+", "1+", "1-")
  property("lexer should lex identifiers without error") {
    forAll(identifiers) { s => check(lexical.identifier)(s); check(lexical.token)(s) }
  }

  val specialTokens = Table("special token", "'", "(", ")")
  property("lexer should lex special tokens without error") {
    forAll(specialTokens) { check(lexical.token) }
  }
}
