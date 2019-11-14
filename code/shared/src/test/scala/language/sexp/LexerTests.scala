package scalaam.test.parser

import org.scalatest._
import org.scalatest.prop._

import scalaam.language.sexp._

class SExpLexerTests extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val lexical = new SExpLexer
  def checkExpected(parser: lexical.Parser[lexical.SExpToken])(input: String, expected: String) =
    parser(new scala.util.parsing.input.CharArrayReader(input.toCharArray)) match {
      case lexical.Success(res, next) =>
        if (!next.atEnd) { println(s"Parsed $res from $input, incorrect") }
        assert(next.atEnd); assert(res.chars == expected)
      case res => throw new Exception(s"Parse failure: $res")
    }
  def check(parser: lexical.Parser[lexical.SExpToken])(input: String) = checkExpected(parser)(input, input)

  val bools = Table("boolean", "#t","#f")
  property("SExpLexer should lex booleans without error") {
    forAll(bools) { s => check(lexical.boolean)(s); check(lexical.token)(s) }
  }

  val integers = Table("integer", "100", "-231")
  property("SExpLexer should lex integers without error") {
    forAll(integers) { s =>
      check(lexical.integer)(s); check(lexical.token)(s) }
  }

  val reals = Table(("real", "output"),
    ("1.0", "1.0"), ("1e10", "1.0E10"), ("1e-5", "1.0E-5"), ("1.3e-5", "1.3E-5"),
    ("0.843", "0.843"), (".234", "0.234"), ("-.08", "-0.08"))
  property("SExpLexer should lex reals without error") {
    forAll(reals) { (s, exp) => checkExpected(lexical.real)(s, exp); checkExpected(lexical.token)(s, exp) }
  }

  val characters = Table("character", "#\\a", "#\\b", /* "#\\u03BB", "#\\Whitespace", */  "#\\λ")
  property("SExpLexer should lex characters without error") {
    forAll(characters) { s => check(lexical.character)(s); check(lexical.token)(s) }
  }

  val strings = Table("string", "\"foo\"", "\"foo\\\"bar\\\"foo\"", "\"foo\nbar\"")
  property("SExpLexer should lex strings without error") {
    forAll(strings) { s => check(lexical.string)(s); check(lexical.token)(s) }
  }

  val identifiers = Table("identifier", "foo", "+", "1+", "1-", "<<test>>")
  property("SExpLexer should lex identifiers without error") {
    forAll(identifiers) { s => check(lexical.identifier)(s); check(lexical.token)(s) }
  }

  val specialTokens = Table("special token", "(", ")", ",", "`", "'", ",@", "#(")
  property("SExpLexer should lex special tokens without error") {
    forAll(specialTokens) { check(lexical.token) }
  }
}
