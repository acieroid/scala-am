import org.scalatest._
import org.scalatest.prop._

import scalaam.language.sexp._
import scalaam.language.scheme.SchemeParser

class ParserSpec extends FlatSpec with Matchers {
  val benchmarkFiles = List("test/ad/abstrct.scm", "test/ad/bfirst.scm", "test/ad/bst.scm", "test/ad/btree.scm", "test/ad/bubsort.scm", "test/ad/dict.scm", "test/ad/dictExamples.scm", "test/ad/heap.scm", "test/ad/inssort.scm", "test/ad/linear.scm", "test/ad/list.scm", "test/ad/mesort.scm", "test/ad/prioq.scm", "test/ad/qsort.scm", "test/ad/qstand.scm", "test/ad/queue.scm", "test/ad/quick.scm", "test/ad/RBtreeADT.scm", "test/ad/selsort.scm", "test/ad/stack.scm", "test/ad/stspaceCODE.scm", "test/blur.scm", "test/bound-precision.scm", "test/church-2-num.scm", "test/church-6.scm", "test/church.scm", "test/collatz.scm", "test/count.scm", "test/eta.scm", "test/fact.scm", "test/fib.scm", "test/foo.scm", "test/gabriel/boyer.scm", "test/gabriel/cpstak.scm", "test/gabriel/dderiv.scm", "test/gabriel/deriv.scm", "test/gabriel/divrec.scm", "test/gabriel/takl.scm", "test/gambit/array1.scm", "test/gambit/browse.scm", "test/gambit/cat.scm", "test/gambit/compiler.scm", "test/gambit/ctak.scm", "test/gambit/deriv.scm", "test/gambit/destruc.scm", "test/gambit/diviter.scm", "test/gambit/earley.scm", "test/gambit/fibc.scm", "test/gambit/graphs.scm", "test/gambit/lattice.scm", "test/gambit/matrix.scm", "test/gambit/mazefun.scm", "test/gambit/nboyer.scm", "test/gambit/nqueens.scm", "test/gambit/paraffins.scm", "test/gambit/perm9.scm", "test/gambit/peval.scm", "test/gambit/primes.scm", "test/gambit/puzzle.scm", "test/gambit/sboyer.scm", "test/gambit/scheme.scm", "test/gambit/slatex.scm", "test/gambit/string.scm", "test/gambit/sum.scm", "test/gambit/sumloop.scm", "test/gambit/tail.scm", "test/gambit/tak.scm", "test/gambit/trav1.scm", "test/gambit/triangl.scm", "test/gambit/wc.scm", "test/gcipd.scm", "test/grid.scm", "test/inc.scm", "test/infinite-1.scm", "test/infinite-2.scm", "test/infinite-3.scm", "test/kcfa2.scm", "test/kcfa3.scm", "test/kernighanvanwyk/ack.scm", "test/letrec-begin.scm", "test/loop2.scm", "test/mceval.scm", "test/mj09.scm", "test/mut-rec.scm", "test/nested-defines.scm", "test/primtest.scm", "test/quasiquoting-simple.scm", "test/quasiquoting.scm", "test/regex.scm", "test/rosetta/easter.scm", "test/rosetta/quadratic.scm", "test/rotate.scm", "test/rsa.scm", "test/sat.scm", "test/scm2c.scm", "test/scm2java.scm", "test/scp1/2.1.scm", "test/scp1/2.4.scm", "test/scp1/3.1.scm", "test/scp1/3.2.1.scm", "test/scp1/3.2.scm", "test/scp1/3.3.scm", "test/scp1/3.4.scm", "test/scp1/3.6.scm", "test/scp1/3.8.scm", "test/scp1/3.9.scm", "test/scp1/4.1.scm", "test/scp1/4.8.scm", "test/scp1/5.14.3.scm", "test/scp1/5.19.scm", "test/scp1/5.20.4.scm", "test/scp1/5.21.scm", "test/scp1/5.22.scm", "test/scp1/5.6.scm", "test/scp1/5.7.scm", "test/scp1/7.11.scm", "test/scp1/7.12.scm", "test/scp1/7.13.scm", "test/scp1/7.14.scm", "test/scp1/7.15.scm", "test/scp1/7.16.scm", "test/scp1/7.17.scm", "test/scp1/7.18.scm", "test/scp1/7.2.scm", "test/scp1/7.3.scm", "test/scp1/7.4.scm", "test/scp1/7.5.scm", "test/scp1/7.6.scm", "test/scp1/7.9.scm", "test/scp1/8.1.1.scm", "test/scp1/8.1.3.scm", "test/scp1/8.10.scm", "test/scp1/8.11.scm", "test/scp1/8.12.scm", "test/scp1/8.13.scm", "test/scp1/8.14.scm", "test/scp1/8.15.scm", "test/scp1/8.16.scm", "test/scp1/8.5.scm", "test/scp1/8.6.scm", "test/scp1/9.12.scm", "test/scp1/9.13.scm", "test/scp1/9.14.scm", "test/scp1/9.15.scm", "test/scp1/9.16.scm", "test/scp1/9.17.scm", "test/scp1/9.18.scm", "test/scp1/9.2.scm", "test/scp1/9.3.scm", "test/scp1/9.5.scm", "test/scp1/9.6.scm", "test/scp1/9.7.scm", "test/scp1/9.8.scm", "test/scp1/9.9.scm", "test/SICP-compiler.scm", "test/sigscheme/arithint.scm", "test/sigscheme/case.scm", "test/sigscheme/let-loop.scm", "test/sigscheme/loop.scm", "test/sigscheme/mem.scm", "test/sigscheme/rec.scm", "test/sigscheme/takr.scm", "test/sq.scm", "test/Streams.scm", "test/sym.scm", "test/widen.scm", "test/work.scm")

  def fileContent(file: String): Option[String] = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    Option(content)
  }


  benchmarkFiles.foreach(bench =>
    fileContent(bench).foreach(content =>
      bench should "be parsed correctly" in {
        assert(SchemeParser.parse(content).toString.length > 0)
      }))
}

class LexerSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
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
  property("lexer should lex booleans without error") {
    forAll(bools) { s => check(lexical.boolean)(s); check(lexical.token)(s) }
  }

  val integers = Table("integer", "100", "-231")
  property("lexer should lex integers without error") {
    forAll(integers) { s =>
      check(lexical.integer)(s); check(lexical.token)(s) }
  }

  val reals = Table(("real", "output"),
    ("1.0", "1.0"), ("1e10", "1.0E10"), ("1e-5", "1.0E-5"), ("1.3e-5", "1.3E-5"),
    ("0.843", "0.843"), (".234", "0.234"), ("-.08", "-0.08"))
  property("lexer should lex reals without error") {
    forAll(reals) { (s, exp) => checkExpected(lexical.real)(s, exp); checkExpected(lexical.token)(s, exp) }
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
