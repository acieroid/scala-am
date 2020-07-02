package scalaam.test.language.sexp

import scalaam.language.sexp._
import scalaam.test._
import scalaam.util._

trait SExpParserTestsSpec extends SchemeBenchmarkTests {
  def onBenchmark(benchmark: Benchmark) =
    property(s"SExpParser can correctly parse $benchmark", ParserTest) {
      val content = Reader.loadFile(benchmark)
      val parsed = SExpParser.parse(content)
      // Check that the parsing was succesful
      assert(parsed.mkString("").nonEmpty)
      // Check that printing and parsing the result again gives the same result
      val printed = parsed.mkString("")
      val reparsed = SExpParser.parse(printed)
      assert(parsed.mkString("") == reparsed.mkString(""),
        "Printing and parsing again gives a result different from the original parse")
    }
}

class SExpParserTests extends SExpParserTestsSpec with SequentialBenchmarks