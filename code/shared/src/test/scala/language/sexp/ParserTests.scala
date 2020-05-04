package scalaam.test.parser

import scalaam.util._
import scalaam.test._
import scalaam.language.sexp._

trait SExpParserTests extends SchemeBenchmarkTests {
  def onBenchmark(benchmark: Benchmark) =
    property(s"SExpParser can correctly parse $benchmark") {
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

class SimpleSExpParserTests extends SExpParserTests
                               with SimpleBenchmarks
