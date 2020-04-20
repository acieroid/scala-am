package scalaam.test.parser

import scalaam.core._
import scalaam.io.Reader
import scalaam.test._
import scalaam.language.scheme._

trait SchemeParserTests extends SchemeBenchmarkTests {
  def onBenchmark(benchmark: Benchmark) =
    property(s"SchemeParser can correctly parse $benchmark") {
      val content = Reader.loadFile(benchmark)
      val parsed = SchemeParser.parse(content)
      // Check that the parsing was succesful
      assert(parsed.toString.nonEmpty)
      // Check that printing and parsing the result again gives the same result
      val printed = parsed.toString
      val reparsed = SchemeParser.parse(printed, Position.newTag("Scala-AM"))
      assert(parsed.toString == reparsed.toString,
        "Printing and parsing again gives a result different from the original parse")
      assert(reparsed.subexpressions.forall(e => e.idn.pos.tag.contains("Scala-AM") || e.idn == NoCodeIdentity && e.idn.pos.tag.isEmpty))
    }
}

// class SimpleSchemeParserTests extends SchemeParserTests with SimpleBenchmarks

class ExtendedSchemeParserTests extends SchemeParserTests with AllBenchmarks
