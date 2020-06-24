package scalaam.test.tag

import org.scalatest.Tag

// Tags by non-functional test characteristics.
object SlowTest         extends Tag("SlowTest")

// Tags by function.
object ParserTest       extends Tag("ParserTest")
object LatticeTest      extends Tag("LatticeTest")
object PrimitiveTest    extends Tag("PrimitiveTest")
object SoundnessTest    extends Tag("SoundnessTest")

// Tags by language.
object SchemeTest       extends Tag("SchemeTest")
object CSchemeTest      extends Tag("CSchemeTest")