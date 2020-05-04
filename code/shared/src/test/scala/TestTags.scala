package scalaam.test.tag

import org.scalatest.Tag

// tags by non-functional test characteristics
object SlowTest         extends Tag("SlowTest")

// tags by function
object ParserTest       extends Tag("ParserTest")
object LatticeTest      extends Tag("LatticeTest")
object PrimitiveTest    extends Tag("PrimitiveTest")
object SoundnessTest    extends Tag("SoundnessTest")

// TODO: tags by language 
//object SchemeTest       extends Tag("SchemeTest")