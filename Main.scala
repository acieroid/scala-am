object Main {
  def main(args: Array[String]) {
    val parser = new SExpParser
    val exp = parser.parse("(1 2 (3 4))")
    /*
    val exp = new SExpPair(new SExpIdentifier("define"),
      new SExpPair(new SExpIdentifier("x"),
        new SExpPair(new SExpQuoted(new SExpPair(new SExpIdentifier("foo"),
          new SExpNil())),
          new SExpNil())))
     */
    println(exp)
  }
}
