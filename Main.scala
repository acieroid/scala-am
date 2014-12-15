object Main {
  def main(args: Array[String]) {
    val parser = new SExpParser
    val exp = parser.parse("(and a (or b (and c d (or e f))))")
    /*
    val exp = new SExpPair(new SExpIdentifier("define"),
      new SExpPair(new SExpIdentifier("x"),
        new SExpPair(new SExpQuoted(new SExpPair(new SExpIdentifier("foo"),
          new SExpNil())),
          new SExpNil())))
     */
    println(exp)
    println(Scheme.compile(exp))
    println(Scheme.rename(Scheme.compile(exp)))
    println(ANF.compile(Scheme.rename(Scheme.compile(exp))))
  }
}
