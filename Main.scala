object Main {
  def main(args: Array[String]) {
    val parser = new SExpParser
    val exp = parser.parse("(define (foo x) (+ x 1) (* x 2))")
    /*
    val exp = new SExpPair(new SExpIdentifier("define"),
      new SExpPair(new SExpIdentifier("x"),
        new SExpPair(new SExpQuoted(new SExpPair(new SExpIdentifier("foo"),
          new SExpNil())),
          new SExpNil())))
     */
    println(exp)
    println(Scheme.compile(exp))
  }
}
