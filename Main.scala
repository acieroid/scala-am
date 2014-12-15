object Main {
  def main(args: Array[String]) {
    val parser = new SExpParser
    val exp = parser.parse("(let ((x 1)) (let ((x 2)) (lambda (x) x) x) x)")
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
  }
}
