package scalaam.cli

object Main {

  import scalaam.language.scheme._

  def main(args: Array[String]): Unit = {
    println("boe")
  }

  def testLexer(file: String) = {
    val txt = loadFile(file)
    val prg = SchemeParser.parse(txt)
    val bds = List("+","-","*","/","=","<","<=",">",">=","modulo","cons","car","cdr",
                   "eq?","assq","pair?","set-car!","set-cdr!","cadr","error","null?",
                   "not","equal?","member","caddr","cadddr")
    val lex = SchemeLexicalAddresser.translateProgram(prg,bds)
    println(lex)
  }

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }
}
