object ANFConverter {
  def main(args: Array[String]) = {
    val f = args(0)
    Util.fileContent(f).foreach(p => println(ANF.parse(p)))
  }
}
