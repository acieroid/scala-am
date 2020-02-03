package primitiveCompilation

case class Id(name: String) extends AnyVal

object Id {

  private var c = 0

  def genId(): Id = {
    val id = s"VAR${c.toString}"
    c = c + 1
    Id(id)
  }

}