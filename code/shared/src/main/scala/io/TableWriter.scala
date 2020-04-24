package scalaam.io

object TableWriter {

  def writeTable[V](data: Map[String, Map[String, V]], rowName: String, rows: List[String], columns: List[String], defaultValue: V): String = {
    var output: String = ""
    for (row <- rows) {
      val values = columns.map(data(row).withDefaultValue(defaultValue))
      output = row ++ " " ++ values.mkString(" & ") ++ "\\\\\\\\\n" ++ output
    }
    rowName ++ " " ++ columns.mkString(" & ") ++ "\\\\\\\\\n" ++ output
  }

}
