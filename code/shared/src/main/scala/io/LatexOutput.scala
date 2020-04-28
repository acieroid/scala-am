package scalaam.io

object LatexOutput {

  /**
   * Creates a string representing a latex table from a given dataset.
   * @param data          The dataset to be converted to a table. The first keys should correspond to the line names and the second keys should correspond to the column names.
   * @param rowName       The meaning of the row names. Can be the empty string.
   * @param rows          The row names. The order of the elements in the list will also be the order in which the rows will appear in the table.
   * @param columns       The column names. The order of the elements in the list will also be the order in which the columns will appear in the table.
   * @param defaultValue  The value to be put at positions in the table for which 'data' does not contain a value.
   * @tparam V            The type of values to be put in the table. The string representation of these values will be put in the table.
   * @return              A string representing a latex table, containing the values of the given dataset for the given values of 'rows' and 'columns'.
   */
  def table[V](data: Map[String, Map[String, V]], rowName: String, rows: List[String], columns: List[String], defaultValue: String): String = {
    var output: String = "\\bottomrule\n\\end{tabular}\n\\end{table}"
    for (row <- rows.reverse) {
      val values = columns.map(data(row).withDefaultValue(defaultValue))
      output = row ++ " & " ++ values.mkString(" & ") ++ "\\\\\n" ++ output
    }
    (s"\\begin{table}\n\\center\n\\begin{tabular}{l${"c" * columns.length}}\n\\toprule\n" ++
      rowName ++ " & " ++ columns.mkString(" & ") ++ "\\\\\n" ++ output).replace("_", "\\_")
  }

  def table[V](data: Map[String, Map[String, V]], rowName: String, rows: List[String], columns: List[String], defaultValue: V): String =
    table(data, rowName, rows, columns, defaultValue.toString)

}
