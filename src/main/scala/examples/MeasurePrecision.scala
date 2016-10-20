object MeasurePrecision {
  def main(args: Array[String]) {
    val program = Util.fileContent("test/bound-precision.scm")
    /* Changing the bound to less than 100 decrease precision */
    val lattice = new BoundedIntLattice(1000, false)
    implicit val isLattice = lattice.isSchemeLattice
    val address = ClassicalAddress
    val timestamp = ZeroCFA
    implicit val isTimestamp = timestamp.isTimestamp
    val sem = new SchemeSemantics[lattice.L, address.A, timestamp.T](new SchemePrimitives[address.A, lattice.L])
    val machine = new AAMGlobalStore[SchemeExp, lattice.L, address.A, timestamp.T]
    val output = machine.eval(sem.parse(program), sem, false, None)
    val store = output.joinedStore
    println("Final store:")
    println(store)
    val cardinalities: Map[address.A, Cardinality] = store.cardinalities()
    val (inf: Int, unique: Int, others: List[Int]) = cardinalities.values.foldLeft((0, 0, List[Int]()))((acc, v) => v match {
      case CardinalityInf => (acc._1 + 1, acc._2, acc._3)
      case CardinalityNumber(1) => (acc._1, acc._2 + 1, acc._3)
      case CardinalityNumber(n) => (acc._1, acc._2, n :: acc._3)
    })
    println(s"Infinite cardinalities: $inf")
    println(s"Unique cardinalities: $unique")
    if (others.isEmpty) {
      println("No others")
    } else {
      val sum = others.foldLeft(0)(_ + _)
      val avg = sum.toDouble / others.length
      val median = others.sorted.apply(others.length / 2)
      println(s"Sum of others: $sum")
      println(s"Avg of others: $avg")
      println(s"Median of others: $median")
    }
  }
}
