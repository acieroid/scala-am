package scalaam

object Main {
  def main(args: Array[String]) {
    import scalaam.language.lambda._
    import scalaam.machine._
    import scalaam.core._
    import scalaam.graph._

    val address = NameAddress
    val timestamp = ZeroCFA[LambdaExp]()
    val lattice = LambdaSetLattice[address.A]()
    val sem = LambdaSemantics[lattice.L, address.A, timestamp.T, LambdaExp](address.Alloc[timestamp.T, LambdaExp])
    val machine = new AAM[LambdaExp, address.A, lattice.L, timestamp.T](sem)
    val graph = DotGraph.empty[machine.State, machine.Transition]
    val result = machine.run(LambdaParser.parse("((lambda (x) (lambda (y) y)) (lambda (z) z))"), graph, Timeout.Infinity)
    result.asInstanceOf[DotGraph[machine.State, machine.Transition]].toFile("foo.dot")
    println(result)
  }
}
