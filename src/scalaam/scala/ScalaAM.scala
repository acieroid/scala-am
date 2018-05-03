package scalaam

object Main {
  def main(args: Array[String]) {
    import scalaam.language.lambda._
    import scalaam.machine._
    import scalaam.core._
    import scalaam.graph._

    type A = NameAddress
    val timestamp = ZeroCFA[Unit]()
    val allocator = NameAllocator[timestamp.T, Unit]()(timestamp.typeclass)
    val lattice = LambdaSetLattice[A]()
    val sem = LambdaSemantics[lattice.L, A, timestamp.T, Unit](allocator)(timestamp.typeclass, lattice.typeclass)
    val machine = new AAM[LambdaExp, lattice.L, A, timestamp.T, Unit](sem)(timestamp.typeclass, lattice.typeclass)
    val graph = DotGraph.empty[machine.State, machine.Transition]
    val result = machine.run(LambdaParser.parse("((lambda (x) (lambda (y) y)) (lambda (z) z))"), graph, Timeout.Infinity)
    result.asInstanceOf[DotGraph[machine.State, machine.Transition]].toFile("foo.dot")
    println(result)
  }
}
