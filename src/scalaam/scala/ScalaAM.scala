package scalaam

object Main {
  def main(args: Array[String]) = {
    scheme()
  }

  def lambda() = {
    import scalaam.language.lambda._
    import scalaam.machine._
    import scalaam.core._
    import scalaam.graph._

    val address   = NameAddress
    val timestamp = ZeroCFA[LambdaExp]()
    val lattice   = LambdaSetLattice[address.A]()
    val sem = LambdaSemantics[lattice.L, address.A, timestamp.T, LambdaExp](
      address.Alloc[timestamp.T, LambdaExp])
    val machine = new AAM[LambdaExp, address.A, lattice.L, timestamp.T](sem)
    val graph   = DotGraph[machine.State, machine.Transition]
    val result = machine.run[graph.G](
      LambdaParser.parse("((lambda (x) (lambda (y) y)) (lambda (z) z))"),
      Timeout.Infinity)
    result.toFile("foo.dot")
  }

  def scheme() = {
    import scalaam.language.scheme._
    import scalaam.machine._
    import scalaam.core._
    import scalaam.graph._
    import scalaam.lattice._

    val address = NameAddress
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice = new MakeSchemeLattice[SchemeExp, address.A,
      Concrete.S, Concrete.B, Concrete.I, Concrete.R, Concrete.C, Concrete.Sym]
    val sem = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
      address.Alloc[timestamp.T, SchemeExp])
    val machine = new AAM[SchemeExp, address.A, lattice.L, timestamp.T](sem)
    val graph = DotGraph[machine.State, machine.Transition]
    val result = machine.run[graph.G](
      SchemeParser.parse("(equal? 1 2)"),
      Timeout.Infinity)
    result.toFile("foo.dot")
  }
}
