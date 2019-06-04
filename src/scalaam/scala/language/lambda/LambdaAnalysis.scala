package scalaam.language.lambda

object LambdaAnalysis {
  def main(args: Array[String]) = {
    import scalaam.machine._
    import scalaam.core._
    import scalaam.graph._

    /* To run an analysis, we instanciate a number of components */

    /* We need a representation of memory addresses. We use a simple abstraction
     * that represent a memory for a variable by the name of the variable. As a
     * result, all variables that have the same name will be allocated at the
     * same memory address and their value will be joined. This is a
     * context-insensitive analysis. */
    val address = NameAddress

    /* We don't use timestamps, so we use ZeroCFA which is an empty timestamp. */
    val timestamp = ZeroCFA[LambdaExp]()
    /* We use our set lattice implementation defined in LambdaLattice.scala */
    val lattice = LambdaSetLattice[address.A]()
    /* We use the semantics we defined for lambda-calculus*/
    val sem = LambdaSemantics[lattice.L, address.A, timestamp.T, LambdaExp](address.Alloc[timestamp.T, LambdaExp])
    /* We use the AAM machine asbtraction, pre-defined in Scala-AM */
    val machine = new AAM[LambdaExp, address.A, lattice.L, timestamp.T](sem)
    /* This is the kind of graph we want to generate. In this case, we generate a
     * plain graph in the DOT format. Other possibilities include graph
     * representations that only store the information needed for a specific
     * client analysis, e.g. one that only stores error states in order to
     * statically analyze the program to locate possible errors that can be
     * reached. */
    val graph  = new DotGraph[machine.State, machine.Transition]
    /* We can then run our analysis on an example program */
    val result = machine.run[graph.G](LambdaParser.parse("((lambda (x) (lambda (y) y)) (lambda (z) z))"), Timeout.Infinity)
    /* We can then output our graph to a file to inspect it. */
    result.toFile("foo.dot")
  }
}
