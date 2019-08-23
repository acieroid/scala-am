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
    val church = """
(letrec ((true (lambda (x y) x))
         (false (lambda (x y) y))
         (plus (lambda (n1 n2)
                 (lambda (f) (lambda (x) ((n1 f) ((n2 f) x))))))
         (mult (lambda (n1 n2)
                 (lambda (f) (n2 (n1 f)))))
         (pred (lambda (n)
                 (lambda (f)
                   (lambda (x)
                     (((n (lambda (g) (lambda (h) (h (g f)))))
                       (lambda (ignored) x))
                      (lambda (id) id))))))
         (sub (lambda (n1 n2)
                ((n2 pred) n1)))
         (church0 (lambda (f) (lambda (x) x)))
         (church1 (lambda (f) (lambda (x) (f x))))
         (church2 (lambda (f) (lambda (x) (f (f x)))))
         (church3 (lambda (f) (lambda (x) (f (f (f x))))))
         (church0? (lambda (n)
                     ((n (lambda (x) false)) true)))
         (church=? (lambda (n1 n2)
                     ((church0? n1) ; if
                       (church0? n2)
                       ((church0? n2) ; if
                         false
                         (church=? (sub n1 church1) (sub n2 church1)))))))
  ;; multiplication distributes over addition
  (church=? (mult church2 (plus church1 church3))
            (plus (mult church2 church1) (mult church2 church3))))
"""
    val result = machine.run[graph.G](LambdaParser.parse(church), Timeout.Infinity)
    /* We can then output our graph to a file to inspect it. */
    result.toFile("foo.dot")
  }
}
