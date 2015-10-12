/**
 * These are the traits that should be implemented by an abstract
 * machine. Example abstract machines that implement these are AAM.scala,
 * AAC.scala and Free.scala.
 */

/**
 * The output of the abstract machine
 */
trait Output[Abs] {
  /**
   * Returns the set of final values that can be reached by the abstract machine.
   * Example: the Scheme program (+ 1 2) has as final values the set {3} , in the concrete case.
   */
  def finalValues: Set[Abs]

  /**
   * Checks if the set of final values contains a value that subsumes @param v
   */
  def containsFinalValue(v: Abs): Boolean

  /**
   * Returns the number of states visited to evaluate the program
   */
  def numberOfStates: Int

  /**
   * Returns the time it took to evaluate the program
   */
  def time: Double

  /**
   * Outputs the graph computed by the machine in a dot file
   */
  def toDotFile(path: String): Unit
}

/**
 * The interface of the abstract machine itself
 */
trait AbstractMachine[Exp, Abs, Addr] {
  /** The abstract machine is parameterized by abstract values, addresses and
    * expressions. Look into AAM.scala for an example of how to define these
    * parameters */
  implicit def abs : AbstractValue[Abs]
  implicit def absi : AbstractInjection[Abs]
  implicit def addr : Address[Addr]
  implicit def addri : AddressInjection[Addr]
  implicit def exp : Expression[Exp]

  /** The name of the abstract machine */
  def name: String

  /** Evaluates a program, given a semantics. If @param graph is true, the state
    * graph will be computed and stored in the output. Returns an object
    * implementing the Output trait, containing information about the
    * evaluation.
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr], graph: Boolean): Output[Abs]
}
