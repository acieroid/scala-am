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
trait AbstractMachine[Exp, Abs, Addr, Time] {
  /**
   * The abstract machine is parameterized by abstract values, addresses and
   * expressions. Look into AAM.scala for an example of how to define these
   * parameters */
  implicit def abs : AbstractValue[Abs]
  implicit def addr : Address[Addr]
  implicit def exp : Expression[Exp]
  implicit def time : Timestamp[Time]

  /** The name of the abstract machine */
  def name: String

  /**
   * Evaluates a program, given a semantics. If @param graph is true, the state
   * graph will be computed and stored in the output. Returns an object
   * implementing the Output trait, containing information about the
   * evaluation.
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean): Output[Abs]
}

/**
 * Abstract machine with a control component that works in an eval-kont way: it
 * can either be evaluating something, or have reached a value and will pop a
 * continuation.
 */
abstract class EvalKontMachine[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def abs = implicitly[AbstractValue[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]

  /**
   * The control component of the machine
   */
  trait Control {
    def subsumes(that: Control): Boolean
    def toString(store: Store[Addr, Abs]): String = toString()
  }

  /**
   * It can either be an eval component, where an expression needs to be
   * evaluated in an environment
   */
  case class ControlEval(exp: Exp, env: Environment[Addr]) extends Control {
    override def toString() = s"ev(${exp})"
    def subsumes(that: Control) = that match {
      case ControlEval(exp2, env2) => exp.equals(exp2) && env.subsumes(env2)
      case _ => false
    }
  }
  /**
   * Or it can be a continuation component, where a value has been reached and a
   * continuation should be popped from the stack to continue the evaluation
   */
  case class ControlKont(v: Abs) extends Control {
    override def toString() = s"ko(${v})"
    override def toString(store: Store[Addr, Abs]) = s"ko(${abs.toString(v, store)})"
    def subsumes(that: Control) = that match {
      case ControlKont(v2) => abs.subsumes(v, v2)
      case _ => false
    }
  }
  /**
   * Or an error component, in case an error is reached (e.g., incorrect number
   * of arguments in a function call)
   */
  case class ControlError(reason: String) extends Control {
    override def toString() = s"err($reason)"
    def subsumes(that: Control) = that.equals(this)
  }
}
