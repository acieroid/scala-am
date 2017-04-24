/**
 * These are the traits that should be implemented by an abstract
 * machine. Example abstract machines that implement these are AAM.scala,
 * AAC.scala and Free.scala.
 */

/**
 * The interface of the abstract machine itself.
 * The abstract machine is parameterized by abstract values, addresses and
 * expressions. Look into AAM.scala for an example of how to define these
 * parameters
 */
abstract class AbstractMachine[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp] {
  /** The name of the abstract machine */
  def name: String

  /**
   * The output of the abstract machine
   */
  trait Output {
    /**
     * Returns the set of final values that can be reached by the abstract machine.
     * Example: the Scheme program (+ 1 2) has as final values the set {3} , in the concrete case.
     */
    def finalValues: Set[Abs]

    /**
     * Checks if the set of final values contains a value that subsumes @param v
     */
    def containsFinalValue(v: Abs): Boolean =
      finalValues.exists(v2 => JoinLattice[Abs].subsumes(v2, v))

    /**
     * Returns the number of states visited to evaluate the program
     */
    def numberOfStates: Int

    /**
     * Returns the time it took to evaluate the program
     */
    def time: Double

    /**
     * Does this output comes from a computation that timed out?
     */
    def timedOut: Boolean

    /**
     * Outputs the graph computed by the machine in a file, according to the given output format
     */
    def toFile(path: String)(output: GraphOutput): Unit
  }

  /**
   * Evaluates a program, given a semantics. If @param graph is true, the state
   * graph will be computed and stored in the output. Returns an object
   * implementing the Output trait, containing information about the
   * evaluation. @param timeout is the timeout in ns, when reached, the
   * evaluation stops and the currently computed results are returned.
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean = false, timeout: Timeout = Timeout.start(None)): Output
}

/**
 * Abstract machine with a control component that works in an eval-kont way: it
 * can either be evaluating something, or have reached a value and will pop a
 * continuation.
 */
abstract class EvalKontMachine[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends AbstractMachine[Exp, Abs, Addr, Time] {

  /**
   * The control component of the machine
   */
  trait Control {
    def subsumes(that: Control): Boolean
  }
  object Control {
    import org.json4s._
    import org.json4s.JsonDSL._
    import org.json4s.jackson.JsonMethods._
    import scala.language.implicitConversions
    import JSON._
    implicit def controlToJSON(c: Control): JValue = c match {
      case ControlEval(exp, env) =>
        ("type" -> "ev") ~ ("exp" -> exp.toString) ~ ("env" -> env)
      case ControlKont(v) =>
        ("type" -> "kont") ~ ("value" -> v.toString)
      case ControlError(err) =>
        ("type" -> "err") ~ ("error" -> err.toString)
      case ControlCall(fexp, args, _, _) =>
        ("type" -> "call") ~ ("fexp" -> fexp.toString) ~ ("args" -> args.map(_.toString))
      case ControlReturn(fexp, v) =>
        ("type" -> "return") ~ ("fexp" -> fexp.toString) ~ ("value" -> v.toString)
    }
  }

  /**
   * It can either be an eval component, where an expression needs to be
   * evaluated in an environment
   */
  case class ControlEval(exp: Exp, env: Environment[Addr]) extends Control {
    override def toString = s"ev(${exp})"
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
    override def toString = s"ko(${v})"
    def subsumes(that: Control) = that match {
      case ControlKont(v2) => JoinLattice[Abs].subsumes(v, v2)
      case _ => false
    }
  }
  /**
   * Or an error component, in case an error is reached (e.g., incorrect number
   * of arguments in a function call)
   */
  case class ControlError(err: SemanticError) extends Control {
    override def toString = s"err($err)"
    def subsumes(that: Control) = that.equals(this)
  }
  case class ControlCall(fexp: Exp, args: List[Abs], e: Exp, env: Environment[Addr]) extends Control {
    override def toString = {
      val argss = args.mkString(", ")
      s"call($fexp, [$argss])"
    }
    def subsumes(that: Control) = that.equals(this)
  }
  case class ControlReturn(fexp: Exp, v: Abs) extends Control {
    override def toString = s"return($fexp, $v)"
    def subsumes(that: Control) = that.equals(this)
  }
}
