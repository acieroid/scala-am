/**
 * Implementation of a concrete CESK machine. It should be faster than other
 * implementations in this framework because it doesn't store the set of visited
 * states.
 */
class ConcreteMachine[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "ConcreteMachine"

  trait ConcreteMachineOutput extends Output {
    def toFile(path: String)(output: GraphOutput) = println("Not generating graph for ConcreteMachine")
  }

  case class ConcreteMachineOutputError(store: Store[Addr, Abs], time: Double, numberOfStates: Int, err: String) extends ConcreteMachineOutput {
    def finalValues = {
      println(s"Execution failed: $err")
      Set()
    }
    def timedOut = false
  }
  case class ConcreteMachineOutputTimeout(store: Store[Addr, Abs], time: Double, numberOfStates: Int) extends ConcreteMachineOutput {
    def finalValues = Set()
    def timedOut = true
  }
  case class ConcreteMachineOutputValue(store: Store[Addr, Abs], time: Double, numberOfStates: Int, v: Abs) extends ConcreteMachineOutput {
    def finalValues = Set(v)
    def timedOut = false
  }

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    def loop(control: Control, store: Store[Addr, Abs], stack: List[Frame], t: Time, count: Int): ConcreteMachineOutput = {
      if (timeout.reached) {
        ConcreteMachineOutputTimeout(store, timeout.time, count)
      } else {
        control match {
          case ControlEval(e, env) =>
            val actions = sem.stepEval(e, env, store, t)
            if (actions.size == 1) {
              actions.head match {
                case ActionReachedValue(v, store2, _) => loop(ControlKont(v), store2, stack, Timestamp[Time].tick(t), count + 1)
                case ActionPush(frame, e, env, store2, _) => loop(ControlEval(e, env), store2, frame :: stack, Timestamp[Time].tick(t), count + 1)
                case ActionEval(e, env, store2, _) => loop(ControlEval(e, env), store2, stack, Timestamp[Time].tick(t), count + 1)
                case ActionStepIn(fexp, _, e, env, store2, _, _) => loop(ControlEval(e, env), store2, stack, Timestamp[Time].tick(t, fexp), count + 1)
                case ActionError(err) => ConcreteMachineOutputError(store, timeout.time, count, err.toString)
              }
            } else {
              ConcreteMachineOutputError(store, timeout.time, count, s"execution was not concrete (got ${actions.size} actions instead of 1)")
            }
          case ControlKont(v) => /* pop a continuation */
            stack match {
              case frame :: tl =>
                val actions = sem.stepKont(v, frame, store, t)
                if (actions.size == 1) {
                  actions.head match {
                    case ActionReachedValue(v, store2, _) => loop(ControlKont(v), store2, tl, Timestamp[Time].tick(t), count + 1)
                    case ActionPush(frame, e, env, store2, _) => loop(ControlEval(e, env), store2, frame :: tl, Timestamp[Time].tick(t), count + 1)
                    case ActionEval(e, env, store2, _) => loop(ControlEval(e, env), store2, tl, Timestamp[Time].tick(t), count + 1)
                    case ActionStepIn(fexp, _, e, env, store2, _, _) => loop(ControlEval(e, env), store2, tl, Timestamp[Time].tick(t, fexp), count + 1)
                    case ActionError(err) => ConcreteMachineOutputError(store, timeout.time, count, err.toString)
                  }
                } else {
                  ConcreteMachineOutputError(store, timeout.time, count, s"execution was not concrete (got ${actions.size} actions instead of 1)")
                }
              case Nil => ConcreteMachineOutputValue(store, timeout.time, count, v)
            }
          case ControlError(err) =>
            ConcreteMachineOutputError(store, timeout.time, count, err.toString)
        }
      }
    }
    loop(ControlEval(exp, Environment.initial[Addr](sem.initialEnv)),
      Store.initial[Addr, Abs](sem.initialStore),
      Nil, Timestamp[Time].initial(""), 0)
  }
}
