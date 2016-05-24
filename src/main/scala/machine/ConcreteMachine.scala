/**
 * Implementation of a concrete CESK machine. It should be faster than other
 * implementations in this framework because it doesn't store the set of visited
 * states.
 */
class ConcreteMachine[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "ConcreteMachine"

  trait ConcreteMachineOutput extends Output[Abs] {
    def toDotFile(path: String) = println("Not generating graph for ConcreteMachine")
  }

  case class ConcreteMachineOutputError(time: Double, numberOfStates: Int, err: String) extends ConcreteMachineOutput {
    def finalValues = {
      println(s"Execution failed: $err")
      Set()
    }
    def containsFinalValue(v: Abs) = false
    def timedOut = false
  }
  case class ConcreteMachineOutputTimeout(time: Double, numberOfStates: Int) extends ConcreteMachineOutput {
    def finalValues = Set()
    def containsFinalValue(v: Abs) = false
    def timedOut = true
  }
  case class ConcreteMachineOutputValue(time: Double, numberOfStates: Int, v: Abs) extends ConcreteMachineOutput {
    def finalValues = Set(v)
    def containsFinalValue(v2: Abs) = v == v2
    def timedOut = false
  }

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] = {
    def loop(control: Control, store: Store[Addr, Abs], stack: List[Frame], t: Time, start: Long, count: Int): ConcreteMachineOutput = {
      if (timeout.map(System.nanoTime - start > _).getOrElse(false)) {
        ConcreteMachineOutputTimeout((System.nanoTime - start) / Math.pow(10, 9), count)
      } else {
        control match {
          case ControlEval(e, env) =>
            val actions = sem.stepEval(e, env, store, t)
            if (actions.size == 1) {
              actions.head match {
                case ActionReachedValue(v, store2, _) => loop(ControlKont(v), store2, stack, time.tick(t), start, count + 1)
                case ActionPush(e, frame, env, store2, _) => loop(ControlEval(e, env), store2, frame :: stack, time.tick(t), start, count + 1)
                case ActionEval(e, env, store2, _) => loop(ControlEval(e, env), store2, stack, time.tick(t), start, count + 1)
                case ActionStepIn(fexp, _, e, env, store2, _, _) => loop(ControlEval(e, env), store2, stack, time.tick(t, fexp), start, count + 1)
                case ActionError(err) => ConcreteMachineOutputError((System.nanoTime - start) / Math.pow(10, 9), count, err)
              }
            } else {
              ConcreteMachineOutputError((System.nanoTime - start) / Math.pow(10, 9), count, s"execution was not concrete (got ${actions.size} actions instead of 1)")
            }
          case ControlKont(v) if abs.isError(v) => /* reached an error */
            ConcreteMachineOutputError((System.nanoTime - start) / Math.pow(10, 9), count, v.toString)
          case ControlKont(v) => /* pop a continuation */
            stack match {
              case frame :: tl =>
                val actions = sem.stepKont(v, frame, store, t)
                if (actions.size == 1) {
                  actions.head match {
                    case ActionReachedValue(v, store2, _) => loop(ControlKont(v), store2, tl, time.tick(t), start, count + 1)
                    case ActionPush(e, frame, env, store2, _) => loop(ControlEval(e, env), store2, frame :: tl, time.tick(t), start, count + 1)
                    case ActionEval(e, env, store2, _) => loop(ControlEval(e, env), store2, tl, time.tick(t), start, count + 1)
                    case ActionStepIn(fexp, _, e, env, store2, _, _) => loop(ControlEval(e, env), store2, tl, time.tick(t, fexp), start, count + 1)
                    case ActionError(err) => ConcreteMachineOutputError((System.nanoTime - start) / Math.pow(10, 9), count, err)
                  }
                } else {
                  ConcreteMachineOutputError((System.nanoTime - start) / Math.pow(10, 9), count, s"execution was not concrete (got ${actions.size} actions instead of 1)")
                }
              case Nil => ConcreteMachineOutputValue((System.nanoTime - start) / Math.pow(10, 9), count, v)
            }
          case ControlError(err) =>
            ConcreteMachineOutputError((System.nanoTime - start) / Math.pow(10, 9), count, err)
        }
      }
    }
    loop(ControlEval(exp, Environment.initial[Addr](sem.initialEnv)),
      Store.initial[Addr, Abs](sem.initialStore),
      Nil, time.initial(""), System.nanoTime, 0)
  }
}
