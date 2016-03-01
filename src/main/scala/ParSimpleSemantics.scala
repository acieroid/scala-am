/*
 * Main thread spawns a new thread at each step, then waits for the result and returns the values of each variable.
 * Each thread executes the code in its body.
 */
import scala.util.parsing.input.Position

class ParSimpleSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
    extends BaseSemantics[ParSimpleExp, Abs, Addr, Time] {
  def thread = implicitly[ThreadIdentifier[TID]]

  trait ParSimpleFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameThreadCode(code: List[ParSimpleAssignment], env: Environment[Addr]) extends ParSimpleFrame
  case class FrameJoin(tids: List[TID], variable: ParSimpleVariable, pos: Position, env: Environment[Addr]) extends ParSimpleFrame

  case class ParSimpleSpawn(threads: List[(String, ParSimpleThreadCode)], variable: ParSimpleVariable, pos: Position) extends ParSimpleExp {
    override def toString = "spawn..."
  }
  case class ParSimpleJoin(tid: TID, pos: Position) extends ParSimpleExp {
    override def toString = "wait..."
  }

  private def inject(v: Value) = v match {
    case ValueString(s) => abs.inject(s)
    case ValueSymbol(sym) => abs.injectSymbol(sym)
    case ValueInteger(n) => abs.inject(n)
    case ValueFloat(f) => abs.inject(f)
    case ValueBoolean(b) => abs.inject(b)
    case ValueCharacter(c) => abs.inject(c)
    case _ => throw new Exception(s"Unhandled value: $v")
  }

  def stepEval(e: ParSimpleExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time): Set[Action[ParSimpleExp, Abs, Addr]] = e match {
    case ParSimpleProgram(vars, threads, variable, pos) => {
      val (env2, store2) = vars.foldLeft((env, store))((acc, binding) => binding match {
        case (variable, value) =>
          val vara = addr.variable(variable, abs.bottom, t)
          (acc._1.extend(variable, vara), acc._2.extend(vara, inject(value)))
      })
      Set(ActionEval(ParSimpleSpawn(threads, variable, pos), env2, store2))
    }
    case ParSimpleSpawn(threads, variable, pos) => {
      val tidlist = threads.map({ case (name, _) => name -> thread.thread[ParSimpleExp, Time](ParSimpleVariable(name, pos), t)})
      val tids = tidlist.toMap
      // val finalAction: Action[ParSimpleExp, Abs, Addr] = ActionPush(ParSimpleJoin(tids.values.head, pos), FrameJoin(tids.values.drop(1).toList, variable, pos, env), env, store)
      val finalAction: Action[ParSimpleExp, Abs, Addr] = ActionEval(variable, env, store)
      val spawnAction = threads.foldLeft(finalAction)((acc, th) => th match {
        case (name, code) =>
          ActionSpawn(tids(name), code, env, acc)
      })
      Set(spawnAction)
    }
    case ParSimpleJoin(tid, _) => Set(ActionJoin(abs.injectTid(tid), store))
    case ParSimpleThreadCode(ParSimpleAssignment(variable, value, _) :: rest, pos) => env.lookup(variable) match {
      case Some(vara) => if (rest.isEmpty) {
        Set(ActionReachedValue(abs.inject(false), store.update(vara, inject(value)), Set(EffectWriteVariable(vara))))
      } else {
        Set(ActionEval(ParSimpleThreadCode(rest, pos), env, store.update(vara, inject(value)), Set(EffectWriteVariable(vara))))
      }
      case None => Set(ActionError(s"Unbound variable: ${variable}"))
    }
    //case ParSimpleThreadCode(e :: rest, pos) => Set(ActionPush(e, FrameThreadCode(rest, env), env, store, Set()))
    //case ParSimpleThreadCode(Nil, _) => Set(ActionReachedValue(abs.inject(false), store, Set()))
    case ParSimpleAssignment(variable, value, _) => env.lookup(variable) match {
      case Some(vara) =>
        val v = inject(value)
        Set(ActionReachedValue(v, store.update(vara, v), Set(EffectWriteVariable(vara))))
      case None => Set(ActionError(s"Unbound variable: ${variable}"))
    }
    case ParSimpleVariable(variable, _) => env.lookup(variable) match {
      case Some(vara) => Set(ActionReachedValue(store.lookup(vara), store, Set(EffectReadVariable(vara))))
      case None => Set(ActionError(s"Unbound variable: ${variable}"))
    }
  }

  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) = frame match {
    case FrameThreadCode(e :: rest, env) => Set(ActionPush(e, FrameThreadCode(rest, env), env, store, Set()))
    case FrameThreadCode(Nil, _) => Set(ActionReachedValue(abs.inject(false), store, Set()))
    case FrameJoin(tid :: rest, variable, pos, env) => Set(ActionPush(ParSimpleJoin(tid, pos), FrameJoin(rest, variable, pos, env), env, store, Set()))
    case FrameJoin(Nil, variable, _, env) => Set(ActionEval(variable, env, store))
  }

  def parse(program: String): ParSimpleExp = ParSimple.parse(program)
}
