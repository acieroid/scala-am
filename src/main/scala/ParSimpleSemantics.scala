/*
 * Main thread spawns a new thread at each step, then waits for the result and returns the values of each variable.
 * Each thread executes the code in its body.
 */
class ParSimpleSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
    extends BaseSemantics[ParSimpleExp, Abs, Addr, Time] {
  def thread = implicitly[ThreadIdentifier[TID]]

  trait ParSimpleFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameThreadCode(code: List[ParSimpleAssignment], env: Environment[Addr]) extends ParSimpleFrame
  case class FrameJoin(tids: List[TID], variable: ParSimpleVariable, env: Environment[Addr]) extends ParSimpleFrame

  case class ParSimpleSpawn(threads: List[(String, ParSimpleThreadCode)], variable: ParSimpleVariable) extends ParSimpleExp {
    override def toString = "spawn..."
  }
  case class ParSimpleJoin(tid: TID) extends ParSimpleExp {
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
    case ParSimpleProgram(vars, threads, variable) => {
      val (env2, store2) = vars.foldLeft((env, store))((acc, binding) => binding match {
        case (variable, value) =>
          val vara = addr.variable(variable, t)
          (acc._1.extend(variable, vara), acc._2.extend(vara, inject(value)))
      })
      println(env2)
      Set(ActionEval(ParSimpleSpawn(threads, variable), env2, store2))
    }
    case ParSimpleSpawn(threads, variable) => {
      val tidlist = threads.map({ case (name, _) => name -> thread.thread[ParSimpleExp, Time](ParSimpleVariable(name), t)})
      val tids = tidlist.toMap
      val finalAction: Action[ParSimpleExp, Abs, Addr] = ActionPush(ParSimpleJoin(tids.values.head), FrameJoin(tids.values.drop(1).toList, variable, env), env, store)
      val spawnAction = threads.foldLeft(finalAction)((acc, th) => th match {
        case (name, code) =>
          ActionSpawn(tids(name), code, env, acc)
      })
      Set(spawnAction)
    }
    case ParSimpleJoin(tid) => Set(ActionJoin(abs.injectTid(tid), store))
    case ParSimpleThreadCode(e :: rest) => Set(ActionPush(e, FrameThreadCode(rest, env), env, store, Set()))
    case ParSimpleThreadCode(Nil) => Set(ActionReachedValue(abs.inject(false), store, Set()))
    case ParSimpleAssignment(variable, value) => env.lookup(variable) match {
      case Some(vara) =>
        val v = inject(value)
        Set(ActionReachedValue(v, store.update(vara, v), Set(EffectWriteVariable(vara))))
      case None => Set(ActionError(s"Unbound variable: ${variable}"))
    }
    case ParSimpleVariable(variable) => env.lookup(variable) match {
      case Some(vara) => Set(ActionReachedValue(store.lookup(vara), store, Set(EffectReadVariable(vara))))
      case None => Set(ActionError(s"Unbound variable: ${variable}"))
    }
  }

  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) = frame match {
    case FrameThreadCode(e :: rest, env) => Set(ActionPush(e, FrameThreadCode(rest, env), env, store, Set()))
    case FrameThreadCode(Nil, _) => Set(ActionReachedValue(abs.inject(false), store, Set()))
    case FrameJoin(tid :: rest, variable, env) => Set(ActionPush(ParSimpleJoin(tid), FrameJoin(rest, variable, env), env, store, Set()))
    case FrameJoin(Nil, variable, env) => Set(ActionEval(variable, env, store))
  }

  def parse(program: String): ParSimpleExp = ParSimple.parse(program)
}
