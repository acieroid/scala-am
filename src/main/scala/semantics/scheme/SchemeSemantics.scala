import SchemeOps._

/**
 * Basic Scheme semantics, without any optimization
 */
class BaseSchemeSemantics[Abs : SchemeLattice, Addr : Address, Time : Timestamp](primitives: Primitives[Addr, Abs])
    extends BaseSemantics[SchemeExp, Abs, Addr, Time] {
  def sabs = implicitly[SchemeLattice[Abs]]

  trait SchemeFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameFuncallOperator(fexp: SchemeExp, args: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameFuncallOperands(f: Abs, fexp: SchemeExp, cur: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, env: Environment[Addr]) extends SchemeFrame
  case class FrameLet(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameLetStar(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameLetrec(addr: Addr, bindings: List[(Addr, SchemeExp)], body: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameSet(variable: String, env: Environment[Addr]) extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameCond(cons: List[SchemeExp], clauses: List[(SchemeExp, List[SchemeExp])], env: Environment[Addr]) extends SchemeFrame
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameAnd(rest: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameOr(rest: List[SchemeExp], env: Environment[Addr]) extends SchemeFrame
  case class FrameDefine(variable: String, env: Environment[Addr]) extends SchemeFrame

  protected def evalBody(body: List[SchemeExp], env: Environment[Addr], store: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = body match {
    case Nil => ActionReachedValue(sabs.inject(false), store)
    case List(exp) => ActionEval(exp, env, store)
    case exp :: rest => ActionPush(exp, FrameBegin(rest, env), env, store)
  }

  def conditional(v: Abs, t: => Action[SchemeExp, Abs, Addr], f: => Action[SchemeExp, Abs, Addr]): Set[Action[SchemeExp, Abs, Addr]] =
    (if (sabs.isTrue(v)) Set(t) else Set()) ++ (if (sabs.isFalse(v)) Set(f) else Set())

  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], store: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
    val fromClo: Set[Action[SchemeExp, Abs, Addr]] = sabs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body, pos), env1) =>
        if (args.length == argsv.length) {
          bindArgs(args.zip(argsv), env1, store, t) match {
            case (env2, store) =>
              if (body.length == 1)
                ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
              else
                ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
          }
        } else { ActionError[SchemeExp, Abs, Addr](s"Arity error when calling $fexp (${args.length} arguments expected, got ${argsv.length})") }
      case (λ, _) => ActionError[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")
    })
    val fromPrim = sabs.getPrimitives(function).map(prim =>
      prim.call(fexp, argsv, store, t) match {
        case Right((res, store2, effects)) => ActionReachedValue[SchemeExp, Abs, Addr](res, store2, effects)
        case Left(err) => ActionError[SchemeExp, Abs, Addr](err)
      })
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Set(ActionError(s"Called value is not a function: $function"))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[Abs] = v match {
    case ValueString(s) => Some(sabs.inject(s))
    case ValueInteger(n) => Some(sabs.inject(n))
    case ValueFloat(n) => Some(sabs.inject(n))
    case ValueBoolean(b) => Some(sabs.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], env: Environment[Addr], store: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, store, t)
    case e :: rest => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, env), env, store))
  }
  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[SchemeExp], env: Environment[Addr], store: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, env, store, t)

  protected def evalQuoted(exp: SExp, store: Store[Addr, Abs], t: Time): (Abs, Store[Addr, Abs]) = exp match {
    case SExpIdentifier(sym, _) => (sabs.injectSymbol(sym), store)
    case SExpPair(car, cdr, _) => {
      val care: SchemeExp = SchemeIdentifier(car.toString, car.pos)
      val cdre: SchemeExp = SchemeIdentifier(cdr.toString, cdr.pos)
      val cara = addr.cell(care, t)
      val (carv, store2) = evalQuoted(car, store, t)
      val cdra = addr.cell(cdre, t)
      val (cdrv, store3) = evalQuoted(cdr, store2, t)
      (sabs.cons(cara, cdra), store3.extend(cara, carv).extend(cdra, cdrv))
    }
    case SExpValue(v, _) => (v match {
      case ValueString(str) => sabs.inject(str)
      case ValueCharacter(c) => sabs.inject(c)
      case ValueSymbol(sym) => sabs.injectSymbol(sym) /* shouldn't happen */
      case ValueInteger(n) => sabs.inject(n)
      case ValueFloat(n) => sabs.inject(n)
      case ValueBoolean(b) => sabs.inject(b)
      case ValueNil => sabs.nil
    }, store)
    case SExpQuoted(q, pos) => evalQuoted(SExpPair(SExpIdentifier("quote", pos), SExpPair(q, SExpValue(ValueNil, pos), pos), pos), store, t)
  }

  def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time) = e match {
    case λ: SchemeLambda => Set(ActionReachedValue(sabs.inject[SchemeExp, Addr]((λ, env)), store))
    case SchemeFuncall(f, args, _) => Set(ActionPush(f, FrameFuncallOperator(f, args, env), env, store))
    case SchemeIf(cond, cons, alt, _) => Set(ActionPush(cond, FrameIf(cons, alt, env), env, store))
    case SchemeLet(Nil, body, _) => Set(evalBody(body, env, store))
    case SchemeLet((v, exp) :: bindings, body, _) => Set(ActionPush(exp, FrameLet(v, List(), bindings, body, env), env, store))
    case SchemeLetStar(Nil, body, _) => Set(evalBody(body, env, store))
    case SchemeLetStar((v, exp) :: bindings, body, _) => Set(ActionPush(exp, FrameLetStar(v, bindings, body, env), env, store))
    case SchemeLetrec(Nil, body, _) => Set(evalBody(body, env, store))
    case SchemeLetrec((v, exp) :: bindings, body, _) => {
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => addr.variable(v, abs.bottom, t))
      val (env1, store1) = variables.zip(addresses).foldLeft((env, store))({ case ((env, store), (v, a)) => (env.extend(v, a), store.extend(a, abs.bottom)) })
      Set(ActionPush(exp, FrameLetrec(addresses.head, addresses.tail.zip(bindings.map(_._2)), body, env1), env1, store1))
    }
    case SchemeSet(variable, exp, _) => Set(ActionPush(exp, FrameSet(variable, env), env, store))
    case SchemeBegin(body, _) => Set(evalBody(body, env, store))
    case SchemeCond(Nil, _) => Set(ActionError(s"cond without clauses"))
    case SchemeCond((cond, cons) :: clauses, _) => Set(ActionPush(cond, FrameCond(cons, clauses, env), env, store))
    case SchemeCase(key, clauses, default, _) => Set(ActionPush(key, FrameCase(clauses, default, env), env, store))
    case SchemeAnd(Nil, _) => Set(ActionReachedValue(sabs.inject(true), store))
    case SchemeAnd(exp :: exps, _) => Set(ActionPush(exp, FrameAnd(exps, env), env, store))
    case SchemeOr(Nil, _) => Set(ActionReachedValue(sabs.inject(false), store))
    case SchemeOr(exp :: exps, _) => Set(ActionPush(exp, FrameOr(exps, env), env, store))
    case SchemeDefineVariable(name, exp, _) => Set(ActionPush(exp, FrameDefine(name, env), env, store))
    case SchemeDefineFunction(name, args, body, pos) => {
      val a = addr.variable(name, abs.bottom, t)
      val v = sabs.inject[SchemeExp, Addr]((SchemeLambda(args, body, pos), env))
      val env1 = env.extend(name, a)
      val store1 = store.extend(a, v)
      Set(ActionReachedValue(v, store))
    }
    case SchemeIdentifier(name, _) => env.lookup(name) match {
      case Some(a) => store.lookup(a) match {
        case Some(v) => Set(ActionReachedValue(v, store, Set(EffectReadVariable(a))))
        case None => Set(ActionError(s"Unbound variable: $a"))
      }
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case SchemeQuoted(quoted, _) => evalQuoted(quoted, store, t) match {
      case (value, store2) => Set(ActionReachedValue(value, store2))
    }
    case SchemeValue(v, _) => evalValue(v) match {
      case Some(v) => Set(ActionReachedValue(v, store))
      case None => Set(ActionError(s"Unhandled value: $v"))
    }
  }

  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) = frame match {
    case FrameFuncallOperator(fexp, args, env) => funcallArgs(v, fexp, args, env, store, t)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, env) => funcallArgs(f, fexp, (exp, v) :: args, toeval, env, store, t)
    case FrameIf(cons, alt, env) =>
      conditional(v, ActionEval(cons, env, store), ActionEval(alt, env, store))
    case FrameLet(name, bindings, Nil, body, env) => {
      val variables = name :: bindings.reverse.map(_._1)
      val addresses = variables.map(variable => addr.variable(variable, v, t))
      val (env1, store1) = ((name, v) :: bindings).zip(addresses).foldLeft((env, store))({
        case ((env, store), ((variable, value), a)) => (env.extend(variable, a), store.extend(a, value))
      })
      Set(evalBody(body, env1, store1))
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body, env) =>
      Set(ActionPush(e, FrameLet(variable, (name, v) :: bindings, toeval, body, env), env, store))
    case FrameLetStar(name, bindings, body, env) => {
      val a = addr.variable(name, abs.bottom, t)
      val env1 = env.extend(name, a)
      val store1 = store.extend(a, v)
      bindings match {
        case Nil => Set(evalBody(body, env1, store1))
        case (variable, exp) :: rest => Set(ActionPush(exp, FrameLetStar(variable, rest, body, env1), env1, store1))
      }
    }
    case FrameLetrec(a, Nil, body, env) => Set(evalBody(body, env, store.update(a, v)))
    case FrameLetrec(a, (a1, exp) :: rest, body, env) =>
      Set(ActionPush(exp, FrameLetrec(a1, rest, body, env), env, store.update(a, v)))
    case FrameSet(name, env) => env.lookup(name) match {
      case Some(a) => Set(ActionReachedValue(sabs.inject(false), store.update(a, v), Set(EffectWriteVariable(a))))
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case FrameBegin(body, env) => Set(evalBody(body, env, store))
    case FrameCond(cons, clauses, env) =>
      conditional(v, if (cons.isEmpty) { ActionReachedValue(v, store) } else { evalBody(cons, env, store) },
        clauses match {
          case Nil => ActionReachedValue(sabs.inject(false), store)
          case (exp, cons2) :: rest => ActionPush(exp, FrameCond(cons2, rest, env), env, store)
        })
    case FrameCase(clauses, default, env) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists(v2 => evalValue(v2.value) match {
          case None => false
          case Some(v2) => sabs.subsumes(v, v2)
        }))
          /* TODO: precision could be improved by restricting v to v2 */
          Set[Action[SchemeExp, Abs, Addr]](evalBody(body, env, store))
        else
          Set[Action[SchemeExp, Abs, Addr]]()
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
      fromClauses.toSet + evalBody(default, env, store)
    }
    case FrameAnd(Nil, env) =>
      conditional(v, ActionReachedValue(v, store), ActionReachedValue(sabs.inject(false), store))
    case FrameAnd(e :: rest, env) =>
      conditional(v, ActionPush(e, FrameAnd(rest, env), env, store), ActionReachedValue(sabs.inject(false), store))
    case FrameOr(Nil, env) =>
      conditional(v, ActionReachedValue(v, store), ActionReachedValue(sabs.inject(false), store))
    case FrameOr(e :: rest, env) =>
      conditional(v, ActionReachedValue(v, store), ActionPush(e, FrameOr(rest, env), env, store))
    case FrameDefine(name, env) => throw new Exception(s"TODO: define not handled (no global environment)")
  }

  def parse(program: String): SchemeExp = Scheme.parse(program)
  override def initialEnv = primitives.forEnv
  override def initialStore = primitives.forStore
}

/**
 * Extend base Scheme semantics with:
 *   - atomic evaluation: parts of some constructs can be evaluated atomically
 *     without needing to introduce more states in the state graph. For example,
 *     (+ 1 1) can directly be evaluated to 2 without modifying the store. Also,
 *     to evaluate (+ 1 (f)), we can directly push the continuation and jump to
 *     the evaluation of (f), instead of evaluating +, and 1 in separate states.
 */
class SchemeSemantics[Abs : SchemeLattice, Addr : Address, Time : Timestamp](primitives: Primitives[Addr, Abs])
    extends BaseSchemeSemantics[Abs, Addr, Time](primitives) {

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs]): Option[(Abs, Set[Effect[Addr]])] = e match {
    case λ: SchemeLambda => Some((sabs.inject[SchemeExp, Addr]((λ, env)), Set()))
    case SchemeIdentifier(name, _) => env.lookup(name).flatMap(a => store.lookup(a).map(v => (v, Set(EffectReadVariable(a)))))
    case SchemeValue(v, _) => evalValue(v).map(value => (value, Set()))
    case _ => None
  }

  protected def addEffects(action: Action[SchemeExp, Abs, Addr], effects: Set[Effect[Addr]]): Action[SchemeExp, Abs, Addr] = action match {
    case ActionReachedValue(v, store, effs) => ActionReachedValue(v, store, effs ++ effects)
    case ActionPush(e, frame, env, store, effs) => ActionPush(e, frame, env, store, effs ++ effects)
    case ActionEval(e, env, store, effs) => ActionEval(e, env, store, effs ++ effects)
    case ActionStepIn(fexp, clo, e, env, store, argsv, effs) => ActionStepIn(fexp, clo, e, env, store, argsv, effs ++ effects)
    case ActionError(err) => action
  }

  override protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], env: Environment[Addr], store: Store[Addr, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, store, t)
    case e :: rest => atomicEval(e, env, store) match {
      case Some((v, effs)) => funcallArgs(f, fexp, (e, v) :: args, rest, env, store, t).map(addEffects(_, effs))
      case None => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, env), env, store))
    }
  }

  /**
   * Optimize the following pattern: when we see an ActionPush(exp, frame, env, store)
   * where exp is an atomic expression, we can atomically evaluate exp to get v,
   * and call stepKont(v, store, frame).
   */
  protected def optimizeAtomic(actions: Set[Action[SchemeExp, Abs, Addr]], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
    actions.flatMap({
      case ActionPush(exp, frame, env, store, effects) => atomicEval(exp, env, store) match {
        case Some((v, effs)) => stepKont(v, frame, store, t).map(addEffects(_, effs ++ effects))
        case None => Set[Action[SchemeExp, Abs, Addr]](ActionPush(exp, frame, env, store, effects))
      }
      case action => Set[Action[SchemeExp, Abs, Addr]](action)
    })
  }

  override def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time) =
    optimizeAtomic(super.stepEval(e, env, store, t), t)

  override def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) =
    optimizeAtomic(super.stepKont(v, frame, store, t), t)
}

class ConcurrentSchemeSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier](primitives: Primitives[Addr, Abs])
    extends SchemeSemantics[Abs, Addr, Time](primitives: Primitives[Addr, Abs]) {
  def cabs = implicitly[ConcurrentSchemeLattice[Abs]]
  def aabs = implicitly[AbstractValue[Abs]]
  def thread = implicitly[ThreadIdentifier[TID]]

  case class FrameJoin(env: Environment[Addr]) extends SchemeFrame
  case class FrameCasIndex(variable: String, eold: SchemeExp, enew: SchemeExp, env: Environment[Addr]) extends SchemeFrame
  case class FrameCasOld(variable: String, index: Option[Abs], enew: SchemeExp, env: Environment[Addr]) extends SchemeFrame
  case class FrameCasNew(variable: String, index: Option[Abs], enew: SchemeExp, old: Abs, env: Environment[Addr]) extends SchemeFrame
  case class FrameAcquire(env: Environment[Addr]) extends SchemeFrame
  case class FrameRelease(env: Environment[Addr]) extends SchemeFrame

  override def addEffects(action: Action[SchemeExp, Abs, Addr], effects: Set[Effect[Addr]]) = action match {
    case ActionSpawn(t: TID @unchecked, e, env, act, effs) => ActionSpawn(t, e, env, act, effs ++ effects)
    case ActionJoin(tid, store, effs) => ActionJoin(tid, store, effs ++ effects)
    case _ => super.addEffects(action, effects)
  }

  override def stepEval(e: SchemeExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time) = e match {
    case SchemeSpawn(exp, _) =>
      val tid = thread.thread[SchemeExp, Time](exp, t)
      Set(ActionSpawn(tid, exp, env, ActionReachedValue(cabs.injectTid(tid), store)))
    case SchemeJoin(exp, _) => optimizeAtomic(Set(ActionPush(exp, FrameJoin(env), env, store)), t)
    case SchemeCas(variable, eold, enew, _) => Set(ActionPush(eold, FrameCasOld(variable, None, enew, env), env, store))
    case SchemeCasVector(variable, index, eold, enew, _) => Set(ActionPush(index, FrameCasIndex(variable, eold, enew, env), env, store))
    case SchemeAcquire(exp, _) => Set(ActionPush(exp, FrameAcquire(env), env, store))
    case SchemeRelease(exp, _) => Set(ActionPush(exp, FrameRelease(env), env, store))
    case _ => super.stepEval(e, env, store, t)
  }

  override def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) = frame match {
    case FrameJoin(env) =>
      val tids = cabs.getTids(v)
      if (tids.isEmpty) {
        Set(ActionError(s"join performed on a non-tid value: $v"))
      } else {
        Set(ActionJoin(v, store))
      }
    case FrameCasIndex(variable, eold, enew, env) =>
      Set(ActionPush(eold, FrameCasOld(variable, Some(v), enew, env), env, store))
    case FrameCasOld(variable, index, enew, env) =>
      Set(ActionPush(enew, FrameCasNew(variable, index, enew, v, env), env, store))
    case FrameCasNew(variable, index, enew, old, env) =>
      env.lookup(variable) match {
        case Some(a) => index match {
          case Some(i) =>
            /* Compare and swap on vector element */
            aabs.getVectors(store.lookupBot(a)).flatMap(va => {
              val vec = store.lookupBot(va)
              val oldvals = aabs.vectorRef(vec, i)
              oldvals.flatMap({
                case Left(_) => /* ignoring error values */ Set[Action[SchemeExp, Abs, Addr]]()
                case Right(a) => {
                  val oldval = store.lookupBot(a)
                  val success: Action[SchemeExp, Abs, Addr] = {
                    /* Vector element matches old, success */
                    val (newvec, addrs) = aabs.vectorSet(vec, i, addr.cell(enew, t))
                    ActionReachedValue(cabs.inject(true), addrs.foldLeft(store.update(va, newvec))((acc, a) => acc.updateOrExtend(a, v)),
                      addrs.flatMap(a => Set(EffectWriteVector(a), EffectReadVector(a))))
                  }
                  val fail: Action[SchemeExp, Abs, Addr] = ActionReachedValue(cabs.inject(false), store, Set(EffectReadVector(a))) /* Vector element doesn't match, fail */
                  conditional(cabs.binaryOp(Eq)(oldval, old), success, fail)
                }})})
          case None =>
            /* Compare and swap on variable value */
            conditional(cabs.binaryOp(Eq)(store.lookupBot(a), old),
              /* Compare and swap succeeds */
              ActionReachedValue(aabs.inject(true), store.update(a, v), Set(EffectWriteVariable(a), EffectReadVariable(a))),
              /* Compare and swap fails */
              ActionReachedValue(aabs.inject(false), store, Set(EffectReadVariable(a))))
        }
        case None => Set(ActionError(s"Unbound variable: $variable"))
      }
    case FrameAcquire(env) =>
      val locks = cabs.getLocks(v)
      if (locks.isEmpty) {
        Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"acquire performed on a non-lock value: $v"))
      } else {
        locks.flatMap(a => {
          val v = store.lookupBot(a)
          if (cabs.isTrue(cabs.unaryOp(IsLock)(v))) {
            if (cabs.isFalse(cabs.unaryOp(IsLocked)(v))) {
              Set[Action[SchemeExp, Abs, Addr]](ActionReachedValue[SchemeExp, Abs, Addr](cabs.inject(true), store.update(a, cabs.lockedValue), Set(EffectAcquire(a))))
            } else {
              Set[Action[SchemeExp, Abs, Addr]]()
            }
          } else {
            Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"acquire performed on a non-lock value: $v"))
          }
        })
      }
    case FrameRelease(env) =>
      val locks = cabs.getLocks(v)
      if (locks.isEmpty) {
        Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"release performed on a non-lock value: $v"))
      } else {
        cabs.getLocks(v).flatMap(a => {
          val v = store.lookupBot(a)
          if (cabs.isTrue(cabs.unaryOp(IsLock)(v))) {
            if (cabs.isTrue(cabs.unaryOp(IsLocked)(v))) {
              Set[Action[SchemeExp, Abs, Addr]](ActionReachedValue[SchemeExp, Abs, Addr](cabs.inject(true), store.update(a, cabs.unlockedValue), Set(EffectRelease(a))))
            } else {
              /* Lock is already released */
              Set[Action[SchemeExp, Abs, Addr]](ActionReachedValue[SchemeExp, Abs, Addr](cabs.inject(true), store))
            }
          } else {
            Set[Action[SchemeExp, Abs, Addr]](ActionError[SchemeExp, Abs, Addr](s"release performed on a non-lock value: $v"))
          }
        })
      }
    case _ => super.stepKont(v, frame, store, t)
  }
}
