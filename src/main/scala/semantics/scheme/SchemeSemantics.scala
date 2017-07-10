import SchemeOps._
import scalaz.Scalaz._
import scalaz._

/**
 * Basic Scheme semantics, without any optimization
 */
class BaseSchemeSemantics[V : IsSchemeLattice, Addr : Address, Time : Timestamp](primitives: Primitives[Addr, V])
    extends Semantics[SchemeExp, V, Addr, Time] {
  type Env = Environment[Addr]
  type Sto = Store[Addr, V]
  type Actions = Set[Action[SchemeExp, V, Addr]]

  trait SchemeFrame extends Frame {
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameFuncallOperator(fexp: SchemeExp, args: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameFuncallOperands(f: V, fexp: SchemeExp, cur: SchemeExp, args: List[(SchemeExp, V)],
    toeval: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, env: Env) extends SchemeFrame
  case class FrameLet(variable: Identifier, bindings: List[(Identifier, V)],
    toeval: List[(Identifier, SchemeExp)], body: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameLetStar(variable: Identifier, bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameLetrec(addr: Addr, bindings: List[(Addr, SchemeExp)], body: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameSet(variable: Identifier, env: Env) extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameCond(cons: List[SchemeExp], clauses: List[(SchemeExp, List[SchemeExp])], env: Env) extends SchemeFrame
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameAnd(rest: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameOr(rest: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameDefine(variable: Identifier, env: Env) extends SchemeFrame
  case class FrameDoInit(vars: List[(Identifier, V, Option[SchemeExp])], variable: Identifier, step: Option[SchemeExp], toeval: List[(Identifier, SchemeExp, Option[SchemeExp])], test: SchemeExp, finals: List[SchemeExp], commands: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameDoBody(toeval: List[SchemeExp], vars: List[(Identifier, V, Option[SchemeExp])], test: SchemeExp, finals: List[SchemeExp], commands: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameDoStep(vars: List[(Identifier, V, Option[SchemeExp])], variable: Identifier, step: Option[SchemeExp], toeval: List[(Identifier, V, Option[SchemeExp])], test: SchemeExp, finals: List[SchemeExp], commands: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameDoTest(vars: List[(Identifier, V, Option[SchemeExp])], test: SchemeExp, finals: List[SchemeExp], commands: List[SchemeExp], env: Env) extends SchemeFrame

  protected def evalBody(body: List[SchemeExp], env: Env, store: Sto): Actions = body match {
    case Nil => Action.value(IsSchemeLattice[V].inject(false), store)
    case List(exp) => Action.eval(exp, env, store)
    case exp :: rest => Action.push(FrameBegin(rest, env), exp, env, store)
  }

  def evalDoBody(toeval: List[SchemeExp], vars: List[(Identifier, V, Option[SchemeExp])], test: SchemeExp, finals: List[SchemeExp], commands: List[SchemeExp], t: Time, env: Env, store: Sto): Actions = toeval match {
    case Nil => evalDoStep(Nil, vars, test, finals, commands, t, env, store)
    case exp :: rest => Action.push(FrameDoBody(rest, vars, test, finals, commands, env), exp, env, store)
  }
  def evalDoStep(vars: List[(Identifier, V, Option[SchemeExp])], toeval: List[(Identifier, V, Option[SchemeExp])], test: SchemeExp, finals: List[SchemeExp], commands: List[SchemeExp], t: Time, env: Env, store: Sto): Actions = toeval match {
    case Nil =>
      val vars2 = vars.reverse
      val store2 = vars2.foldLeft(store)({
        case (store, (variable, value, None)) => store
        case (store, (variable, value, Some(step))) => env.lookup(variable.name) match {
          case Some(a) => store.update(a, value)
          case None => throw new Exception(s"unbounded variable $variable") // TODO: it SHOULD be bound
        }
      })
      Action.push(FrameDoTest(vars, test, finals, commands, env), test, env, store2)
    case (variable, v, None) :: rest =>
      evalDoStep((variable, v, None) :: vars, rest, test, finals, commands, t, env, store)
    case (variable, _, Some(step)) :: rest =>
      Action.push(FrameDoStep(vars, variable, Some(step), rest, test, finals, commands, env), step, env, store)
  }

  def conditional(v: V, t: => Actions, f: => Actions): Actions =
    (if (IsSchemeLattice[V].isTrue(v)) t else Action.none) ++ (if (IsSchemeLattice[V].isFalse(v)) f else Action.none)

  def evalCall(function: V, fexp: SchemeExp, argsv: List[(SchemeExp, V)], store: Sto, t: Time): Actions = {
    val fromClo: Actions = IsSchemeLattice[V].getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body, pos), env1) =>
        if (args.length == argsv.length) {
          bindArgs(args.zip(argsv.map(_._2)), env1, store, t) match {
            case (env2, store) =>
              if (body.length == 1)
                Action.stepIn(fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store, argsv)
              else
                Action.stepIn(fexp, (SchemeLambda(args, body, pos), env1), SchemeBegin(body, pos), env2, store, argsv)
          }
        } else { Action.error(ArityError(fexp.toString, args.length, argsv.length)) }
      case (lambda, _) => Action.error(TypeError(lambda.toString, "operator", "closure", "not a closure"))
    })
    val fromPrim: Actions = IsSchemeLattice[V].getPrimitives(function).flatMap(prim =>
      for { (res, store2, effects) <- prim.call(fexp, argsv, store, t) } yield Action.value(res, store2, effects) )
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Action.error(TypeError(function.toString, "operator", "function", "not a function"))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[V] = v match {
    case ValueString(s) => Some(IsSchemeLattice[V].inject(s))
    case ValueInteger(n) => Some(IsSchemeLattice[V].inject(n))
    case ValueReal(n) => Some(IsSchemeLattice[V].inject(n))
    case ValueBoolean(b) => Some(IsSchemeLattice[V].inject(b))
    case ValueCharacter(c) => Some(IsSchemeLattice[V].inject(c))
    case _ => None
  }

  protected def funcallArgs(f: V, fexp: SchemeExp, args: List[(SchemeExp, V)], toeval: List[SchemeExp], env: Env, store: Sto, t: Time): Actions = toeval match {
    case Nil =>
      evalCall(f, fexp, args.reverse, store, t)
    case e :: rest => Action.push(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store)
  }
  protected def funcallArgs(f: V, fexp: SchemeExp, args: List[SchemeExp], env: Env, store: Sto, t: Time): Actions =
    funcallArgs(f, fexp, List(), args, env, store, t)

  protected def evalQuoted(exp: SExp, store: Sto, t: Time): (V, Sto) = exp match {
    case SExpId(Identifier(sym, _)) => (IsSchemeLattice[V].injectSymbol(sym), store)
    case SExpPair(car, cdr, _) => {
      val care: SchemeExp = SchemeVar(Identifier(car.toString, car.pos))
      val cdre: SchemeExp = SchemeVar(Identifier(cdr.toString, cdr.pos))
      val cara = Address[Addr].cell(care, t)
      val (carv, store2) = evalQuoted(car, store, t)
      val cdra = Address[Addr].cell(cdre, t)
      val (cdrv, store3) = evalQuoted(cdr, store2, t)
        (IsSchemeLattice[V].cons(cara, cdra), store3.extend(cara, carv).extend(cdra, cdrv))
    }
    case SExpValue(v, _) => (v match {
      case ValueString(str) => IsSchemeLattice[V].inject(str)
      case ValueCharacter(c) => IsSchemeLattice[V].inject(c)
      case ValueSymbol(sym) => IsSchemeLattice[V].injectSymbol(sym) /* shouldn't happen */
      case ValueInteger(n) => IsSchemeLattice[V].inject(n)
      case ValueReal(n) => IsSchemeLattice[V].inject(n)
      case ValueBoolean(b) => IsSchemeLattice[V].inject(b)
      case ValueNil => IsSchemeLattice[V].nil
    }, store)
    case SExpQuoted(q, pos) => evalQuoted(SExpPair(SExpId(Identifier("quote", pos)), SExpPair(q, SExpValue(ValueNil, pos), pos), pos), store, t)
  }

  def stepEval(e: SchemeExp, env: Env, store: Sto, t: Time) = e match {
    case λ: SchemeLambda => Action.value(IsSchemeLattice[V].inject[SchemeExp, Addr]((λ, env)), store)
    case SchemeFuncall(f, args, _) => Action.push(FrameFuncallOperator(f, args, env), f, env, store)
    case SchemeIf(cond, cons, alt, _) => Action.push(FrameIf(cons, alt, env), cond, env, store)
    case SchemeLet(Nil, body, _) => evalBody(body, env, store)
    case SchemeLet((v, exp) :: bindings, body, _) => Action.push(FrameLet(v, List(), bindings, body, env), exp, env, store)
    case SchemeLetStar(Nil, body, _) => evalBody(body, env, store)
    case SchemeLetStar((v, exp) :: bindings, body, _) => Action.push(FrameLetStar(v, bindings, body, env), exp, env, store)
    case SchemeLetrec(Nil, body, _) => evalBody(body, env, store)
    case SchemeLetrec(bindings, body, _) =>
      val variables = bindings.map(_._1)
      val addresses = variables.map(v => Address[Addr].variable(v, JoinLattice[V].bottom, t))
      val (env1, store1) = variables.zip(addresses).foldLeft((env, store))({
        case ((env, store), (v, a)) =>
          (env.extend(v.name, a), store.extend(a, JoinLattice[V].bottom))
      })
      val exp = bindings.head._2
      Action.push(FrameLetrec(addresses.head, addresses.zip(bindings.map(_._2)).tail, body, env1), exp, env1, store1)
    case SchemeNamedLet(name, bindings, body, pos) =>
      val fexp = SchemeLambda(bindings.map(_._1), body, pos)
      val a = Address[Addr].variable(name, JoinLattice[V].bottom, t)
      val env2 = env.extend(name.name, a)
      val f = IsSchemeLattice[V].inject[SchemeExp, Addr]((fexp, env2))
      funcallArgs(f, fexp, List(), bindings.map(_._2), env2, store.extend(a, f), t)
    case SchemeSet(variable, exp, _) => Action.push(FrameSet(variable, env), exp, env, store)
    case SchemeBegin(body, _) => evalBody(body, env, store)
    case SchemeCond(Nil, _) => Action.error(NotSupported("cond without clauses"))
    case SchemeCond((cond, cons) :: clauses, _) => Action.push(FrameCond(cons, clauses, env), cond, env, store)
    case SchemeCase(key, clauses, default, _) => Action.push(FrameCase(clauses, default, env), key, env, store)
    case SchemeAnd(Nil, _) => Action.value(IsSchemeLattice[V].inject(true), store)
    case SchemeAnd(exp :: exps, _) => Action.push(FrameAnd(exps, env), exp, env, store)
    case SchemeOr(Nil, _) => Action.value(IsSchemeLattice[V].inject(false), store)
    case SchemeOr(exp :: exps, _) => Action.push(FrameOr(exps, env), exp, env, store)
    case SchemeDefineVariable(name, exp, _) => Action.push(FrameDefine(name, env), exp, env, store)
    case SchemeDefineFunction(f, args, body, pos) => {
      val a = Address[Addr].variable(f, JoinLattice[V].bottom, t)
      val v = IsSchemeLattice[V].inject[SchemeExp, Addr]((SchemeLambda(args, body, pos), env))
      val env1 = env.extend(f.name, a)
      val store1 = store.extend(a, v)
      Action.value(v, store)
    }
    case SchemeDo(Nil, test, finals, commands, pos) =>
      Action.push(FrameDoTest(Nil, test, finals, commands, env), test, env, store)
    case SchemeDo((name, init, step) :: vars, test, finals, commands, pos) =>
      Action.push(FrameDoInit(List(), name, step, vars, test, finals, commands, env), init, env, store)
    case SchemeVar(variable) => env.lookup(variable.name) match {
      case Some(a) => store.lookup(a) match {
        case Some(v) => Action.value(v, store, Set(EffectReadVariable(a)))
        case None => Action.error(UnboundAddress(a.toString))
      }
      case None => Action.error(UnboundVariable(variable))
    }
    case SchemeQuoted(quoted, _) => evalQuoted(quoted, store, t) match {
      case (value, store2) => Action.value(value, store2)
    }
    case SchemeValue(v, _) => evalValue(v) match {
      case Some(v) => Action.value(v, store)
      case None => Action.error(NotSupported(s"Unhandled value: $v"))
    }
  }

  def stepKont(v: V, frame: Frame, store: Sto, t: Time) = frame match {
    case FrameFuncallOperator(fexp, args, env) => funcallArgs(v, fexp, args, env, store, t)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, env) => funcallArgs(f, fexp, (exp, v) :: args, toeval, env, store, t)
    case FrameIf(cons, alt, env) =>
      conditional(v, Action.eval(cons, env, store), Action.eval(alt, env, store))
    case FrameLet(name, bindings, Nil, body, env) => {
      val variables = name :: bindings.reverseMap(_._1)
      val addresses = variables.map(variable => Address[Addr].variable(variable, v, t))
      val (env1, store1) = ((name, v) :: bindings).zip(addresses).foldLeft((env, store))({
        case ((env, store), ((variable, value), a)) => (env.extend(variable.name, a), store.extend(a, value))
      })
      evalBody(body, env1, store1)
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body, env) =>
      Action.push(FrameLet(variable, (name, v) :: bindings, toeval, body, env), e, env, store)
    case FrameLetStar(variable, bindings, body, env) => {
      val a = Address[Addr].variable(variable, JoinLattice[V].bottom, t)
      val env1 = env.extend(variable.name, a)
      val store1 = store.extend(a, v)
      bindings match {
        case Nil => evalBody(body, env1, store1)
        case (variable, exp) :: rest => Action.push(FrameLetStar(variable, rest, body, env1), exp, env1, store1)
      }
    }
    case FrameLetrec(a, Nil, body, env) => evalBody(body, env, store.update(a, v))
    case FrameLetrec(a, (a1, exp) :: rest, body, env) =>
      Action.push(FrameLetrec(a1, rest, body, env), exp, env, store.update(a, v))
    case FrameSet(variable, env) => env.lookup(variable.name) match {
      case Some(a) => Action.value(IsSchemeLattice[V].inject(false), store.update(a, v), Set(EffectWriteVariable(a)))
      case None => Action.error(UnboundVariable(variable))
    }
    case FrameBegin(body, env) => evalBody(body, env, store)
    case FrameCond(cons, clauses, env) =>
      conditional(v, if (cons.isEmpty) { Action.value(v, store) } else { evalBody(cons, env, store) },
        clauses match {
          case Nil => Action.value(IsSchemeLattice[V].inject(false), store)
          case (exp, cons2) :: rest => Action.push(FrameCond(cons2, rest, env), exp, env, store)
        })
    case FrameCase(clauses, default, env) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists({
          case SchemeValue(ValueSymbol(s), _) =>
            IsSchemeLattice[V].subsumes(v, IsSchemeLattice[V].injectSymbol(s))
          case v2 => evalValue(v2.value).exists(v2 => IsSchemeLattice[V].subsumes(v, v2)) })) {
          /* TODO: precision could be improved by restricting v to v2 */
          evalBody(body, env, store)
        } else {
          Action.none
        }
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
        fromClauses.toSet ++ evalBody(default, env, store)
    }
    case FrameAnd(Nil, env) =>
      conditional(v, Action.value(v, store), Action.value(IsSchemeLattice[V].inject(false), store))
    case FrameAnd(e :: rest, env) =>
      conditional(v, Action.push(FrameAnd(rest, env), e, env, store), Action.value(IsSchemeLattice[V].inject(false), store))
    case FrameOr(Nil, env) =>
      conditional(v, Action.value(v, store), Action.value(IsSchemeLattice[V].inject(false), store))
    case FrameOr(e :: rest, env) =>
      conditional(v, Action.value(v, store), Action.push(FrameOr(rest, env), e, env, store))
    case FrameDefine(name, env) => throw new Exception("TODO: define not handled (no global environment)")
    case FrameDoInit(vars, name, step, Nil, test, finals, commands, env) =>
      val vars2 = ((name, v, step) :: vars).reverse
      val addresses = vars2.map({ case (variable, value, _) => Address[Addr].variable(variable, value, t) })
      val (env2, store2) = vars2.zip(addresses).foldLeft((env, store))({
        case ((env, store), ((variable, value, _), a)) => (env.extend(variable.name, a), store.extend(a, value))
      })
      Action.push(FrameDoTest(vars2, test, finals, commands, env2), test, env2, store2)
    case FrameDoInit(vars, name, step, (name2, init, step2) :: toeval, test, finals, commands, env) =>
      Action.push(FrameDoInit((name, v, step) :: vars, name2, step2, toeval, test, finals, commands, env), init, env, store)
    case FrameDoTest(vars, test, finals, commands, env) =>
      conditional(v,
        /* eval to true, run finals */
        evalBody(finals, env, store),
        /* eval to false, run commands and keep iterating */
        evalDoBody(commands, vars, test, finals, commands, t, env, store))
    case FrameDoBody(toeval, vars, test, finals, commands, env) =>
      evalDoBody(toeval, vars, test, finals, commands, t, env, store)
    case FrameDoStep(vars, variable, step, toeval, test, finals, commands, env) =>
      evalDoStep((variable, v, step) :: vars, toeval, test, finals, commands, t, env, store)
  }

  def parse(program: String): SchemeExp = Scheme.parse(program)
  override def initialBindings = primitives.bindings
}

/**
 * Extend base Scheme semantics with:
 *   - atomic evaluation: parts of some constructs can be evaluated atomically
 *     without needing to introduce more states in the state graph. For example,
 *     (+ 1 1) can directly be evaluated to 2 without modifying the store. Also,
 *     to evaluate (+ 1 (f)), we can directly push the continuation and jump to
 *     the evaluation of (f), instead of evaluating +, and 1 in separate states.
 */
class SchemeSemantics[V : IsSchemeLattice, Addr : Address, Time : Timestamp](primitives: Primitives[Addr, V])
    extends BaseSchemeSemantics[V, Addr, Time](primitives) {
  /** Tries to perform atomic evaluation of an expression. Returns the result of
   * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(e: SchemeExp, env: Env, store: Sto): Option[(V, Set[Effect[Addr]])] = e match {
    case λ: SchemeLambda => Some((IsSchemeLattice[V].inject[SchemeExp, Addr]((λ, env)), Set()))
    case SchemeVar(variable) => env.lookup(variable.name).flatMap(a => store.lookup(a).map(v => (v, Set(EffectReadVariable(a)))))
    case SchemeValue(v, _) => evalValue(v).map(value => (value, Set()))
    case _ => None
  }

  /**
   * Optimize the following pattern: when we see an ActionPush(frame, exp, env, store)
   * where exp is an atomic expression, we can atomically evaluate exp to get v,
   * and call stepKont(v, store, frame).
   */
  protected def optimizeAtomic(actions: Actions, t: Time): Actions = actions.flatMap({
    case ActionPush(frame, exp, env, store, effects) => atomicEval(exp, env, store) match {
      case Some((v, effs)) => stepKont(v, frame, store, t).map(_.addEffects(effs ++ effects))
      case None => Action.push(frame, exp, env, store, effects)
    }
    case action => action
  })

  override protected def funcallArgs(f: V, fexp: SchemeExp, args: List[(SchemeExp, V)], toeval: List[SchemeExp],
    env: Env, store: Sto, t: Time): Actions = toeval match {
    case Nil =>
      evalCall(f, fexp, args.reverse, store, t)
    case e :: rest => atomicEval(e, env, store) match {
      case Some((v, effs)) => funcallArgs(f, fexp, (e, v) :: args, rest, env, store, t).map(_.addEffects(effs))
      case None => Action.push(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store)
    }
  }

  override def stepEval(e: SchemeExp, env: Env, store: Sto, t: Time) =
    optimizeAtomic(super.stepEval(e, env, store, t), t)

  override def stepKont(v: V, frame: Frame, store: Sto, t: Time) =
    optimizeAtomic(super.stepKont(v, frame, store, t), t)
}
