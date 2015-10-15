/**
 * Basic Scheme semantics, without any optimization
 */
class BaseSchemeSemantics[Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr]) extends BaseSemantics[SchemeExp, Abs, Addr] {

  trait SchemeFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameFuncallOperator(fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameFuncallOperands(f: Abs, fexp: SchemeExp, cur: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLet(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetStar(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetrec(addr: Addr, bindings: List[(Addr, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameSet(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCond(cons: List[SchemeExp], clauses: List[(SchemeExp, List[SchemeExp])], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameAnd(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameOr(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDefine(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCasOld(variable: String, enew: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCasNew(variable: String, old: Abs, ρ: Environment[Addr]) extends SchemeFrame
  object FrameHalt extends SchemeFrame {
    override def toString() = "FHalt"
  }

  protected def evalBody(body: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = body match {
    case Nil => ActionReachedValue(absi.inject(false), σ)
    case List(exp) => ActionEval(exp, ρ, σ)
    case exp :: rest => ActionPush(exp, FrameBegin(rest, ρ), ρ, σ)
  }

  def conditional(v: Abs, t: Action[SchemeExp, Abs, Addr], f: Action[SchemeExp, Abs, Addr]): Set[Action[SchemeExp, Abs, Addr]] =
    (if (abs.isTrue(v)) Set(t) else Set()) ++ (if (abs.isFalse(v)) Set(f) else Set())

  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[SchemeExp, Abs, Addr]] =
    abs.foldValues(function, (v) => {
      val fromClo: Set[Action[SchemeExp, Abs, Addr]] = abs.getClosures[SchemeExp, Addr](v).map({
        case (SchemeLambda(args, body), ρ1) =>
          if (args.length == argsv.length) {
            bindArgs(args.zip(argsv), ρ1, σ) match {
              case (ρ2, σ) =>
                if (body.length == 1)
                  ActionStepIn[SchemeExp, Abs, Addr]((SchemeLambda(args, body), ρ1), body.head, ρ2, σ, argsv)
                else
                  ActionStepIn[SchemeExp, Abs, Addr]((SchemeLambda(args, body), ρ1), SchemeBegin(body), ρ2, σ, argsv)
            }
          } else { ActionError[SchemeExp, Abs, Addr](s"Arity error when calling $fexp (${args.length} arguments expected, got ${argsv.length})") }
        case (λ, _) => ActionError[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")
      })
      val fromPrim = abs.getPrimitive(v) match {
        case Some(prim) => prim.call(fexp, argsv, σ) match {
          case Right((res, σ2)) => Set(ActionReachedValue[SchemeExp, Abs, Addr](res, σ2))
          case Left(err) => Set(ActionError[SchemeExp, Abs, Addr](err))
        }
        case None => Set()
      }
      if (fromClo.isEmpty && fromPrim.isEmpty) {
        Set(ActionError(s"Called value is not a function: $v"))
      } else {
        fromClo ++ fromPrim
      }})

  protected def evalValue(v: Value): Option[Abs] = v match {
    case ValueString(s) => Some(absi.inject(s))
    case ValueInteger(n) => Some(absi.inject(n))
    case ValueBoolean(b) => Some(absi.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, ρ, σ)
    case e :: rest => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ), ρ, σ))
  }
  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, ρ, σ)

  protected def evalQuoted(exp: SExp, σ: Store[Addr, Abs]): (Abs, Store[Addr, Abs]) = exp match {
    case SExpIdentifier(sym) => (absi.injectSymbol(sym), σ)
    case SExpPair(car, cdr) => {
      val care: SchemeExp = SchemeIdentifier(car.toString).setPos(car.pos)
      val cdre: SchemeExp = SchemeIdentifier(cdr.toString).setPos(cdr.pos)
      val cara = addri.cell(care)
      val (carv, σ2) = evalQuoted(car, σ)
      val cdra = addri.cell(cdre)
      val (cdrv, σ3) = evalQuoted(cdr, σ2)
      (absi.cons(cara, cdra), σ3.extend(cara, carv).extend(cdra, cdrv))
    }
    case SExpValue(v) => (v match {
      case ValueString(str) => absi.inject(str)
      case ValueCharacter(c) => throw new Exception("character not yet supported")
      case ValueSymbol(sym) => absi.injectSymbol(sym) /* shouldn't happen */
      case ValueInteger(n) => absi.inject(n)
      case ValueFloat(n) => throw new Exception("floats not yet supported")
      case ValueBoolean(b) => absi.inject(b)
      case ValueNil() => absi.nil
    }, σ)
    case SExpQuoted(q) => evalQuoted(SExpPair(SExpIdentifier("quote"), SExpPair(q, SExpValue(ValueNil()))), σ)
  }

  def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs]) = e match {
    case λ: SchemeLambda => Set(ActionReachedValue(absi.inject[SchemeExp, Addr]((λ, ρ)), σ))
    case SchemeFuncall(f, args) => Set(ActionPush(f, FrameFuncallOperator(f, args, ρ), ρ, σ))
    case SchemeIf(cond, cons, alt) => Set(ActionPush(cond, FrameIf(cons, alt, ρ), ρ, σ))
    case SchemeLet(Nil, body) => Set(evalBody(body, ρ, σ))
    case SchemeLet((v, exp) :: bindings, body) => Set(ActionPush(exp, FrameLet(v, List(), bindings, body, ρ), ρ, σ))
    case SchemeLetStar(Nil, body) => Set(evalBody(body, ρ, σ))
    case SchemeLetStar((v, exp) :: bindings, body) => Set(ActionPush(exp, FrameLetStar(v, bindings, body, ρ), ρ, σ))
    case SchemeLetrec(Nil, body) => Set(evalBody(body, ρ, σ))
    case SchemeLetrec((v, exp) :: bindings, body) => {
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(addri.variable)
      val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ, σ), (v, a)) => (ρ.extend(v, a), σ.extend(a, absi.bottom)) })
      Set(ActionPush(exp, FrameLetrec(addresses.head, addresses.tail.zip(bindings.map(_._2)), body, ρ1), ρ1, σ1))
    }
    case SchemeSet(variable, exp) => Set(ActionPush(exp, FrameSet(variable, ρ), ρ, σ))
    case SchemeBegin(body) => Set(evalBody(body, ρ, σ))
    case SchemeCond(Nil) => Set(ActionError(s"cond without clauses"))
    case SchemeCond((cond, cons) :: clauses) => Set(ActionPush(cond, FrameCond(cons, clauses, ρ), ρ, σ))
    case SchemeCase(key, clauses, default) => Set(ActionPush(key, FrameCase(clauses, default, ρ), ρ, σ))
    case SchemeAnd(Nil) => Set(ActionReachedValue(absi.inject(true), σ))
    case SchemeAnd(exp :: exps) => Set(ActionPush(exp, FrameAnd(exps, ρ), ρ, σ))
    case SchemeOr(Nil) => Set(ActionReachedValue(absi.inject(false), σ))
    case SchemeOr(exp :: exps) => Set(ActionPush(exp, FrameOr(exps, ρ), ρ, σ))
    case SchemeDefineVariable(name, exp) => Set(ActionPush(exp, FrameDefine(name, ρ), ρ, σ))
    case SchemeDefineFunction(name, args, body) => {
      val addr = addri.variable(name)
      val v = absi.inject[SchemeExp, Addr]((SchemeLambda(args, body), ρ))
      val ρ1 = ρ.extend(name, addr)
      val σ1 = σ.extend(addr, v)
      Set(ActionReachedValue(v, σ))
    }
    case SchemeIdentifier(name) => ρ.lookup(name) match {
      case Some(a) => Set(ActionReachedValue(σ.lookup(a), σ))
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case SchemeQuoted(quoted) => evalQuoted(quoted, σ) match {
      case (value, σ2) => Set(ActionReachedValue(value, σ2))
    }
    case SchemeValue(v) => evalValue(v) match {
      case Some(v) => Set(ActionReachedValue(v, σ))
      case None => Set(ActionError(s"Unhandled value: $v"))
    }
    case SchemeCas(variable, eold, enew) => Set(ActionPush(eold, FrameCasOld(variable, enew, ρ), ρ, σ))
  }

  def stepKont(v: Abs, σ: Store[Addr, Abs], frame: Frame) = frame match {
    case FrameHalt => Set()
    case FrameFuncallOperator(fexp, args, ρ) => funcallArgs(v, fexp, args, ρ, σ)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, ρ) => funcallArgs(f, fexp, (exp, v) :: args, toeval, ρ, σ)
    case FrameIf(cons, alt, ρ) =>
      conditional(v, ActionEval(cons, ρ, σ), ActionEval(alt, ρ, σ))
    case FrameLet(name, bindings, Nil, body, ρ) => {
      val variables = name :: bindings.reverse.map(_._1)
      val addresses = variables.map(addri.variable)
      val (ρ1, σ1) = ((name, v) :: bindings).zip(addresses).foldLeft((ρ, σ))({
        case ((ρ, σ), ((variable, value), addr)) => (ρ.extend(variable, addr), σ.extend(addr, value))
      })
      Set(evalBody(body, ρ1, σ1))
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body, ρ) =>
      Set(ActionPush(e, FrameLet(variable, (name, v) :: bindings, toeval, body, ρ), ρ, σ))
    case FrameLetStar(name, bindings, body, ρ) => {
      val addr = addri.variable(name)
      val ρ1 = ρ.extend(name, addr)
      val σ1 = σ.extend(addr, v)
      bindings match {
        case Nil => Set(evalBody(body, ρ1, σ1))
        case (variable, exp) :: rest => Set(ActionPush(exp, FrameLetStar(variable, rest, body, ρ1), ρ1, σ1))
      }
    }
    case FrameLetrec(addr, Nil, body, ρ) => Set(evalBody(body, ρ, σ.update(addr, v)))
    case FrameLetrec(addr, (addr1, exp) :: rest, body, ρ) =>
      Set(ActionPush(exp, FrameLetrec(addr1, rest, body, ρ), ρ, σ.update(addr, v)))
    case FrameSet(name, ρ) => ρ.lookup(name) match {
      case Some(a) => Set(ActionReachedValue(absi.inject(false), σ.update(a, v)))
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case FrameBegin(body, ρ) => Set(evalBody(body, ρ, σ))
    case FrameCond(cons, clauses, ρ) =>
      conditional(v, if (cons.isEmpty) { ActionReachedValue(v, σ) } else { evalBody(cons, ρ, σ) },
        clauses match {
          case Nil => ActionReachedValue(absi.inject(false), σ)
          case (exp, cons2) :: rest => ActionPush(exp, FrameCond(cons2, rest, ρ), ρ, σ)
        })
    case FrameCase(clauses, default, ρ) => throw new Exception(s"TODO: case not handled (yet)")
      /* TODO: find every clause that can be true, generate one successor per
         clause in order, until a clause that can't be false is reached. If none
         is reached, generate a successor for the default */
    case FrameAnd(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(absi.inject(false), σ))
    case FrameAnd(e :: rest, ρ) =>
      conditional(v, ActionPush(e, FrameAnd(rest, ρ), ρ, σ), ActionReachedValue(absi.inject(false), σ))
    case FrameOr(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(absi.inject(false), σ))
    case FrameOr(e :: rest, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionPush(e, FrameOr(rest, ρ), ρ, σ))
    case FrameDefine(name, ρ) => throw new Exception(s"TODO: define not handled (no global environment)")
    case FrameCasOld(variable, enew, ρ) =>
      Set(ActionPush(enew, FrameCasNew(variable, v, ρ), ρ, σ))
    case FrameCasNew(variable, old, ρ) =>
      ρ.lookup(variable) match {
        case Some(a) => conditional(abs.eq(σ.lookup(a), old),
          /* Compare and swap succeeds */
          ActionReachedValue(absi.inject(true), σ.update(a, v)),
          /* Compare and swap fails */
          ActionReachedValue(absi.inject(false), σ))
        case None => Set(ActionError(s"Unbound variable: $variable"))
      }
  }

  def parse(program: String): SchemeExp = Scheme.parse(program)
}

/**
 * Extend base Scheme semantics with:
 *   - atomic evaluation: parts of some constructs can be evaluated atomically
 *     without needing to introduce more states in the state graph. For example,
 *     (+ 1 1) can directly be evaluated to 2 without modifying the store. Also,
 *     to evaluate (+ 1 (f)), we can directly push the continuation and jump to
 *     the evaluation of (f), instead of evaluating +, and 1 in separate states.
 */
class SchemeSemantics[Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr]) extends BaseSchemeSemantics[Abs, Addr] {

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Option[Abs] = e match {
    case λ: SchemeLambda => Some(absi.inject[SchemeExp, Addr]((λ, ρ)))
    case SchemeIdentifier(name) => ρ.lookup(name).map(σ.lookup _)
    case SchemeValue(v) => evalValue(v)
    case _ => None
  }

  override protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, ρ, σ)
    case e :: rest => atomicEval(e, ρ, σ) match {
      case Some(v) => funcallArgs(f, fexp, (e, v) :: args, rest, ρ, σ)
      case None => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ), ρ, σ))
    }
  }

  /**
   * Optimize the following pattern: when we see an ActionPush(exp, frame, ρ, σ)
   * where exp is an atomic expression, we can atomically evaluate exp to get v,
   * and call stepKont(v, σ, frame).
   */
  protected def optimizeAtomic(actions: Set[Action[SchemeExp, Abs, Addr]]): Set[Action[SchemeExp, Abs, Addr]] = {
    actions.flatMap({
      case ActionPush(exp, frame, ρ, σ) => atomicEval(exp, ρ, σ) match {
        case Some(v) => stepKont(v, σ, frame)
        case None => Set[Action[SchemeExp, Abs, Addr]](ActionPush(exp, frame, ρ, σ))
      }
      case action => Set[Action[SchemeExp, Abs, Addr]](action)
    })
  }

  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs]) =
    optimizeAtomic(super.stepEval(e, ρ, σ))

  override def stepKont(v: Abs, σ: Store[Addr, Abs], frame: Frame) =
    optimizeAtomic(super.stepKont(v, σ, frame))
}
