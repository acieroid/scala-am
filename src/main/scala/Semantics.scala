/**
 * This is where the semantics of a language are defined. By defining the
 * semantics of a language, you get an abstract abstract machine for free (but
 * you might need to adapt existing lattices to support values from your
 * language).
 *
 * Semantics should be defined as small-step operational semantics. To define a
 * semantics, you have to implement the Semantics trait. You'll need to
 * specialize it on the type of expression of your language (e.g., for ANF,
 * ANFSemantics specializes on ANFExp). To do so, you need to define what
 * actions should be taken when:
 *   1. Evaluating an expression e (stepEval)
 *   2. Continuing evaluation when a value v has been reached (stepKont)
 *
 * To have a simple overview of how semantics should be defined, look at the
 * ANFSemantics class, as it defines semantics of ANF Scheme, a very lightweight
 * language.
 */

trait Semantics[Exp, Abs, Addr] {
  implicit def abs : AbstractValue[Abs]
  implicit def absi : AbstractInjection[Abs]
  implicit def addr : Address[Addr]
  implicit def addri : AddressInjection[Addr]
  implicit def exp : Expression[Exp]
  /**
   * Defines what actions should be taken when an expression e needs to be
   * evaluated, in environment e with store σ
   */
  def stepEval(e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[Exp, Abs, Addr]]
  /**
   * Defines what actions should be taken when a value v has been reached, and
   * the topmost frame is frame
   */
  def stepKont(v: Abs, σ: Store[Addr, Abs], frame: Frame): Set[Action[Exp, Abs, Addr]]
}

/**
 * The different kinds of actions that can be taken by the abstract machine
 */
abstract class Action[Exp : Expression, Abs : AbstractValue, Addr : Address]
/**
 * A value is reached by the interpreter. As a result, a continuation will be popped with the given reached value.
 */
case class ActionReachedValue[Exp : Expression, Abs : AbstractValue, Addr : Address](v: Abs, σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
/**
 * A frame needs to be pushed on the stack, and the interpretation continues by evaluating expression e in environment ρ
 */
case class ActionPush[Exp : Expression, Abs : AbstractValue, Addr : Address](e: Exp, frame: Frame, ρ: Environment[Addr], σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
/**
 * Evaluation continues with expression e in environment ρ
 */
case class ActionEval[Exp : Expression, Abs : AbstractValue, Addr : Address](e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
/**
 * Similar to ActionEval, but only used when stepping inside a function's body (clo is therefore the function stepped into)
 */
case class ActionStepIn[Exp : Expression, Abs : AbstractValue, Addr : Address](clo: (Exp, Environment[Addr]), e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
/**
 * An error has been reached
 */
case class ActionError[Exp : Expression, Abs : AbstractValue, Addr : Address](reason: String) extends Action[Exp, Abs, Addr]

class ANFSemantics[Abs, Addr](implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
                              ad: Address[Addr], adi: AddressInjection[Addr]) extends Semantics[ANFExp, Abs, Addr] {
  /* wtf scala */
  def abs = implicitly[AbstractValue[Abs]]
  def absi = implicitly[AbstractInjection[Abs]]
  def addr = implicitly[Address[Addr]]
  def addri = implicitly[AddressInjection[Addr]]
  def exp = implicitly[Expression[ANFExp]]
  sealed abstract class ANFFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
  }
  case class FrameLet(v: String, body: ANFExp, env: Environment[Addr]) extends ANFFrame {
    override def toString() = s"FrameLet(${v.toString})"
  }
  case class FrameLetrec(v: String, a: Addr, body: ANFExp, env: Environment[Addr]) extends ANFFrame {
    override def toString() = s"FrameLetrec(${v.toString})"
  }
  object FrameHalt extends ANFFrame {
    override def toString() = "FHalt"
  }

  def atomicEval(e: ANFAtomicExp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Either[String, Abs] = e match {
    case λ: ANFLambda => Right(absi.inject[ANFExp, Addr]((λ, ρ)))
    case ANFIdentifier(name) => ρ.lookup(name) match {
      case Some(a) => Right(σ.lookup(a))
      case None => Left(s"Unbound variable: $name")
    }
    case ANFValue(ValueString(s)) => Right(absi.inject(s))
    case ANFValue(ValueInteger(n)) => Right(absi.inject(n))
    case ANFValue(ValueBoolean(b)) => Right(absi.inject(b))
    case ANFValue(v) => Left(s"Unhandled value: ${v}")
  }

  def bindArgs(l: List[(String, (ANFExp, Abs))], ρ: Environment[Addr], σ: Store[Addr, Abs]): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((ρ, σ))({ case ((ρ, σ), (name, (exp, value))) => {
      val a = addri.variable(name)
      (ρ.extend(name, a), σ.extend(a, value))
    }})

  def stepEval(e: ANFExp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[ANFExp, Abs, Addr]] = e match {
    case ae: ANFAtomicExp => atomicEval(ae, ρ, σ) match {
      case Left(err) => Set(ActionError(err))
      case Right(v) => Set(ActionReachedValue(v, σ))
    }
    case ANFFuncall(f, args) =>
      val init : Either[String, List[(ANFExp, Abs)]] = Right(List())
      args.foldLeft(init)((acc: Either[String, List[(ANFExp, Abs)]], arg: ANFAtomicExp) => acc match {
        case Right(l) => atomicEval(arg, ρ, σ) match {
          case Right(v) => Right((arg, v) :: l)
          case Left(err) => Left(err)
        }
        case Left(err) => Left(err)
      }) match {
        case Left(err) => Set(ActionError(err))
        case Right(argsv) =>
          atomicEval(f, ρ, σ) match {
            case Left(err) => Set(ActionError(err))
            case Right(fv) =>
              abs.foldValues(fv, (v) => {
                val fromClo: Set[Action[ANFExp, Abs, Addr]] = abs.getClosures[ANFExp, Addr](v).map({
                  case (ANFLambda(args, body), ρ) => if (args.length == argsv.length) {
                    bindArgs(args.zip(argsv.reverse), ρ, σ) match {
                      case (ρ2, σ) => ActionStepIn((ANFLambda(args, body), ρ), body, ρ2, σ)
                    }
                  } else { ActionError[ANFExp, Abs, Addr](s"Arity error when calling $f (${args.length} arguments expected, got ${argsv.length})") }
                  case (λ, _) => ActionError[ANFExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")
                })
                val fromPrim: Set[Action[ANFExp, Abs, Addr]] = abs.getPrimitive(v) match {
                  case Some(prim) => prim.call(f, argsv, σ) match {
                    case Right((res, σ2)) => Set(ActionReachedValue(res, σ2))
                    case Left(err) => Set(ActionError(err))
                  }
                  case None => Set()
                }
                if (fromClo.isEmpty && fromPrim.isEmpty) {
                  Set(ActionError(s"Called value is not a function: $fv"))
                } else {
                  fromClo ++ fromPrim
                }
              })
          }
      }
    case ANFIf(cond, cons, alt) =>
      atomicEval(cond, ρ, σ) match {
        case Left(err) => Set(ActionError(err))
        case Right(v) => {
          val t = ActionEval(cons, ρ, σ)
          val f = ActionEval(alt, ρ, σ)
          if (abs.isTrue(v) && abs.isFalse(v)) { Set(t, f) } else if (abs.isTrue(v)) { Set(t) } else if (abs.isFalse(v)) { Set(f) } else { Set() }
        }
      }
    case ANFLet(variable, exp, body) =>
      Set(ActionPush(exp, FrameLet(variable, body, ρ), ρ, σ))
    case ANFLetrec(variable, exp, body) => {
      val vara = addri.variable(variable)
      val ρ1 = ρ.extend(variable, vara)
      val σ1 = σ.extend(vara, absi.bottom)
      Set(ActionPush(exp, FrameLetrec(variable, vara, body, ρ1), ρ1, σ1))
    }
    case ANFSet(variable, value) => ρ.lookup(variable) match {
      case Some(vara) => atomicEval(value, ρ, σ) match {
        case Left(err) => Set(ActionError(err))
        case Right(v) => Set(ActionReachedValue(v, σ.update(vara, v)))
      }
      case None => Set(ActionError(s"Unbound variable: ${variable}"))
    }
    case ANFQuoted(SExpIdentifier(sym)) => Set(ActionReachedValue(absi.injectSymbol(sym), σ))
    case ANFQuoted(sexp) => Set(ActionError("TODO: quoted expressions not yet handled"))
  }

  def stepKont(v: Abs, σ: Store[Addr, Abs], frame: Frame) = frame match {
    case FrameHalt => Set()
    case FrameLet(variable, body, ρ) => {
      val vara = addri.variable(variable)
      Set(ActionEval(body, ρ.extend(variable, vara), σ.extend(vara, v)))
    }
    case FrameLetrec(variable, vara, body, ρ) =>
      Set(ActionEval(body, ρ, σ.update(vara, v)))
  }
}

class SchemeSemantics[Abs, Addr](implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
                                 ad: Address[Addr], adi: AddressInjection[Addr]) extends Semantics[SchemeExp, Abs, Addr] {
  def abs = implicitly[AbstractValue[Abs]]
  def absi = implicitly[AbstractInjection[Abs]]
  def addr = implicitly[Address[Addr]]
  def addri = implicitly[AddressInjection[Addr]]
  def exp = implicitly[Expression[SchemeExp]]
  sealed abstract class SchemeFrame extends Frame {
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
  object FrameHalt extends SchemeFrame {
    override def toString() = "FHalt"
  }

  private def bindArgs(l: List[(String, (SchemeExp, Abs))], ρ: Environment[Addr], σ: Store[Addr, Abs]): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((ρ, σ))({ case ((ρ, σ), (name, (exp, value))) => {
      val a = addri.variable(name)
      (ρ.extend(name, a), σ.extend(a, value))
    }})

  private def evalBody(body: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Action[SchemeExp, Abs, Addr] = body match {
    case Nil => ActionReachedValue(absi.bottom /* TODO: undefined */, σ)
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
                  ActionStepIn[SchemeExp, Abs, Addr]((SchemeLambda(args, body), ρ1), body.head, ρ2, σ)
                else
                  ActionStepIn[SchemeExp, Abs, Addr]((SchemeLambda(args, body), ρ1), SchemeBegin(body), ρ2, σ)
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

  private def evalValue(v: Value): Option[Abs] = v match {
    case ValueString(s) => Some(absi.inject(s))
    case ValueInteger(n) => Some(absi.inject(n))
    case ValueBoolean(b) => Some(absi.inject(b))
    case _ => None
  }

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  private def atomicEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Option[Abs] = e match {
    case λ: SchemeLambda => Some(absi.inject[SchemeExp, Addr]((λ, ρ)))
    case SchemeIdentifier(name) => ρ.lookup(name).map(σ.lookup _)
    case SchemeValue(v) => evalValue(v)
    case _ => None
  }

  private def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, ρ, σ)
    case e :: rest => atomicEval(e, ρ, σ) match {
      case Some(v) => funcallArgs(f, fexp, (e, v) :: args, rest, ρ, σ)
      case None => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ), ρ, σ))
    }
  }
  private def funcallArgs(f: Abs, fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, ρ, σ)

  private def evalQuoted(exp: SExp, σ: Store[Addr, Abs]): (Abs, Store[Addr, Abs]) = exp match {
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
      /* TODO: the following optimization for the SchemeFuncall case breaks AAC on kcfa3 */
      /* atomicEval(f, ρ, σ) match {
           case Some(v) => funcallArgs(v, f, args, ρ, σ)
           case None => Set(ActionPush(f, FrameFuncallOperator(f, args, ρ), ρ, σ))
      } */
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
      case Some(a) => Set(ActionReachedValue(absi.bottom /* TODO: undefined */, σ.update(a, v)))
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case FrameBegin(body, ρ) => Set(evalBody(body, ρ, σ))
    case FrameCond(cons, clauses, ρ) =>
      conditional(v, if (cons.isEmpty) { ActionReachedValue(v, σ) } else { evalBody(cons, ρ, σ) },
        clauses match {
          case Nil => ActionReachedValue(absi.bottom /* TODO: undefined */, σ)
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
  }
}
