trait Semantics[Exp, Abs, Addr] {
  implicit def abs : AbstractValue[Abs]
  implicit def absi : AbstractInjection[Abs]
  implicit def addr : Address[Addr]
  implicit def addri : AddressInjection[Addr]
  implicit def exp : Expression[Exp]
  def stepEval(e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[Exp, Abs, Addr]]
  def stepKont(v: Abs, σ: Store[Addr, Abs], frame: Frame): Set[Action[Exp, Abs, Addr]]
}

abstract class Action[Exp : Expression, Abs : AbstractValue, Addr : Address]
case class ActionReachedValue[Exp : Expression, Abs : AbstractValue, Addr : Address](v: Abs, σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
case class ActionPush[Exp : Expression, Abs : AbstractValue, Addr : Address](e: Exp, frame: Frame, ρ: Environment[Addr], σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
case class ActionEval[Exp : Expression, Abs : AbstractValue, Addr : Address](e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
case class ActionStepIn[Exp : Expression, Abs : AbstractValue, Addr : Address](clo: (Exp, Environment[Addr]), e: Exp, ρ: Environment[Addr], σ: Store[Addr, Abs]) extends Action[Exp, Abs, Addr]
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

  def bindArgs(l: List[(String, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs]): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((ρ, σ))({ case ((ρ, sigma), (name, value)) => {
      val a = addri.variable(name)
      (ρ.extend(name, a), σ.extend(a, value))
    }})

  def stepEval(e: ANFExp, ρ: Environment[Addr], σ: Store[Addr, Abs]) = e match {
    case ae: ANFAtomicExp => atomicEval(ae, ρ, σ) match {
      case Left(err) => Set(ActionError(err))
      case Right(v) => Set(ActionReachedValue(v, σ))
    }
    case ANFFuncall(f, args) =>
      /* TODO: monadic style */
      val init : Either[String, List[Abs]] = Right(List())
      args.foldLeft(init)((acc: Either[String, List[Abs]], arg: ANFAtomicExp) => acc match {
        case Right(l) => atomicEval(arg, ρ, σ) match {
          case Right(v) => Right(v :: l)
          case Left(err) => Left(err)
        }
        case Left(err) => Left(err)
      }) match {
        case Left(err) => Set(ActionError(err))
        case Right(argsv) =>
          atomicEval(f, ρ, σ) match {
            case Left(err) => Set(ActionError(err))
            case Right(fv) =>
              abs.foldValues(fv, (v) => abs.getClosure[ANFExp, Addr](v) match {
                case Some((ANFLambda(args, body), ρ)) => if (args.length == argsv.length) {
                  bindArgs(args.zip(argsv), ρ, σ) match {
                    case (ρ2, σ) => Set(ActionStepIn((ANFLambda(args, body), ρ), body, ρ2, σ))
                  }
                } else { Set(ActionError(s"Arity error (${args.length} arguments expected, got ${argsv.length}")) }
                case Some((λ, _)) => Set(ActionError(s"Incorrect closure with lambda-expression ${λ}"))
                case None => abs.getPrimitive(v) match {
                  case Some((name, f)) => f(argsv) match {
                    case Right(res) => Set(ActionReachedValue(res, σ))
                    case Left(err) => Set(ActionError(err))
                  }
                  case None => Set(ActionError("Called value is not a function"))
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
        case Right(v) => Set(ActionReachedValue(v, σ.extend(vara, v)))
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
      Set(ActionEval(body, ρ, σ.extend(vara, v)))
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
  }
  case class FrameFuncallOperator(args: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameFuncallOperands(f: Abs, args: List[Abs], toeval: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLet(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetStar(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetRec(addrs: List[Addr], bindings: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
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

  def bindArgs(l: List[(String, Abs)], ρ: Environment[Addr], σ: Store[Addr, Abs]): (Environment[Addr], Store[Addr, Abs]) =
    l.foldLeft((ρ, σ))({ case ((ρ, sigma), (name, value)) => {
      val a = addri.variable(name)
      (ρ.extend(name, a), σ.extend(a, value))
    }})

  def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, Abs]) = e match {
    case λ: SchemeLambda => Set(ActionReachedValue(absi.inject[SchemeExp, Addr]((λ, ρ)), σ))
    case SchemeFuncall(f, args) => Set(ActionPush(f, FrameFuncallOperator(args, ρ), ρ, σ))
    case SchemeIf(cond, cons, alt) => Set(ActionPush(cond, FrameIf(cons, alt, ρ), ρ, σ))
    case SchemeLet((v, exp) :: bindings, body) => Set(ActionPush(exp, FrameLet(v, List(), bindings, body, ρ), ρ, σ))
    case SchemeLetStar((v, exp) :: bindings, body) => Set(ActionPush(exp, FrameLetStar(v, bindings, body, ρ), ρ, σ))
    case SchemeLetrec((v, exp) :: bindings, body) => {
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(addri.variable)
      val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ, σ), (v, a)) => (ρ.extend(v, a), σ.extend(a, absi.bottom)) })
      Set(ActionPush(exp, FrameLetRec(addresses, bindings, body, ρ1), ρ1, σ1))
    }
    case SchemeSet(variable, exp) => Set(ActionPush(exp, FrameSet(variable, ρ), ρ, σ))
    case SchemeBegin(exp :: exps) => Set(ActionPush(exp, FrameBegin(exps, ρ), ρ, σ))
    case SchemeCond((cond, cons) :: clauses) => Set(ActionPush(cond, FrameCond(cons, clauses, ρ), ρ, σ))
    case SchemeCase(key, clauses, default) => Set(ActionPush(key, FrameCase(clauses, default, ρ), ρ, σ))
    case SchemeAnd(exp :: exps) => Set(ActionPush(exp, FrameAnd(exps, ρ), ρ, σ))
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
    case SchemeQuoted(SExpIdentifier(sym)) => Set(ActionReachedValue(absi.injectSymbol(sym), σ))
    case SchemeQuoted(quoted) => throw new Exception(s"TODO: quoted expressions not yet handled")
    case SchemeValue(v) => {
      val injected = v match {
        case ValueString(s) => Some(absi.inject(s))
        case ValueInteger(n) => Some(absi.inject(n))
        case ValueBoolean(b) => Some(absi.inject(b))
        case _ => None
      }
      injected match {
        case Some(v) => Set(ActionReachedValue(v, σ))
        case None => Set(ActionError(s"Unhandled value: $v"))
      }
  }
  }

  def stepKont(v: Abs, σ: Store[Addr, Abs], frame: Frame) = frame match {
    case FrameHalt => Set()
    case _ => throw new Exception(s"Unhandled frame: $frame")
  }
}
