/**
 * Semantics for ANF Scheme (abstract grammar defined in ANF.scala)
 */
class ANFSemantics[Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr]) extends BaseSemantics[ANFExp, Abs, Addr] {
  /** ANF Scheme only has three types of continuation frames: halt, let, and letrec */
  trait ANFFrame extends Frame {
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

  /** Performs evaluation of an atomic expression, returning either an error or the produced value */
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

  def stepEval(e: ANFExp, ρ: Environment[Addr], σ: Store[Addr, Abs]): Set[Action[ANFExp, Abs, Addr]] = e match {
    /* To step an atomic expression, performs atomic evaluation on it */
    case ae: ANFAtomicExp => atomicEval(ae, ρ, σ) match {
      case Left(err) => Set(ActionError(err))
      case Right(v) => Set(ActionReachedValue(v, σ))
    }
    /* Function call is the interesting case */
    case ANFFuncall(f, args) =>
      val init : Either[String, List[(ANFExp, Abs)]] = Right(List())
      /* We first atomically evaluate every argument (since we're in ANF, they should
       * all be atomic). Errors need to be propagated, hence the Either. */
      args.foldLeft(init)((acc: Either[String, List[(ANFExp, Abs)]], arg: ANFAtomicExp) => acc match {
        case Right(l) => atomicEval(arg, ρ, σ) match {
          case Right(v) => Right((arg, v) :: l)
          case Left(err) => Left(err)
        }
        case Left(err) => Left(err)
      }) match {
        case Left(err) => Set(ActionError(err))
        case Right(argsv) =>
          /* We then evaluate the operator (note that the order of evaluation of the
           * operator or operands does not matter, since they are all atomic,
           * and atomic expressions cannot perform store updates). */
          atomicEval(f, ρ, σ) match {
            case Left(err) => Set(ActionError(err))
            case Right(fv) =>
              /* For every value of the operand, we call the contained closure and primitive */
              abs.foldValues(fv, (v) => {
                val fromClo: Set[Action[ANFExp, Abs, Addr]] = abs.getClosures[ANFExp, Addr](v).map({
                  case (ANFLambda(args, body), ρ) => if (args.length == argsv.length) {
                    /* To call a closure, bind the arguments and step into the function */
                    bindArgs(args.zip(argsv.reverse), ρ, σ) match {
                      case (ρ2, σ) => ActionStepIn((ANFLambda(args, body), ρ), body, ρ2, σ, argsv)
                    }
                  } else { ActionError[ANFExp, Abs, Addr](s"Arity error when calling $f (${args.length} arguments expected, got ${argsv.length})") }
                  case (λ, _) => ActionError[ANFExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")
                })
                val fromPrim: Set[Action[ANFExp, Abs, Addr]] = abs.getPrimitive(v) match {
                  /* To call a primitive, apply the call method with the given arguments and the store */
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
    /* To evaluate (if cond cons alt), evaluate cons (which is atomic), and
     * depending on the result, either step into cons or alt, or in both */
    case ANFIf(cond, cons, alt) =>
      atomicEval(cond, ρ, σ) match {
        case Left(err) => Set(ActionError(err))
        case Right(v) => {
          val t = ActionEval(cons, ρ, σ)
          val f = ActionEval(alt, ρ, σ)
          if (abs.isTrue(v) && abs.isFalse(v)) { Set(t, f) } else if (abs.isTrue(v)) { Set(t) } else if (abs.isFalse(v)) { Set(f) } else { Set() }
        }
      }
    /* To evaluate a let, first evaluate the binding */
    case ANFLet(variable, exp, body) =>
      Set(ActionPush(exp, FrameLet(variable, body, ρ), ρ, σ))
    /* Same for letrec, but we need to bind the variable to an undefined value first */
    case ANFLetrec(variable, exp, body) => {
      val vara = addri.variable(variable)
      val ρ1 = ρ.extend(variable, vara)
      val σ1 = σ.extend(vara, absi.bottom)
      Set(ActionPush(exp, FrameLetrec(variable, vara, body, ρ1), ρ1, σ1))
    }
    /* A set! needs to update the value of a variable in the store */
    case ANFSet(variable, value) => ρ.lookup(variable) match {
      case Some(vara) => atomicEval(value, ρ, σ) match {
        case Left(err) => Set(ActionError(err))
        case Right(v) => Set(ActionReachedValue(v, σ.update(vara, v)))
      }
      case None => Set(ActionError(s"Unbound variable: ${variable}"))
    }
    /* A quoted identifier is a value */
    case ANFQuoted(SExpIdentifier(sym)) => Set(ActionReachedValue(absi.injectSymbol(sym), σ))
    /* A quoted s-expression is more complicated to evaluate, as it may require
     * store allocation and is therefore not atomic. We don't deal with them in
     * ANF (they can always be converted into calls to cons). */
    case ANFQuoted(sexp) => Set(ActionError("TODO: quoted expressions not yet handled"))
  }

  def stepKont(v: Abs, σ: Store[Addr, Abs], frame: Frame) = frame match {
    /* Final state is reached */
    case FrameHalt => Set()
    /* Allocate the variable and bind it to the reached value */
    case FrameLet(variable, body, ρ) => {
      val vara = addri.variable(variable)
      Set(ActionEval(body, ρ.extend(variable, vara), σ.extend(vara, v)))
    }
    /* Just bind the variable to the reached value, since it has already been allocated */
    case FrameLetrec(variable, vara, body, ρ) =>
      Set(ActionEval(body, ρ, σ.update(vara, v)))
  }

  def parse(program: String): ANFExp = ANF.parse(program)
}
