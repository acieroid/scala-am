/**
 * Semantics for ANF Scheme (abstract grammar defined in ANF.scala)
 */
class ANFSemantics[Abs : IsSchemeLattice, Addr : Address, Time : Timestamp](primitives: Primitives[Addr, Abs])
    extends BaseSemantics[ANFExp, Abs, Addr, Time] {
  def sabs = implicitly[IsSchemeLattice[Abs]]
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

  /** Performs evaluation of an atomic expression, returning either an error or the produced value */
  def atomicEval(e: ANFAtomicExp, env: Environment[Addr], store: Store[Addr, Abs]): MayFail[(Abs, Set[Effect[Addr]])] = e match {
    case lam: ANFLambda => MayFailSuccess((sabs.inject[ANFExp, Addr]((lam, env)), Set()))
    case ANFIdentifier(name, _) => env.lookup(name) match {
      case Some(a) => store.lookup(a) match {
        case Some(v) => MayFailSuccess(v, Set(EffectReadVariable(a)))
        case None => MayFailError(List(UnboundAddress(a.toString)))
      }
      case None => MayFailError(List(UnboundVariable(name)))
    }
    case ANFValue(ValueString(s), _) => MayFailSuccess((sabs.inject(s), Set()))
    case ANFValue(ValueInteger(n), _) => MayFailSuccess((sabs.inject(n), Set()))
    case ANFValue(ValueFloat(n), _) => MayFailSuccess((sabs.inject(n), Set()))
    case ANFValue(ValueBoolean(b), _) => MayFailSuccess((sabs.inject(b), Set()))
    case ANFValue(v, _) => MayFailError(List(NotSupported(s"Unhandled value: ${v}")))
  }

  def stepEval(e: ANFExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time): Set[Action[ANFExp, Abs, Addr]] = e match {
    /* To step an atomic expression, performs atomic evaluation on it */
    case ae: ANFAtomicExp => atomicEval(ae, env, store).collect({
      case (v, effs) => Set(ActionReachedValue(v, store, effs))
    }, err => Set(ActionError[ANFExp, Abs, Addr](err)))
    /* Function call is the interesting case */
    case ANFFuncall(f, args, _) =>
      val init : MayFail[(List[(ANFExp, Abs)], Set[Effect[Addr]])] = MayFailSuccess(List(), Set())
      /* We first atomically evaluate every argument (since we're in ANF, they should
       * all be atomic). */
      args.foldLeft(init)((acc: MayFail[(List[(ANFExp, Abs)], Set[Effect[Addr]])], arg) => acc.bind({ case (l, effects) =>
        atomicEval(arg, env, store).map({ case (v, effects2) => ((arg, v) :: l, effects ++ effects2) })
      })).bind({ case (argsv, effects) =>
        /* We then evaluate the operator (note that the order of evaluation of the
         * operator or operands does not matter, since they are all atomic,
         * and atomic expressions cannot perform store updates). */
        atomicEval(f, env, store).map({ case (fv, effects2) => (fv, argsv, effects ++ effects2) })
      }).collect({ case (fv, argsv, effects) =>
        /* For every value of the operand, we call the contained closure and primitive */
        val fromClo: Set[Action[ANFExp, Abs, Addr]] = sabs.getClosures[ANFExp, Addr](fv).map({
          case (ANFLambda(args, body, pos), env) => if (args.length == argsv.length) {
            /* To call a closure, bind the arguments and step into the function */
            bindArgs(args.zip(argsv.reverse), env, store, t) match {
              case (env2, store) => ActionStepIn(f, (ANFLambda(args, body, pos), env), body, env2, store, argsv, effects)
            }
          } else { ActionError[ANFExp, Abs, Addr](ArityError(f.toString, args.length, argsv.length)) }
          case (lambda, _) => ActionError[ANFExp, Abs, Addr](TypeError(lambda.toString, "operator", "closure", "not a closure"))
        })
        val fromPrim: Set[Action[ANFExp, Abs, Addr]] = sabs.getPrimitives(fv).flatMap(prim =>
          /* To call a primitive, apply the call method with the given arguments and the store */
          prim.call(f, argsv, store, t).collect({
            case (res, store2, effects2) => Set[Action[ANFExp, Abs, Addr]](ActionReachedValue[ANFExp, Abs, Addr](res, store2, effects ++ effects2))
          }, err => Set[Action[ANFExp, Abs, Addr]](ActionError[ANFExp, Abs, Addr](err))))
        if (fromClo.isEmpty && fromPrim.isEmpty) {
          Set[Action[ANFExp, Abs, Addr]](ActionError[ANFExp, Abs, Addr](TypeError(fv.toString, "operator", "function", "not a function")))
        } else {
          fromClo ++ fromPrim
        }
      }, err => Set[Action[ANFExp, Abs, Addr]](ActionError[ANFExp, Abs, Addr](err)))
    /* To evaluate (if cond cons alt), evaluate cons (which is atomic), and
     * depending on the result, either step into cons or alt, or in both */
    case ANFIf(cond, cons, alt, _) =>
      atomicEval(cond, env, store).collect({
        case (v, effects) => {
          val t = ActionEval(cons, env, store, effects)
          val f = ActionEval(alt, env, store, effects)
          if (sabs.isTrue(v) && sabs.isFalse(v)) { Set(t, f) } else if (sabs.isTrue(v)) { Set(t) } else if (sabs.isFalse(v)) { Set(f) } else { Set() }
        }
      }, err => Set(ActionError(err)))
    /* To evaluate a let, first evaluate the binding */
    case ANFLet(variable, exp, body, _) =>
      Set(ActionPush(exp, FrameLet(variable, body, env), env, store))
    /* Same for letrec, but we need to bind the variable to an undefined value first */
    case ANFLetrec(variable, exp, body, pos) => {
      val vara = addr.variable(variable, abs.bottom, t)
      val env1 = env.extend(variable, vara)
      val store1 = store.extend(vara, abs.bottom)
      Set(ActionPush(exp, FrameLetrec(variable, vara, body, env1), env1, store1))
    }
    /* A set! needs to update the value of a variable in the store */
    case ANFSet(variable, value, _) => env.lookup(variable) match {
      case Some(vara) => atomicEval(value, env, store).collect({
        case (v, effects) => Set(ActionReachedValue(v, store.update(vara, v), effects + EffectWriteVariable(vara)))
      }, err => Set(ActionError(err)))
      case None => Set(ActionError(UnboundVariable(variable)))
    }
    /* A quoted identifier is a value */
    case ANFQuoted(SExpIdentifier(sym, _), _) => Set(ActionReachedValue(sabs.injectSymbol(sym), store))
    /* A quoted s-expression is more complicated to evaluate, as it may require
     * store allocation and is therefore not atomic. We don't deal with them in
     * ANF (they can always be converted into calls to cons). */
    case ANFQuoted(sexp, _) => Set(ActionError(NotSupported("quoted expressions not yet handled in ANF")))
  }

  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time) = frame match {
    /* Allocate the variable and bind it to the reached value */
    case FrameLet(variable, body, env) => {
      val vara = addr.variable(variable, v, t)
      Set(ActionEval(body, env.extend(variable, vara), store.extend(vara, v)))
    }
    /* Just bind the variable to the reached value, since it has already been allocated */
    case FrameLetrec(variable, vara, body, env) =>
      Set(ActionEval(body, env, store.update(vara, v)))
  }

  def parse(program: String): ANFExp = ANF.parse(program)
  override def initialBindings = primitives.bindings
}
