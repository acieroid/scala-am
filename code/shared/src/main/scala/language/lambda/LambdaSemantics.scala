package scalaam.language.lambda

import scalaam.core._

/** The semantics of lambda-calculus. A semantics module extends the `Semantics`
  * trait, and is parameterized by a number of types: values V (which should
  * form a lattice), addresses A, timestamps T, and contexts C. */
case class LambdaSemantics[V, A <: Address, T, C](allocator: Allocator[A, T, C])(
    implicit val timestamp: Timestamp[T, C],
    implicit val lambdaLattice: LambdaLattice[V, A]
) extends Semantics[LambdaExp, A, V, T, C] {

  implicit val lattice: Lattice[V] = lambdaLattice

  /** Frames are pushed on the continuation stack when sub-computations have to be
    * made, in order to remember where to continue the execution of a program
    * once a value for a sub-computation has been reached. */
  /**
    * This frame is used when we are evaluating the operator of a function call
    * (`f` in `(f x)`), and stores the current expression that we are
    * evaluating, as well as the arguments to the function and the current
    * environment.
    */
  case class FrameFuncallOperator(fexp: LambdaExp, args: List[LambdaExp], env: Environment[A])
      extends Frame {
    override def toString = s"rator($fexp)"
  }

  /**
    * This frame is used when we have evaluated the operator of a function call
    * to a value `f`, and we are currently evaluating the argument `cur`. It
    * also stores the arguments that we have evaluated in `args`, and the
    * arguments that are left to evaluate in `toeval`. The current environment
    * is also stored within the frame.
    */
  case class FrameFuncallOperands(
      f: V,
      fexp: LambdaExp,
      cur: LambdaExp,
      args: List[(V, LambdaExp)],
      toeval: List[LambdaExp],
      env: Environment[A]
  ) extends Frame {
    override def toString = s"rand($cur)"
  }

  /** This frame is used for letrec bindings */
  case class FrameLetrec(
      addr: A,
      bindings: List[(A, LambdaExp)],
      body: LambdaExp,
      env: Environment[A]
  ) extends Frame {
    override def toString = s"letrec"
  }

  case class FrameIf(cons: LambdaExp, alt: LambdaExp, env: Environment[A]) extends Frame {
    override def toString = s"if"
  }

  /** stepEval defines how an expression is evaluated and returns possibly
    * multiple actions to perform. When multiple acitons are returned, this
    * indicates that the interpreter reached a non-deterministic choice (e.g.,
    * an if test where the condition is not known to be definitely true or
    * false) */
  def stepEval(e: LambdaExp, env: Environment[A], store: Store[A, V], t: T): Set[Action.A] =
    e match {
      case LambdaFun(_, _, _) =>
        /** If we are evaluationg a function, e.g., `(lambda (x) e)`, we return the
          * lattice value resulting from injecting this function and the current
          * environment in the lattice. This is using a `Value` action. This
          * action needs to be accompanied by the current store. */
        Action.Value(LambdaLattice[V, A].function(e, env), store)
      case LambdaCall(f, args, _) =>
        /** If we are evaluating a function call, e.g. `(f x)`, we push a frame to
          * remember what we still have to evaluate (after the operator has been
          * evaluated to a value, we will have to evaluate the arguments
          * `args`). We then proceed to evaluate `f`, which is passed as second
          * argument to the `Push` action. This action also needs the current
          * environment and store in which the expression `f` will be
          * evaluated. */
        Action.Push(FrameFuncallOperator(f, args, env), f, env, store)
      case LambdaVar(id) =>
        /** If we are evaluating a variable, e.g. `x`, we look up its identifier in the
          * current environment, which gives us an address `a`, which is then
          * looked up in the store. For each value that is pointed to from this
          * adress in the store, we return a `Value` action, meaning that we can
          * reach any of these values. This is implemented using the `MayFail`
          * monad, which encodes computations that may both succeed and fail (as
          * is often the case in abstract interpretation). */
        Action.fromMF(
          env.lookupMF(id).flatMap(a => store.lookupMF(a).map(v => Action.Value(v, store)))
        )
      case LambdaIf(cond, cons, alt, _) =>
        Action.Push(FrameIf(cons, alt, env), cond, env, store)
      case LambdaLetrec(Nil, body, _) =>
        Action.Eval(body, env, store)
      case LambdaLetrec(bindings, body, _) =>
        val variables = bindings.map(_._1)
        val addresses = variables.map(v => allocator.variable(v, t))
        val (env1, store1) = variables
          .zip(addresses)
          .foldLeft((env, store))({
            case ((env, store), (v, a)) =>
              (env.extend(v.name, a), store.extend(a, LambdaLattice[V, A].bottom))
          })
        val exp = bindings.head._2
        Action.Push(
          FrameLetrec(addresses.head, addresses.zip(bindings.map(_._2)).tail, body, env1),
          exp,
          env1,
          store1
        )
      case LambdaBoolean(b, _) => Action.Value(LambdaLattice[V, A].boolean(b), store)
    }

  /** The `stepKont` function is called when a value has been reached, and the
    * evaluation has to proceed by looking at the top stack frame `frame`. */
  def stepKont(v: V, frame: Frame, store: Store[A, V], t: T): Set[Action.A] = frame match {
    case FrameFuncallOperator(fexp, Nil, _) =>
      /** We have evaluated the operator of a nullary function call, e.g. `(f)`. We
        * directly proceed to evaluate the function call, relying on the
        * `evalCall` function defined below. */
      Action.Call(v, fexp, Nil, store)
    case FrameFuncallOperator(fexp, arg :: args, env) =>
      /** We have evaluated the operator of a function call, e.g. if `(f x)` we have
        * evaluated f. We proceed by evaluating the first argument `arg`, and
        * pushing a `FrameFuncallOperands` to remember that we still have to
        * evaluate other arguments later on. */
      Action.Push(FrameFuncallOperands(v, fexp, arg, List.empty, args, env), arg, env, store)
    case FrameFuncallOperands(f @ _, fexp, cur, args, Nil, env @ _) =>
      /** We have evaluated all arguments to a function call, we proceed with actually
        * calling the function. */
      Action.Call(f, fexp, ((v,cur) :: args).reverse, store)
    case FrameFuncallOperands(f @ _, fexp, cur, args, argtoeval :: argstoeval, env) =>
      /** We have evaluated some of the arguments to a function call but not all, we
        * proceed with the evaluating the rest of the arguments, again pushing a
        * `FrameFuncallOperands` frame on the stack. */
      Action.Push(
        FrameFuncallOperands(f, fexp, argtoeval, (v, cur) :: args, argstoeval, env),
        argtoeval,
        env,
        store
      )
    case FrameIf(cons, alt, env) =>
      conditional(v, Action.Eval(cons, env, store), Action.Eval(alt, env, store))
    case FrameLetrec(a, Nil, body, env) =>
      Action.Eval(body, env, store.update(a, v))
    case FrameLetrec(a, (a1, exp) :: rest, body, env) =>
      Action.Push(FrameLetrec(a1, rest, body, env), exp, env, store.update(a, v))
  }

  def conditional(v: V, t: => Set[Action.A], f: => Set[Action.A]): Set[Action.A] =
    (if (LambdaLattice[V, A].isTrue(v)) t else Action.None) ++ (if (LambdaLattice[V, A].isFalse(v))
                                                                  f
                                                                else Action.None)

  /** This functions performs the evaluation of a function call when the operator
    * `fexp` has been evaluated to the value `f`, and the arguments have been
    * evaluated to the values contained in `argsv`. We need the store to
    * evaluate this call. */
  def stepCall(
      f: V,
      fexp: LambdaExp,
      argsv: List[(V, LambdaExp)],
      store: Store[A, V],
      t: T
  ): Set[Action.A] =
    LambdaLattice[V, A]
      .closures(f) // We extract all the closures contained in the lattice value `f`
      .map({
        /** A closure `clo` is a pairing of a lambda function and a definition environment */
        case clo @ (LambdaFun(args, body, pos @ _), defenv) =>
          if (args.length == argsv.length) {
            /** We have the right number of arguments. We bind the arguments in the
             * definition environment and in the store, and step in the function
             * with the `StepIn` action, with the extended environment
             * `callenv` and updated store `store2`. */
            val (callenv, store2) = bindArgs(args.zip(argsv.map(_._1)), defenv, store, t)
            Action.StepIn(fexp, clo, body, callenv, store2)
          } else {
            /** We don't have the expected number of arguments, throw an arity error. The
             * error is no actually thrown, but rather will be represented in
             * the state graph, so there is a specific action for that. */
            Action.Err(ArityError(fexp, args.length, argsv.length))
          }
        case (lam, _) =>
          /** We called a value that is not a closure. This can't happen for our simple
           * lambda-calculus, but could happen if we were to extend it with
           * other values such as integers, and calling e.g. `(1 x)` */
          Action.Err(TypeError(lam, "closure", "not a closure"))
      })

  /** This error is raised when a function is not called with the right number of arguments */
  case class ArityError(call: LambdaExp, expected: Int, got: Int) extends Error

  /** This error is raised when encountering an unexpected type */
  case class TypeError(e: LambdaExp, expected: String, got: String) extends Error

  /** This function binds multiple arguments to their value according to the input
    * list `l`.  It bninds them in the environment `env` and store `store`,
    * returning the resulting environment and store. */
  def bindArgs(
      l: List[(Identifier, V)],
      env: Environment[A],
      store: Store[A, V],
      t: T
  ): (Environment[A], Store[A, V]) =
    l.foldLeft((env, store))({
      case ((env, store), (id, value)) => {
        val a = allocator.variable(id, t)
        (env.extend(id.name, a), store.extend(a, value))
      }
    })
}
