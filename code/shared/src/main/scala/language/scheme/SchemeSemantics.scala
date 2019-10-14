package scalaam.language.scheme

import scalaam.core._
import scalaam.language.sexp._

trait SchemeSemantics[A <: Address, V, T, C] extends Semantics[SchemeExp, A, V, T, C] {
  implicit val timestamp: Timestamp[T, C]
  implicit val schemeLattice: SchemeLattice[V, SchemeExp, A]
  val allocator: Allocator[A, T, C]

  def evalCall(
      function: V,
      fexp: SchemeExp,
      argsv: List[(SchemeExp, V)],
      store: Store[A, V],
      t: T
  ): Set[Action.A]
}

/**
  * Basic Scheme semantics, without any optimization
  */
class BaseSchemeSemantics[A <: Address, V, T, C](val allocator: Allocator[A, T, C])(
    implicit val timestamp: Timestamp[T, C],
    implicit val schemeLattice: SchemeLattice[V, SchemeExp, A]
) extends SchemeSemantics[A, V, T, C]
    with SchemePrimitives[A, V, T, C] {
  implicit val lattice: Lattice[V] = schemeLattice
  import schemeLattice._

  type Env     = Environment[A]
  type Sto     = Store[A, V]
  type Actions = Set[Action.A]

  case class ArityError(call: SchemeExp, expected: Int, got: Int) extends Error
  /* TODO[medium]: TypeError is defined both in MakeSchemeLattice and here, define it only in one place */
  case class TypeError(message: String, on: V) extends Error
  case class NotSupported(message: String)     extends Error

  trait SchemeFrame extends Frame {
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameFuncallOperator(fexp: SchemeExp, args: List[SchemeExp], env: Env)
      extends SchemeFrame
  case class FrameFuncallOperands(
      f: V,
      fexp: SchemeExp,
      cur: SchemeExp,
      args: List[(SchemeExp, V)],
      toeval: List[SchemeExp],
      env: Env
  ) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, env: Env) extends SchemeFrame
  case class FrameLet(
      variable: Identifier,
      bindings: List[(Identifier, V)],
      toeval: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      env: Env
  ) extends SchemeFrame
  case class FrameLetStar(
      variable: Identifier,
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      env: Env
  ) extends SchemeFrame
  case class FrameLetrec(addr: A, bindings: List[(A, SchemeExp)], body: List[SchemeExp], env: Env)
      extends SchemeFrame
  case class FrameSet(variable: Identifier, env: Env)    extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp], env: Env) extends SchemeFrame
  case class FrameAnd(rest: List[SchemeExp], env: Env)   extends SchemeFrame
  case class FrameOr(rest: List[SchemeExp], env: Env)    extends SchemeFrame
  case class FrameDefine(variable: Identifier, env: Env) extends SchemeFrame

  protected def evalBody(body: List[SchemeExp], env: Env, store: Sto): Actions = body match {
    case Nil         => Action.Value(bool(false), store)
    case List(exp)   => Action.Eval(exp, env, store)
    case exp :: rest => Action.Push(FrameBegin(rest, env), exp, env, store)
  }

  def conditional(v: V, t: => Actions, f: => Actions): Actions =
    (if (isTrue(v)) t else Action.None) ++ (if (isFalse(v)) f else Action.None)

  def evalCall(
      function: V,
      fexp: SchemeExp,
      argsv: List[(SchemeExp, V)],
      store: Sto,
      t: T
  ): Actions = {
    val fromClo: Actions = getClosures(function).map({
      case ((SchemeLambda(args, body, pos), env1), _) =>
        if (args.length == argsv.length) {
          bindArgs(args.zip(argsv.map(_._2)), env1, store, t) match {
            case (env2, store) =>
              if (body.length == 1)
                Action.StepIn(fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store)
              else
                Action.StepIn(
                  fexp,
                  (SchemeLambda(args, body, pos), env1),
                  SchemeBegin(body, pos),
                  env2,
                  store
                )
          }
        } else { Action.Err(ArityError(fexp, args.length, argsv.length)) }
      case ((lambda, env1),name) =>
        Action.Err(
          TypeError("operator expected to be a closure, but is not", closure((lambda, env1),None)))
    })
    val fromPrim: Actions =
      getPrimitives[Primitive](function).flatMap(prim => prim.callAction(fexp, argsv, store, t))
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Action.Err(TypeError("operator expected to be a function, but is not", function))
    } else {
      fromClo ++ fromPrim
    }
  }

  protected def evalValue(v: Value): Option[V] = v match {
    case ValueString(s)    => Some(string(s))
    case ValueInteger(n)   => Some(number(n))
    case ValueReal(n)      => Some(real(n))
    case ValueBoolean(b)   => Some(bool(b))
    case ValueCharacter(c) => Some(char(c))
    case _                 => None
  }

  protected def bindArgs(
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

  protected def funcallArgs(
      f: V,
      fexp: SchemeExp,
      args: List[(SchemeExp, V)],
      toeval: List[SchemeExp],
      env: Env,
      store: Sto,
      t: T
  ): Actions = toeval match {
    case Nil =>
      evalCall(f, fexp, args.reverse, store, t)
    case e :: rest => Action.Push(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store)
  }
  protected def funcallArgs(
      f: V,
      fexp: SchemeExp,
      args: List[SchemeExp],
      env: Env,
      store: Sto,
      t: T
  ): Actions =
    funcallArgs(f, fexp, List(), args, env, store, t)

  protected def evalQuoted(exp: SExp, store: Sto, t: T): (V, Sto) = exp match {
    case SExpId(Identifier(sym, _)) => (symbol(sym), store)
    case SExpPair(car @ _, cdr @ _, _) =>
      val (carv, store2) = evalQuoted(car, store, t)
      val (cdrv, store3) = evalQuoted(cdr, store2, t)
      val cell           = cons(carv, cdrv)
      val a              = allocator.pointer(exp, t)
      (pointer(a), store3.extend(a, cell))
    case SExpValue(v, _) =>
      (v match {
        case ValueString(str)  => string(str)
        case ValueCharacter(c) => char(c)
        case ValueSymbol(sym)  => symbol(sym) /* shouldn't happen */
        case ValueInteger(n)   => number(n)
        case ValueReal(n)      => real(n)
        case ValueBoolean(b)   => bool(b)
        case ValueNil          => nil
      }, store)
    case SExpQuoted(q, pos) =>
      evalQuoted(
        SExpPair(SExpId(Identifier("quote", pos)), SExpPair(q, SExpValue(ValueNil, pos), pos), pos),
        store,
        t
      )
  }

  def stepEval(e: SchemeExp, env: Env, store: Sto, t: T) = e match {
    case lam: SchemeLambda            => Action.Value(closure((lam, env),None), store)
    case SchemeFuncall(f, args, _)    => Action.Push(FrameFuncallOperator(f, args, env), f, env, store)
    case SchemeIf(cond, cons, alt, _) => Action.Push(FrameIf(cons, alt, env), cond, env, store)
    case SchemeLet(Nil, body, _)      => evalBody(body, env, store)
    case SchemeLet((v, exp) :: bindings, body, _) =>
      Action.Push(FrameLet(v, List(), bindings, body, env), exp, env, store)
    case SchemeLetStar(Nil, body, _) => evalBody(body, env, store)
    case SchemeLetStar((v, exp) :: bindings, body, _) =>
      Action.Push(FrameLetStar(v, bindings, body, env), exp, env, store)
    case SchemeLetrec(Nil, body, _) => evalBody(body, env, store)
    case SchemeLetrec(bindings, body, _) =>
      val variables = bindings.map(_._1)
      val addresses = variables.map(v => allocator.variable(v, t))
      val (env1, store1) = variables
        .zip(addresses)
        .foldLeft((env, store))({
          case ((env, store), (v, a)) =>
            (env.extend(v.name, a), store.extend(a, bottom))
        })
      val exp = bindings.head._2
      Action.Push(
        FrameLetrec(addresses.head, addresses.zip(bindings.map(_._2)).tail, body, env1),
        exp,
        env1,
        store1
      )
    case SchemeNamedLet(name, bindings, body, pos) =>
      val fexp = SchemeLambda(bindings.map(_._1), body, pos)
      val a    = allocator.variable(name, t)
      val env2 = env.extend(name.name, a)
      val f    = closure((fexp, env2),Some(name.name))
      funcallArgs(f, fexp, List(), bindings.map(_._2), env2, store.extend(a, f), t)
    case SchemeSet(variable, exp, _)        => Action.Push(FrameSet(variable, env), exp, env, store)
    case SchemeBegin(body, _)               => evalBody(body, env, store)
    case SchemeAnd(Nil, _)                  => Action.Value(bool(true), store)
    case SchemeAnd(exp :: exps, _)          => Action.Push(FrameAnd(exps, env), exp, env, store)
    case SchemeOr(Nil, _)                   => Action.Value(bool(false), store)
    case SchemeOr(exp :: exps, _)           => Action.Push(FrameOr(exps, env), exp, env, store)
    case SchemeDefineVariable(name, exp, _) => Action.Push(FrameDefine(name, env), exp, env, store)
    case SchemeDefineFunction(f @ _, args, body, pos) => {
      //val a = allocator.variable(f, t)
      val v = closure((SchemeLambda(args, body, pos), env),Some(f.name))
      // TODO: remove DefineFunction from the language?
      //val env1 = env.extend(f.name, a)
      //val store1 = store.extend(a, v)
      Action.Value(v, store)
    }
    case SchemeVar(variable) =>
      env.lookup(variable.name) match {
        case Some(a) =>
          store.lookup(a) match {
            case Some(v) => Action.Value(v, store)
            case None    => Action.Err(UnboundAddress(a))
          }
        case None =>
          primitives.get(variable.name) match {
            case Some(p) => Action.Value(p, store)
            case None =>
              println(s"Unbound variable $variable"); Action.Err(UnboundVariable(variable))
          }
      }
    case SchemeQuoted(quoted, _) =>
      evalQuoted(quoted, store, t) match {
        case (value, store2) => Action.Value(value, store2)
      }
    case SchemeValue(v, _) =>
      evalValue(v) match {
        case Some(v) => Action.Value(v, store)
        case None    => Action.Err(NotSupported(s"Unhandled value: $v"))
      }
  }

  def stepKont(v: V, frame: Frame, store: Sto, t: T) = frame match {
    case FrameFuncallOperator(fexp, args, env) => funcallArgs(v, fexp, args, env, store, t)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, env) =>
      funcallArgs(f, fexp, (exp, v) :: args, toeval, env, store, t)
    case FrameIf(cons, alt, env) =>
      conditional(v, Action.Eval(cons, env, store), Action.Eval(alt, env, store))
    case FrameLet(name, bindings, Nil, body, env) => {
      val variables = name :: bindings.reverseIterator.map(_._1).toList
      val addresses = variables.map(variable => allocator.variable(variable, t))
      val (env1, store1) = ((name, v) :: bindings)
        .zip(addresses)
        .foldLeft((env, store))({
          case ((env, store), ((variable, value), a)) =>
            (env.extend(variable.name, a), store.extend(a, value))
        })
      evalBody(body, env1, store1)
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body, env) =>
      Action.Push(FrameLet(variable, (name, v) :: bindings, toeval, body, env), e, env, store)
    case FrameLetStar(variable, bindings, body, env) => {
      val a      = allocator.variable(variable, t)
      val env1   = env.extend(variable.name, a)
      val store1 = store.extend(a, v)
      bindings match {
        case Nil => evalBody(body, env1, store1)
        case (variable, exp) :: rest =>
          Action.Push(FrameLetStar(variable, rest, body, env1), exp, env1, store1)
      }
    }
    case FrameLetrec(a, Nil, body, env) => evalBody(body, env, store.update(a, v))
    case FrameLetrec(a, (a1, exp) :: rest, body, env) =>
      Action.Push(FrameLetrec(a1, rest, body, env), exp, env, store.update(a, v))
    case FrameSet(variable, env) =>
      env.lookup(variable.name) match {
        case Some(a) => Action.Value(bool(false), store.update(a, v))
        case None    => Action.Err(UnboundVariable(variable))
      }
    case FrameBegin(body, env) => evalBody(body, env, store)
    case FrameAnd(Nil, _) =>
      conditional(v, Action.Value(v, store), Action.Value(bool(false), store))
    case FrameAnd(e :: rest, env) =>
      conditional(
        v,
        Action.Push(FrameAnd(rest, env), e, env, store),
        Action.Value(bool(false), store)
      )
    case FrameOr(Nil, _) =>
      conditional(v, Action.Value(v, store), Action.Value(bool(false), store))
    case FrameOr(e :: rest, env) =>
      conditional(v, Action.Value(v, store), Action.Push(FrameOr(rest, env), e, env, store))
    case FrameDefine(_, _) =>
      throw new Exception("TODO: define not handled (no global environment)")
  }

  def primitives: Map[String, V] = allPrimitives.map(p => (p.name, primitive[Primitive](p))).toMap
  override def initialBindings   =
    allPrimitives.map(p => (p.name, allocator.primitive(p.name), primitive[Primitive](p))) ++ Set(
      ("null", allocator.primitive("null"), nil)
    )
}

/**
  * Extend base Scheme semantics with:
  *   - atomic evaluation: parts of some constructs can be evaluated atomically
  *     without needing to introduce more states in the state graph. For example,
  *     (+ 1 1) can directly be evaluated to 2 without modifying the store. Also,
  *     to evaluate (+ 1 (f)), we can directly push the continuation and jump to
  *     the evaluation of (f), instead of evaluating +, and 1 in separate states.
  */
class OptimizedSchemeSemantics[A <: Address, V, T, C](allocator: Allocator[A, T, C])( // (primitives: Primitives[Addr, V])
    implicit val t: Timestamp[T, C],                                                  // TODO: how can we use the same names as implicits of the parent class?
    implicit val latt: SchemeLattice[V, SchemeExp, A]
) extends BaseSchemeSemantics[A, V, T, C](allocator)(t, latt) {
  import schemeLattice._

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(e: SchemeExp, env: Env, store: Sto): Option[V] = e match {
    case lam: SchemeLambda   => Some(closure((lam, env),None))
    case SchemeVar(variable) => env.lookup(variable.name).flatMap(a => store.lookup(a))
    case SchemeValue(v, _)   => evalValue(v)
    case _                   => None
  }

  /**
    * Optimize the following pattern: when we see an ActionPush(frame, exp, env, store)
    * where exp is an atomic expression, we can atomically evaluate exp to get v,
    * and call stepKont(v, store, frame).
    */
  protected def optimizeAtomic(actions: Actions, t: T): Actions =
    actions.flatMap({
      case act @ Action.Push(frame, exp, env, store) =>
        atomicEval(exp, env, store) match {
          case Some(v) => stepKont(v, frame, store, t)
          case None    => act
        }
      case action => action
    })

  override protected def funcallArgs(
      f: V,
      fexp: SchemeExp,
      args: List[(SchemeExp, V)],
      toeval: List[SchemeExp],
      env: Env,
      store: Sto,
      t: T
  ): Actions = toeval match {
    case Nil =>
      evalCall(f, fexp, args.reverse, store, t)
    case e :: rest =>
      atomicEval(e, env, store) match {
        case Some(v) => funcallArgs(f, fexp, (e, v) :: args, rest, env, store, t)
        case None    => Action.Push(FrameFuncallOperands(f, fexp, e, args, rest, env), e, env, store)
      }
  }

  override def stepEval(e: SchemeExp, env: Env, store: Sto, t: T) =
    optimizeAtomic(super.stepEval(e, env, store, t), t)

  override def stepKont(v: V, frame: Frame, store: Sto, t: T) =
    optimizeAtomic(super.stepKont(v, frame, store, t), t)
}
