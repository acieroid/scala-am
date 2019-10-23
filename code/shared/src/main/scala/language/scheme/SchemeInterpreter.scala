package scalaam.language.scheme

import scalaam.core._
import scalaam.language.sexp._

/**
  * This is an interpreter that runs a program and calls a callback at every evaluated value.
  * This interpreter dictates the concrete semantics of the Scheme language analyzed by Scala-AM.
 */
class SchemeInterpreter(callback: (Position, SchemeInterpreter.Value) => Unit) {
  import SchemeInterpreter._
  /**
    * Evaluates `program`.
    * Will check the analysis result by calling `compare` on all encountered values.
    */
  def run(program: SchemeExp): Value = {
    eval(program, Map.empty)
  }

  var compared = 0
  def check(e: SchemeExp, v : Value): Value = {
    compared += 1
    v match {
      case Value.Undefined(pos@_) => () // println(s"Undefined behavior arising from position $pos seen at ${e.pos}")
      case Value.Unbound(id) => println(s"Seen unbound identifier $id at ${e.pos}")
      case _ => ()
    }
    callback(e.pos, v)
    v
  }
  var lastAddr = 0
  def newAddr(): Int = {
    lastAddr += 1
    lastAddr
  }
  var store = Map[Addr, Value]()
  def extendStore(a: Addr, v: Value): Unit = {
    store = store + (a -> v)
  }

  def eval(e: SchemeExp, env: Env): Value = check(e, e match {
    case SchemeLambda(_, _, _) => Value.Clo(e, env)
    case SchemeFuncall(f, args, pos) =>
      eval(f, env) match {
        case Value.Clo(SchemeLambda(argsNames, body, pos2), env2) =>
          if (argsNames.length != args.length) {
            throw new Exception(s"Invalid function call at position ${pos}: ${args.length} given, while ${argsNames.length} are expected")
          }
          val envExt = argsNames.zip(args).foldLeft(env2)((env3, arg) => {
            val addr = newAddr()
            extendStore(addr, eval(arg._2, env))
            (env3 + (arg._1.name -> addr))
          })
          eval(SchemeBegin(body, pos2), envExt)
        case Value.Primitive(p) =>
          p.call(args.map(arg => eval(arg, env)))
        case v =>
          throw new Exception(s"Invalid function call at position ${pos}: ${v} is not a closure or a primitive")
      }
    case SchemeIf(cond, cons, alt, _) =>
      eval(cond, env) match {
        case Value.Bool(false) => eval(alt, env)
        case _ => eval(cons, env)
      }
    case SchemeLet(bindings, body, pos) =>
      val envExt = bindings.foldLeft(env)((env2, binding) => {
        val addr = newAddr()
        extendStore(addr, eval(binding._2, env))
        (env2 + (binding._1.name -> addr))
      })
      eval(SchemeBegin(body, pos), envExt)
    case SchemeLetStar(bindings, body, pos) =>
      val envExt = bindings.foldLeft(env)((env2, binding) => {
        val addr = newAddr()
        extendStore(addr, eval(binding._2, env2 /* this is the difference with let */))
        (env2 + (binding._1.name -> addr))
      })
      eval(SchemeBegin(body, pos), envExt)
    case SchemeLetrec(bindings, body, pos) =>
      val envExt = bindings.foldLeft(env)((env2, binding) => {
        val addr = newAddr()
        /* These are the differences with let* (store and env) */
        extendStore(addr, Value.Unbound(binding._1))
        val env3 = env2 + (binding._1.name -> addr)
        extendStore(addr, eval(binding._2, env3))
        env3
      })
      eval(SchemeBegin(body, pos), envExt)
    case SchemeNamedLet(_, _, _, _) => ???
    case SchemeSet(id, v, pos) =>
      /* TODO: primitives can be reassigned with set! without being redefined */
      val addr = env.get(id.name) match {
        case Some(addr) => addr
        case None => throw new Exception(s"Unbound variable $id accessed at position $pos")
      }
      extendStore(addr, eval(v, env))
      Value.Undefined(pos)
    case SchemeBegin(exps, pos) =>
      val init: Value = Value.Undefined(pos)
      exps.foldLeft(init)((_, e) => eval(e, env))
    case SchemeAnd(Nil, _) =>
      Value.Bool(true)
    case SchemeAnd(e :: exps, pos) =>
      eval(e, env) match {
        case Value.Bool(false) => Value.Bool(false)
        case _ => eval(SchemeAnd(exps, pos), env)
      }
    case SchemeOr(Nil, _) =>
      Value.Bool(false)
    case SchemeOr(e :: exps, pos) =>
      eval(e, env) match {
        case Value.Bool(false) => eval(SchemeOr(exps, pos), env)
        case v => v
      }
    case SchemeDefineVariable(_, _, _) => ???
    case SchemeDefineFunction(_, _, _, _) => ???
    case SchemeVar(id) =>
      env.get(id.name) match {
        case Some(addr) => store.get(addr) match {
          case Some(v) => v
          case None => throw new Exception(s"Unbound variable $id at position ${id.pos}")
        }
        case None => primitive(id.name) match {
          case Some(prim) => prim
          case None => throw new Exception(s"Undefined variable $id at position ${id.pos}")
        }
      }
    case SchemeQuoted(quoted, _) =>
      evalQuoted(quoted)
    case SchemeValue(v, _) =>
      v match {
        case ValueString(s) => Value.Str(s)
        case ValueSymbol(s) => Value.Symbol(s)
        case ValueInteger(n) => Value.Integer(n)
        case ValueReal(r) => Value.Real(r)
        case ValueBoolean(b) => Value.Bool(b)
        case ValueCharacter(c) => Value.Character(c)
        case ValueNil => Value.Nil
      }
  })
  def allocateCons(car: Value, cdr: Value): Value = {
    val addr1 = newAddr()
    val addr2 = newAddr()
    extendStore(addr1, car)
    extendStore(addr2, cdr)
    Value.Cons(addr1, addr2)
  }

  def evalQuoted(quoted: SExp): Value = quoted match {
    case SExpId(Identifier(sym, _)) => Value.Symbol(sym)
    case SExpPair(car, cdr, _) =>
      allocateCons(evalQuoted(car), evalQuoted(cdr))
    case SExpValue(v, _) => v match {
      case ValueString(str)  => Value.Str(str)
      case ValueCharacter(c) => Value.Character(c)
      case ValueSymbol(sym)  => Value.Symbol(sym) /* shouldn't happen */
      case ValueInteger(n)   => Value.Integer(n)
      case ValueReal(n)      => Value.Real(n)
      case ValueBoolean(b)   => Value.Bool(b)
      case ValueNil          => Value.Nil
    }
    case SExpQuoted(q, pos) =>
      evalQuoted(
        SExpPair(SExpId(Identifier("quote", pos)), SExpPair(q, SExpValue(ValueNil, pos), pos), pos))
  }

  def primitive(name: String): Option[Value] = Primitives.primitiveMap.get(name).map(p => Value.Primitive(p))

  object Primitives {
    def primitiveMap: Map[String, Prim] = allPrimitives.map(p => (p.name, p)).toMap
    def allPrimitives: List[Prim] = {
    List(
      Times, /* [vv] *: Arithmetic */
      Plus, /* [vv] +: Arithmetic */
      Minus, /* [vv] -: Arithmetic */
      Div, /* [vx] /: Arithmetic (no support for fractions) */
      Abs, /* [vv] abs: Arithmetic */
      ACos, /* [vv] acos: Scientific */
      /* [x]  angle: Complex */
      /* [x]  append: Append/Reverse */
      /* [x]  apply: Fly Evaluation */
      ASin, /* [vv] asin: Scientific */
      Assoc, /* [vv] assoc: Retrieving Alist Entries */
      Assq, /* [vv] assq: Retrieving Alist Entries */
      /* [x]  assv: Retrieving Alist Entries */
      ATan, /* [vv] atan: Scientific */
      Booleanp, /* [vv] boolean?: Booleans */
      /* [x]  call-with-current-continuation: Continuations */
      /* [x]  call-with-input-file: File Ports */
      /* [x]  call-with-output-file: File Ports */
      /* [x]  call-with-values: Multiple Values */
      Car, /* [vv] car: Pairs */
      Cdr, /* [vv] cdr: Pairs */
      Ceiling, /* [vv] ceiling: Arithmetic */
      /* [x]  char->integer: Characters */
      /* [x]  char-alphabetic?: Characters */
      /* [x]  char-ci<=?: Characters */
      /* [x]  char-ci<?: Characters */
      /* [x]  char-ci=?: Characters */
      /* [x]  char-ci>=?: Characters */
      /* [x]  char-ci>?: Characters */
      /* [x]  char-downcase: Characters */
      /* [x]  char-lower-case?: Characters */
      /* [x]  char-numeric?: Characters */
      /* [x]  char-ready?: Reading */
      /* [x]  char-upcase: Characters */
      /* [x]  char-upper-case?: Characters */
      /* [x]  char-whitespace?: Characters */
      /* [x]  char<=?: Characters */
      /* [x]  char<?: Characters */
      /* [x]  char=?: Characters */
      /* [x]  char>=?: Characters */
      /* [x]  char>?: Characters */
      Charp, /* [vv] char?: Characters */
      /* [x]  close-input-port: Closing */
      /* [x]  close-output-port: Closing */
      /* [x]  complex?: Complex Numbers */
      Cons, /* [vv] cons: Pairs */
      Cos, /* [vv] cos: Scientific */
      /* [x]  current-input-port: Default Ports */
      /* [x]  current-output-port: Default Ports */
      Display, /* [v]  display: Writing */
      /* [x]  dynamic-wind: Dynamic Wind */
      /* [x]  eof-object?: Reading */
      Eq, /* [vv] eq?: Equality */
      Equal, /* [vx] equal?: Equality */
      /* [x]  eqv?: Equality */
      /* [x]  eval: Fly Evaluation */
      Evenp, /* [vv] even?: Integer Operations */
      ExactToInexact, /* [vv] exact->inexact: Exactness */
      /* [x]  exact?: Exactness */
      /* [x]  exp: Scientific */
      Expt, /* [vv] expt: Scientific */
      Floor, /* [vv] floor: Arithmetic */
      /* [x]  for-each: List Mapping */
      /* [x]  force: Delayed Evaluation */
      Gcd, /* [vx] gcd: Integer Operations */
      /* [x]  imag-part: Complex */
      InexactToExact, /* [vv] inexact->exact: Exactness */
      /* [x]  inexact?: Exactness */
      /* [x]  input-port?: Ports */
      /* [x]  integer->char: Characters */
      Integerp, /* [vv] integer?: Integers */
      /* [x]  interaction-environment: Fly Evaluation */
      /* [x]  lcm: Integer Operations */
      Length, /* [vv] length: List Selection */
      ListPrim, /* [vv] list: List Constructors */
      /* [x]  list->string: String Constructors */
      /* [x]  list->vector: Vector Creation */
      ListRef, /* [vv] list-ref: List Selection */
      /* [x]  list-tail: List Selection */
      Listp, /* [vv] list?: List Predicates */
      /* [x]  load: Loading */
      Log, /* [vv] log: Scientific */
      /* [x]  magnitude: Complex */
      /* [x]  make-polar: Complex */
      /* [x]  make-rectangular: Complex */
      /* [x]  make-string: String Constructors */
      /* [x]  map: List Mapping */
      Max, /* [vv] max: Arithmetic */
      Member, /* [vv] member: List Searching */
      Memq, /* [v]  memq: List Searching */
      /* [x]  memv: List Searching */
      Min, /* [vv] min: Arithmetic */
      Modulo, /* [vv] modulo: Integer Operations */
      Negativep, /* [vv] negative?: Comparison */
      Newline, /* [v]  newline: Writing */
      Not, /* [vv] not: Booleans */
      Nullp, /* [vv] null?: List Predicates */
      NumberToString, /* [vx] number->string: Conversion: does not support two arguments */
      Numberp, /* [vv] number?: Numerical Tower */
      Oddp, /* [vv] odd?: Integer Operations */
      /* [x]  open-input-file: File Ports */
      /* [x]  open-output-file: File Ports */
      /* [x]  output-port?: Ports */
      Pairp, /* [vv] pair?: Pairs */
      /* [x]  peek-char?: Reading */
      Positivep, /* [vv] positive?: Comparison */
      /* [x]  procedure?: Procedure Properties */
      // TODO Quotient, /* [vv] quotient: Integer Operations */
      /* [x]  rational?: Reals and Rationals */
      /* [x]  read: Scheme Read */
      /* [x]  read-char?: Reading */
      /* [x]  real-part: Complex */
      Realp, /* [vv] real?: Reals and Rationals */
      // TODO Remainder, /* [vv] remainder: Integer Operations */
      /* [x]  reverse: Append/Reverse */
      Round, /* [vv] round: Arithmetic */
      SetCar, /* [vv] set-car!: Pairs */
      SetCdr, /* [vv] set-cdr!: Pairs */
      Sin, /* [vv] sin: Scientific */
      Sqrt, /* [vv] sqrt: Scientific */
      /* [x]  string: String Constructors */
      /* [x]  string->list: List/String Conversion */
      /* [x]  string->number: Conversion */
      StringToSymbol, /* [vv] string->symbol: Symbol Primitives */
      StringAppend, /* [vx] string-append: Appending Strings: only two arguments supported */
      /* [x]  string-ci<: String Comparison */
      /* [x]  string-ci=?: String Comparison */
      /* [x]  string-ci>=?: String Comparison */
      /* [x]  string-ci>?: String Comparison */
      /* [x]  string-copy: String Selection */
      /* [x]  string-fill!: String Modification */
      StringLength, /* [vv] string-length: String Selection */
      StringRef, /* [x]  string-ref: String Selection */
      /* [x]  string-set!: String Modification */
      /* [x]  string<=?: String Comparison */
      StringLt, /* [vv]  string<?: String Comparison */
      /* [x]  string=?: String Comparison */
      /* [x]  string>=?: String Comparison */
      /* [x]  string>?: String Comparison */
      Stringp, /* [vv]  string?: String Predicates */
      /* [x]  substring: String Selection */
      SymbolToString, /* [vv] symbol->string: Symbol Primitives */
      Symbolp, /* [vv] symbol?: Symbol Primitives */
      Tan, /* [vv] tan: Scientific */
      /* [x]  truncate: Arithmetic */
      /* [x]  values: Multiple Values */
      MakeVector, /* [vv] make-vector: Vector Creation */
      Vector, /* [vv] vector: Vector Creation */
      /* [x]  vector->list: Vector Creation */
      /* [x]  vector-fill!: Vector Accessors */
      VectorLength, /* [vv] vector-length: Vector Accessors */
      VectorRef, /* [vv] vector-ref: Vector Accessors */
      VectorSet, /* [vv] vector-set!: Vector Accessors */
      Vectorp, /* [vv] vector?: Vector Creation */
      /* [x]  with-input-from-file: File Ports */
      /* [x]  with-output-to-file: File Ports */
      /* [x]  write-char: Writing */
      Zerop, /* [vv] zero?: Comparison */
      LessThan, /* [vv]  < */
      LessOrEqual, /* [vv]  <= */
      NumEq, /* [vv]  = */
      GreaterThan, /* [vv]  > */
      GreaterOrEqual, /* [vv]  >= */
      /* [x]  numerator */
      /* [x]  denominator */
      /* [x]  rationalize-string */
      /* [x]  scheme-report-environment */
      /* [x]  null-environment */
      /* [x]  write transcript-on */
      /* [x]  transcript-off */
      Caar,
      Cadr, /* [v]  caar etc. */
      Cdar,
      Cddr,
      Caaar,
      Caadr,
      Cadar,
      Caddr,
      Cdaar,
      Cdadr,
      Cddar,
      Cdddr,
      Caaaar,
      Caaadr,
      Caadar,
      Caaddr,
      Cadaar,
      Cadadr,
      Caddar,
      Cadddr,
      Cdaaar,
      Cdaadr,
      Cdadar,
      Cdaddr,
      Cddaar,
      Cddadr,
      Cdddar,
      Cddddr,
      /* Other primitives that are not R5RS */
      Random,
      Error
    )
    }

    abstract class SingleArgumentPrim(val name: String) extends Prim {
      def fun: PartialFunction[Value, Value]
      def call(args: List[Value]) = args match {
        case x :: Nil =>
          if (fun.isDefinedAt(x)) {
            fun(x)
          } else {
            throw new Exception(s"$name: invalid argument type $x")
          }
        case _ => throw new Exception(s"$name: invalid arguments $args")
      }
    }

    ////////////////
    // Arithmetic //
    ////////////////
    object Plus extends Prim {
      val name = "+"
      val default: Value = Value.Integer(0)
      def call(args: List[Value]) = args.foldLeft(default)({
          case (Value.Integer(n1), Value.Integer(n2)) => Value.Integer(n1 + n2)
          case (Value.Integer(n1), Value.Real(n2)) => Value.Real(n1 + n2)
          case (Value.Real(n1), Value.Integer(n2)) => Value.Real(n1 + n2)
          case (Value.Real(n1), Value.Real(n2)) => Value.Real(n1 + n2)
          case (x, y) => throw new Exception(s"+: invalid argument types ($x and $y)")
      })
    }
    object Times extends Prim {
      val name = "*"
      val default: Value = Value.Integer(1)
      def call(args: List[Value]) = args.foldLeft(default)({
        case (Value.Integer(n1), Value.Integer(n2)) => Value.Integer(n1 * n2)
          case (Value.Integer(n1), Value.Real(n2)) => Value.Real(n1 * n2)
          case (Value.Real(n1), Value.Integer(n2)) => Value.Real(n1 * n2)
          case (Value.Real(n1), Value.Real(n2)) => Value.Real(n1 * n2)
          case (x, y) => throw new Exception(s"+: invalid argument types ($x and $y)")
      })
    }
    object Minus extends Prim {
      val name = "-"
      def call(args: List[Value]) = args match {
        case Nil => throw new Exception("-: wrong number of arguments")
        case Value.Integer(x) :: Nil => Value.Integer(- x)
        case Value.Real(x) :: Nil => Value.Real(- x)
        case Value.Integer(x) :: rest => Plus.call(rest) match {
          case Value.Integer(y) => Value.Integer(x - y)
          case Value.Real(y) => Value.Real(x - y)
        }
        case Value.Real(x) :: rest => Plus.call(rest) match {
          case Value.Integer(y) => Value.Real(x - y)
          case Value.Real(y) => Value.Real(x - y)
        }
        case _ => throw new Exception(s"-: invalid arguments $args")
      }
    }
    object Div extends Prim {
      val name = "/"
      def call(args: List[Value]) = args match {
        case Nil => throw new Exception("/: wrong number of arguments")
        case Value.Integer(1) :: Nil => Value.Integer(1)
        case Value.Integer(x) :: Nil => Value.Real(1.0 / x)
        case Value.Real(x) :: Nil => Value.Real(1.0 / x)
        case Value.Integer(x) :: rest => Times.call(rest) match {
          case Value.Integer(y) => if (x % y == 0) { Value.Integer(x / y) } else { Value.Real(x.toDouble / y) }
          case Value.Real(y) => Value.Real(x / y)
        }
        case Value.Real(x) :: rest => Times.call(rest) match {
          case Value.Integer(y) => Value.Real(x / y)
          case Value.Real(y) => Value.Real(x / y)
        }
        case _ => throw new Exception(s"/: invalid arguments $args")
      }
    }

    object Modulo extends Prim {
      val name = "modulo"
      def call(args: List[Value]): Value = args match {
        case Value.Integer(x) :: Value.Integer(y) :: Nil =>
          Value.Integer(scalaam.lattice.MathOps.modulo(x, y))
        case _ => throw new Exception(s"modulo: invalid arguments $args")
      }
    }

    object Abs extends SingleArgumentPrim("abs") {
      def fun = {
        case Value.Integer(x) => Value.Integer(scala.math.abs(x))
        case Value.Real(x) => Value.Real(scala.math.abs(x))
      }
    }
    abstract class DoublePrim(name: String, f: Double => Double)
        extends SingleArgumentPrim(name) {
      def fun = {
        case Value.Real(x) => Value.Real(f(x))
        case Value.Integer(x) => Value.Real(f(x.toDouble))
      }
    }
    object Sin extends DoublePrim("sin", scala.math.sin)
    object ASin extends DoublePrim("asin", scala.math.asin)
    object Cos extends DoublePrim("cos", scala.math.cos)
    object ACos extends DoublePrim("acos", scala.math.acos)
    object Tan extends DoublePrim("tan", scala.math.tan)
    object ATan extends DoublePrim("atan", scala.math.atan)
    object Log extends DoublePrim("log", scala.math.log)

    object Sqrt extends SingleArgumentPrim("sqrt") {
      def fun = {
        case Value.Integer(x) if x < 0 => throw new Exception(s"sqrt: negative argument $x")
        case Value.Real(x) if x < 0 => throw new Exception(s"sqrt: negative argument $x")
        case Value.Integer(x) =>
          val r = scala.math.sqrt(x.toDouble)
          if (r == r.floor) {
            Value.Integer(r.toInt)
          } else {
            Value.Real(r)
          }
        case Value.Real(x) => Value.Real(scala.math.sqrt(x))
      }
    }
    object Expt extends Prim {
      val name = "expt"
      // TODO: expt should also preserve exactness if possible
      def call(args: List[Value]): Value = args match {
        case Value.Integer(x) :: Value.Integer(y) :: Nil =>
          Value.Integer(scala.math.pow(x.toDouble, y.toDouble).toInt)
        case Value.Integer(x) :: Value.Real(y) :: Nil =>
          Value.Real(scala.math.pow(x.toDouble, y))
        case Value.Real(x) :: Value.Integer(y) :: Nil =>
          Value.Real(scala.math.pow(x, y.toDouble))
        case Value.Real(x) :: Value.Real(y) :: Nil =>
          Value.Real(scala.math.pow(x, y))
        case _ => throw new Exception(s"expt: invalid arguments $args")
      }
    }

    object Ceiling extends SingleArgumentPrim("ceiling") {
      def fun = {
        case x: Value.Integer => x
        case Value.Real(x) => Value.Real(x.ceil)
      }
    }
    object Floor extends SingleArgumentPrim("floor") {
      def fun = {
        case x: Value.Integer => x
        case Value.Real(x) => Value.Real(x.floor)
      }
    }
    object Round extends SingleArgumentPrim("round") {
      def fun = {
        case x: Value.Integer => x
        case Value.Real(x) => Value.Real(scalaam.lattice.MathOps.round(x))
      }
    }
    object Evenp extends SingleArgumentPrim("even?") {
      def fun = {
        case Value.Integer(x) if x % 2 == 0 => Value.Bool(true)
        case _: Value.Integer => Value.Bool(false)
      }
    }
    object Oddp extends SingleArgumentPrim("even?") {
      def fun = {
        case Value.Integer(x) if x % 2 == 1 => Value.Bool(true)
        case _: Value.Integer => Value.Bool(false)
      }
    }
    object Negativep extends SingleArgumentPrim("negative?") {
      def fun = {
        case Value.Integer(x) if x < 0 => Value.Bool(true)
        case _: Value.Integer => Value.Bool(false)
      }
    }
    object Positivep extends SingleArgumentPrim("positive?") {
      def fun = {
        case Value.Integer(x) if x > 0 => Value.Bool(true)
        case _: Value.Integer => Value.Bool(false)
      }
    }
    object Zerop extends SingleArgumentPrim("zero??") {
      def fun = {
        case Value.Integer(0) => Value.Bool(true)
        case _: Value.Integer => Value.Bool(false)
      }
    }

    object Max extends Prim {
      val name = "max"
      def max(maximum: Value, rest: List[Value]): Value = rest match {
        case Nil => maximum
        case x :: rest => max(x match {
          case Value.Integer(n1) => maximum match {
            case Value.Integer(n2) => if (n1 > n2) { Value.Integer(n1) } else { maximum }
            case Value.Real(n2) =>
              val r = n1.toDouble
              if (r > n2) { Value.Real(r) } else { maximum }
          }
          case Value.Real(n1) => maximum match {
            case Value.Integer(n2) =>
              val r = n2.toDouble
              if (n1 > r) { Value.Real(n1) } else { maximum }
            case Value.Real(n2) => if (n1 > n2) { Value.Real(n1) } else { Value.Real(n2) }
          }
        }, rest)
      }
      def call(args: List[Value]) = args match {
        case Nil => throw new Exception("max: wrong number of arguments")
        case Value.Integer(first) :: rest =>
          max(Value.Integer(first), rest)
        case Value.Real(first) :: rest =>
          max(Value.Real(first), rest)
        case _ => throw new Exception(s"max: invalid arguments $args")
      }
    }
    object Min extends Prim {
      val name = "min"
      def min(minimum: Value, rest: List[Value]): Value = rest match {
        case Nil => minimum
        case x :: rest => min(x match {
          case Value.Integer(n1) => minimum match {
            case Value.Integer(n2) => if (n1 < n2) { Value.Integer(n1) } else { minimum }
            case Value.Real(n2) =>
              val r = n1.toDouble
              if (r < n2) { Value.Real(r) } else { minimum }
          }
          case Value.Real(n1) => minimum match {
            case Value.Integer(n2) =>
              val r = n2.toDouble
              if (n1 < r) { Value.Real(n1) } else { minimum }
            case Value.Real(n2) => if (n1 < n2) { Value.Real(n1) } else { Value.Real(n2) }
          }
        }, rest)
      }
      def call(args: List[Value]) = args match {
        case Nil => throw new Exception("min: wrong number of arguments")
        case Value.Integer(first) :: rest =>
          min(Value.Integer(first), rest)
        case Value.Real(first) :: rest =>
          min(Value.Real(first), rest)
        case _ => throw new Exception(s"min: invalid arguments $args")
      }
    }
    object Gcd extends Prim {
      val name = "gcd"
      def gcd(a: Int, b: Int): Int = if (b == 0) { a } else { gcd(b, a % b) }
      def call(args: List[Value]) = args match {
        case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Integer(gcd(x, y))
        case _ => throw new Exception(s"gcd: invalid arguments $args")
      }
    }

    object LessThan extends Prim {
      val name = "<"
      def call(args: List[Value]) = args match {
        case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x < y)
        case Value.Integer(x) :: Value.Real(y) :: Nil => Value.Bool(x < y)
        case Value.Real(x) :: Value.Integer(y) :: Nil => Value.Bool(x < y)
        case Value.Real(x) :: Value.Real(y) :: Nil => Value.Bool(x < y)
        case _ => throw new Exception(s"<: invalid arguments $args")
      }
    }

    object LessOrEqual extends Prim {
      val name = "<="
      def call(args: List[Value]) = args match {
        case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x <= y)
        case Value.Integer(x) :: Value.Real(y) :: Nil => Value.Bool(x <= y)
        case Value.Real(x) :: Value.Integer(y) :: Nil => Value.Bool(x <= y)
        case Value.Real(x) :: Value.Real(y) :: Nil => Value.Bool(x <= y)
        case _ => throw new Exception(s"<=: invalid arguments $args")
      }
    }

    object GreaterThan extends Prim {
      val name = ">"
      def call(args: List[Value]) = args match {
        case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x > y)
        case Value.Integer(x) :: Value.Real(y) :: Nil => Value.Bool(x > y)
        case Value.Real(x) :: Value.Integer(y) :: Nil => Value.Bool(x > y)
        case Value.Real(x) :: Value.Real(y) :: Nil => Value.Bool(x > y)
        case _ => throw new Exception(s">: invalid arguments $args")
      }
    }

    object GreaterOrEqual extends Prim {
      val name = ">="
      def call(args: List[Value]) = args match {
        case Value.Integer(x) :: Value.Integer(y) :: Nil => Value.Bool(x >= y)
        case Value.Integer(x) :: Value.Real(y) :: Nil => Value.Bool(x >= y)
        case Value.Real(x) :: Value.Integer(y) :: Nil => Value.Bool(x >= y)
        case Value.Real(x) :: Value.Real(y) :: Nil => Value.Bool(x >= y)
        case _ => throw new Exception(s">=: invalid arguments $args")
      }
    }

    object NumEq extends Prim {
      val name = "="
      def numEqInt(first: Int, l: List[Value]): Value = l match {
        case Nil => Value.Bool(true)
        case Value.Integer(x) :: rest if x == first => numEqInt(first, rest)
        case (_: Value.Integer) :: _ => Value.Bool(false)
        case Value.Real(x) :: rest if x == first => numEqInt(first, rest)
        case (_: Value.Real) :: _ => Value.Bool(false)
        case _ => throw new Exception(s"=: invalid type of arguments $l")
      }
      def numEqReal(first: Double, l: List[Value]): Value = l match {
        case Nil => Value.Bool(true)
        case Value.Integer(x) :: rest if x == first => numEqReal(first, rest)
        case (_: Value.Integer) :: _ => Value.Bool(false)
        case Value.Real(x) :: rest if x == first => numEqReal(first, rest)
        case (_: Value.Real) :: _ => Value.Bool(false)
        case _ => throw new Exception(s"=: invalid type of arguments $l")
      }
      def call(args: List[Value]) = args match {
        case Nil => Value.Bool(true)
        case Value.Integer(x) :: rest => numEqInt(x, rest)
        case Value.Real(x) :: rest => numEqReal(x, rest)
        case _ => throw new Exception(s"=: invalid type of arguments $args")
      }
    }

    //////////////
    // Booleans //
    //////////////
    object Not extends SingleArgumentPrim("not") {
      def fun = {
        case Value.Bool(b) => Value.Bool(!b)
        case _ => Value.Bool(false) /* any non-bool value is considered true */
      }
    }

    /////////////////
    // Conversions //
    /////////////////
    object ExactToInexact extends SingleArgumentPrim("exact->inexact") {
      def fun = {
        case Value.Integer(x) => Value.Real(x.toDouble)
        case x: Value.Real => x
      }
    }
    object InexactToExact extends SingleArgumentPrim("inexact->exact") {
      def fun = {
        case x: Value.Integer => x
        case Value.Real(x) => Value.Integer(x.toInt) /* TODO: fractions */
      }
    }
    object NumberToString extends SingleArgumentPrim("number->string") {
      def fun = {
        case Value.Integer(x) => Value.Str(s"$x")
        case Value.Real(x) => Value.Str(s"$x")
      }
    }
    object SymbolToString extends SingleArgumentPrim("symbol->string") {
      def fun = {
        case Value.Symbol(x) => Value.Str(x)
      }
    }
    object StringToSymbol extends SingleArgumentPrim("string->symbol") {
      def fun = {
        case Value.Str(x) => Value.Symbol(x)
      }
    }

    ////////
    // IO //
    ////////
    object Display extends SingleArgumentPrim("display") {
      def fun = {
        case x =>
          print(x)
          Value.Undefined(Position.none)
      }
    }
    object Newline extends Prim {
      val name = "newline"
      def call(args: List[Value]) = args match {
        case Nil =>
          println("")
          Value.Undefined(Position.none)
        case _ => throw new Exception(s"newline: wrong number of arguments, 0 expected, got ${args.length}")
      }
    }
    object Error extends Prim {
      val name = "error"
      def call(args: List[Value]) = throw new Exception(s"user-raised error: $args")
    }

    /////////////////
    // Type checks //
    /////////////////
    object Booleanp extends SingleArgumentPrim("boolean?") {
      def fun = {
        case _: Value.Bool => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Charp extends SingleArgumentPrim("char?") {
      def fun = {
        case _: Value.Character => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Nullp extends SingleArgumentPrim("null?") {
      def fun = {
        case Value.Nil => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Pairp extends SingleArgumentPrim("pair?") {
      def fun = {
        case _: Value.Cons => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Symbolp extends SingleArgumentPrim("symbol?") {
      def fun = {
        case _: Value.Symbol => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Stringp extends SingleArgumentPrim("string?") {
      def fun = {
        case _: Value.Str => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Integerp extends SingleArgumentPrim("integer?") {
      def fun = {
        case _: Value.Integer => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Realp extends SingleArgumentPrim("real?") {
      def fun = {
        case _: Value.Real => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Numberp extends SingleArgumentPrim("number?") {
      def fun = {
        case _: Value.Integer => Value.Bool(true)
        case _: Value.Real => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }
    object Vectorp extends SingleArgumentPrim("vector?") {
      def fun = {
        case _: Value.Vector => Value.Bool(true)
        case _ => Value.Bool(false)
      }
    }

    /////////////
    // Strings //
    /////////////
    object StringAppend extends Prim {
      val name = "string-append"
      def call(args: List[Value]) =
        Value.Str(args.foldLeft("")((acc, v) => v match {
          case Value.Str(x) => s"$acc$x"
          case _ => throw new Exception(s"string-append: invalid argument $v")
        }))
    }
    object StringLength extends SingleArgumentPrim("string-length") {
      def fun = {
        case Value.Str(x) => Value.Integer(x.length)
      }
    }
    object StringRef extends Prim {
      val name = "string-ref"
      def call(args: List[Value]) = args match {
        case Value.Str(x) :: Value.Integer(n) :: Nil =>
          Value.Character(x(n))
        case _ => throw new Exception(s"string-ref: invalid arguments $args")
      }
    }
    object StringLt extends Prim {
      val name = "string<?"
      def call(args: List[Value]) = args match {
        case Value.Str(x) :: Value.Str(y) :: Nil => Value.Bool(x < y)
        case _ => throw new Exception(s"string<?: invalid arguments $args")
      }
    }

    ///////////////
    // Equality //
    //////////////

    object Eq extends Prim {
      val name = "eq?"
      def call(args: List[Value]) = args match {
        case x :: y :: Nil => Value.Bool(x == y)
        case _ => throw new Exception(s"eq?: wrong number of arguments ${args.length}")
      }
    }
    object Equal extends Prim {
      val name = "equal?"
      def equal(x: Value, y: Value): Boolean = (x, y) match {
        case (x: Value.Clo, y: Value.Clo) => x == y
        case (x: Value.Primitive, y: Value.Primitive) => x == y
        case (x: Value.Str, y: Value.Str) => x == y
        case (x: Value.Symbol, y: Value.Symbol) => x == y
        case (x: Value.Integer, y: Value.Integer) => x == y
        case (x: Value.Real, y: Value.Real) => x == y
        case (x: Value.Bool, y: Value.Bool) => x == y
        case (x: Value.Character, y: Value.Character) => x == y
        case (Value.Nil, Value.Nil) => x == y
        case (Value.Cons(car1, cdr1), Value.Cons(car2, cdr2)) =>
          equal(store(car1), store(car2)) && equal(store(cdr1), store(cdr2))
        case (x: Value.Quoted, y: Value.Quoted) => x == y
        case (x: Value.Vector, y: Value.Vector) => x == y
        case _ => false
      }
      def call(args: List[Value]) = args match {
        case x :: y :: Nil => Value.Bool(equal(x, y))
        case _ => throw new Exception(s"equal?: wrong number of arguments ${args.length}")
      }
    }
    /////////////
    // Vectors //
    /////////////
    object Vector extends Prim {
      val name = "vector"
      def call(args: List[Value]): Value = Value.Vector(args.toArray)
    }
    object MakeVector extends Prim {
      val name = "make-vector"
      def call(args: List[Value]): Value = args match {
        case Value.Integer(size) :: init :: Nil =>
          Value.Vector(Array.fill(size)(init))
        case _ => throw new Exception(s"make-vector: invalid arguments $args")
      }
    }
    object VectorLength extends SingleArgumentPrim("vector-length") {
      def fun = {
        case Value.Vector(v) => Value.Integer(v.length)
      }
    }
    object VectorRef extends Prim {
      val name = "vector-ref"
      def call(args: List[Value]): Value = args match {
        case Value.Vector(v) :: Value.Integer(n) :: Nil =>
          v(n)
        case _ => throw new Exception(s"vector-ref: invalid arguments $args")
      }
    }
    object VectorSet extends Prim {
      val name = "vector-set!"
      def call(args: List[Value]): Value = args match {
        case Value.Vector(vec) :: Value.Integer(idx) :: v :: Nil =>
          vec(idx) = v
          Value.Undefined(Position.none)
        case _ => throw new Exception(s"vector-set!: invalid arguments $args")
      }
    }

    //////////
    // Cons //
    //////////
    object Car extends SingleArgumentPrim("car") {
      def fun = {
        case Value.Cons(car, _) => store(car)
      }
    }
    object Cdr extends SingleArgumentPrim("cdr") {
      def fun = {
        case Value.Cons(_, cdr) => store(cdr)
      }
    }
    class Cxr(name: String, ops: List[Prim]) extends SingleArgumentPrim(name) {
      def fun = {
        case v: Value.Cons =>
          val init: Value = v
          ops.reverse.foldLeft(init)((acc, op) => acc match {
            case _: Value.Cons => op.call(acc :: Nil)
            case _ => throw new Exception(s"$name: invalid list $acc")
          })
      }
    }
    object Caar extends Cxr("caar", List(Car, Car))
    object Cadr extends Cxr("cadr", List(Car, Cdr))
    object Cdar extends Cxr("cdar", List(Cdr, Car))
    object Cddr extends Cxr("cddr", List(Cdr, Cdr))

    object Caaar extends Cxr("caaar", List(Car, Caar))
    object Caadr extends Cxr("caadr", List(Car, Cadr))
    object Cadar extends Cxr("cadar", List(Car, Cdar))
    object Caddr extends Cxr("caddr", List(Car, Cddr))
    object Cdaar extends Cxr("cdaar", List(Cdr, Caar))
    object Cdadr extends Cxr("cdadr", List(Cdr, Cadr))
    object Cddar extends Cxr("cddar", List(Cdr, Cdar))
    object Cdddr extends Cxr("cdddr", List(Cdr, Cddr))

    object Caaaar extends Cxr("caaaar", List(Car, Caaar))
    object Caaadr extends Cxr("caaadr", List(Car, Caadr))
    object Caadar extends Cxr("caadar", List(Car, Cadar))
    object Caaddr extends Cxr("caaddr", List(Car, Caddr))
    object Cadaar extends Cxr("cadaar", List(Car, Cdaar))
    object Cadadr extends Cxr("cadadr", List(Car, Cdadr))
    object Caddar extends Cxr("caddar", List(Car, Cddar))
    object Cadddr extends Cxr("cadddr", List(Car, Cdddr))
    object Cdaaar extends Cxr("cdaaar", List(Cdr, Caaar))
    object Cdaadr extends Cxr("cdaadr", List(Cdr, Caadr))
    object Cdadar extends Cxr("cdadar", List(Cdr, Cadar))
    object Cdaddr extends Cxr("cdaddr", List(Cdr, Caddr))
    object Cddaar extends Cxr("cddaar", List(Cdr, Cdaar))
    object Cddadr extends Cxr("cddadr", List(Cdr, Cdadr))
    object Cdddar extends Cxr("cdddar", List(Cdr, Cddar))
    object Cddddr extends Cxr("cddddr", List(Cdr, Cdddr))


    object Cons extends Prim {
      val name = "cons"
      def call(args: List[Value]): Value = args match {
        case car :: cdr :: Nil =>
          allocateCons(car, cdr)
        case _ => throw new Exception(s"cons: wrong number of arguments $args")
      }
    }
    object SetCar extends Prim {
      val name = "set-car!"
      def call(args: List[Value]): Value = args match {
        case Value.Cons(car, _) :: v :: Nil =>
          extendStore(car, v)
          Value.Undefined(Position.none)
        case _ => throw new Exception(s"set-car!: invalid arguments $args")
      }
    }
    object SetCdr extends Prim {
      val name = "set-cdr!"
      def call(args: List[Value]): Value = args match {
        case Value.Cons(_, cdr) :: v :: Nil =>
          extendStore(cdr, v)
          Value.Undefined(Position.none)
        case _ => throw new Exception(s"set-cdr!: invalid arguments $args")
      }
    }

    ///////////
    // Lists //
    ///////////
    object ListPrim extends Prim {
      val name = "list"
      def call(args: List[Value]) = args match {
        case Nil => Value.Nil
        case head :: rest =>
          allocateCons(head, call(rest))
      }
    }
    object ListRef extends Prim {
      val name = "list-ref"
      def listRef(l: Value, n: Int): Value = (n, l) match {
        case (0, Value.Cons(car, _)) => store(car)
        case (_, Value.Cons(_, cdr)) => listRef(store(cdr), n-1)
        case _ => throw new Exception(s"list-ref: invalid list $l")
      }
      def call(args: List[Value]) = args match {
        case l :: Value.Integer(n) :: Nil => listRef(l, n)
        case _ => throw new Exception(s"list-ref: wrong number of arguments ${args.length}")
      }
    }
    object Length extends SingleArgumentPrim("length") {
      /* TODO: support circular lists */
      def length(l: Value, acc: Int = 0): Int = l match {
        case Value.Nil => acc
        case Value.Cons(_, cdr) => length(store(cdr), acc+1)
        case _ => throw new Exception(s"length: invalid list $l")
      }
      def fun = {
        case l => Value.Integer(length(l, 0))
      }
    }

    object Listp extends SingleArgumentPrim("list?") {
      def listp(l: Value): Boolean = l match {
        case Value.Nil => true
        case Value.Cons(_, cdr) => listp(store(cdr))
        case _ => false
      }
      def fun = {
        case l => Value.Bool(listp(l))
      }
    }
    class MemberLike(val name: String, eq: Prim) extends Prim {
      def member(e: Value, l: Value): Value = l match {
        case Value.Nil => Value.Bool(false)
        case Value.Cons(car, cdr) =>
          eq.call(e :: store(car) :: Nil) match {
            case Value.Bool(true) => l /* return the list upon success */
            case _ => member(e, store(cdr)) /* keep on searching */
          }
        case _ => throw new Exception(s"$name: malformed list $l")
      }
      def call(args: List[Value]): Value = args match {
        case e :: l :: Nil => member(e, l)
        case _ => throw new Exception(s"$name: invalid arguments $args")
      }
    }
    object Member extends MemberLike("member", Equal)
    object Memq extends MemberLike("memq", Eq)

    class AssocLike(val name: String, eq: Prim) extends Prim {
      def assoc(e: Value, l: Value): Value = l match {
        case Value.Nil => Value.Bool(false)
        case Value.Cons(car, cdr) =>
          store(car) match {
            case carv@Value.Cons(caar, _) =>
              eq.call(e :: store(caar) :: Nil) match {
                case Value.Bool(true) => carv /* found it, return the matching pair */
                case _ => assoc(e, store(cdr)) /* keep on looking */
              }
            case _ => throw new Exception(s"$name: malformed association list $l")
          }
        case _ => throw new Exception(s"$name: malformed list $l")
      }
      def call(args: List[Value]): Value = args match {
        case e :: l :: Nil => assoc(e, l)
        case _ => throw new Exception(s"$name: invalid arguments $args")
      }
    }
    object Assoc extends AssocLike("assoc", Equal)
    object Assq extends AssocLike("assq", Eq)

    ///////////
    // Other //
    ///////////
    object Random extends SingleArgumentPrim("random") {
      def fun = {
        case Value.Integer(x) => Value.Integer((scala.math.random() * x).toInt)
        case Value.Real(x) => Value.Real(scala.math.random() * x)
      }
    }
  }
}

object SchemeInterpreter {
  trait Value
  trait Prim {
    val name: String
    def call(args: List[Value]): Value
  }
  type Addr = Int
  type Env = Map[String, Addr]
  type Store = Map[Addr, Value]
  object Value {
    case class Undefined(pos: Position) extends Value /* arises from undefined behavior */
    case class Unbound(id: Identifier) extends Value /* only used for letrec */
    case class Clo(lambda: SchemeExp, env: Env) extends Value
    case class Primitive(p: Prim) extends Value
    case class Str(s: String) extends Value
    case class Symbol(sym: String) extends Value
    case class Integer(n: Int) extends Value
    case class Real(r: Double) extends Value
    case class Bool(b: Boolean) extends Value
    case class Character(c: Char) extends Value
    case object Nil extends Value
    /* TODO: not necessary to represent cons like this, we could just have mutable fields? */
    case class Cons(car: Addr, cdr: Addr) extends Value
    case class Quoted(quoted: SExp) extends Value
    case class Vector(elems: Array[Value]) extends Value
  }
}
