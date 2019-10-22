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
      case Value.Undefined(pos) => println(s"Undefined behavior arising from position $pos seen at ${e.pos}")
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
            val v = eval(arg._2, env)
            store = store + (addr -> v)
            (env3 + (arg._1.name -> addr))
          })
          eval(SchemeBegin(body, pos2), envExt)
        case Value.Primitive(p@_) => ???
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
        store = store + (addr -> eval(binding._2, env))
        (env2 + (binding._1.name -> addr))
      })
      eval(SchemeBegin(body, pos), envExt)
    case SchemeLetStar(bindings, body, pos) =>
      val envExt = bindings.foldLeft(env)((env2, binding) => {
        val addr = newAddr()
        store = store + (addr -> eval(binding._2, env2 /* this is the difference with let */))
        (env2 + (binding._1.name -> addr))
      })
      eval(SchemeBegin(body, pos), envExt)
    case SchemeLetrec(bindings, body, pos) =>
      val envExt = bindings.foldLeft(env)((env2, binding) => {
        val addr = newAddr()
        /* These are the differences with let* (store and env) */
        store = store + (addr -> Value.Unbound(binding._1))
        val env3 = env2 + (binding._1.name -> addr)
        store = store + (addr -> eval(binding._2, env3))
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
      store = store + (addr -> eval(v, env))
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
      Value.Quoted(quoted)
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

  def primitive(name: String): Option[Value] = name match {
    case "+" => Some(Value.Primitive(Primitives.Plus))
    case _ => None
  }

  object Primitives {
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
    object Ceiling extends SingleArgumentPrim("ceiling") {
      def fun = {
        case x: Value.Integer => x
        case Value.Real(x) => Value.Real(x.ceil)
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
    object Error extends SingleArgumentPrim("error") {
      def fun = {
        case x => throw new Exception(s"user-raised error: $x")
      }
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
    object Not extends SingleArgumentPrim("not") {
      def fun = {
        case Value.Bool(b) => Value.Bool(!b)
        case _ => Value.Bool(false) /* any non-bool value is considered true */
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
    // TODO: make-vector
    // TODO: vector
    // TODO: vector-length
    // TODO: vector-ref
    // TODO: vector-set
    // vectorp TODO

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
    object Caar extends SingleArgumentPrim("caar") {
      def fun = {
        case Value.Cons(car, _) => store(car) match {
          case Value.Cons(caar, _) => store(caar)
          case _ => throw new Exception(s"caar: invalid list")
        }
      }
    }
    object Cadr extends SingleArgumentPrim("cadr") {
      def fun = {
        case Value.Cons(car, _) => store(car) match {
          case Value.Cons(_, cadr) => store(cadr)
          case _ => throw new Exception(s"cadr: invalid list")
        }
      }
    }
    object Cdar extends SingleArgumentPrim("cdar") {
      def fun = {
        case Value.Cons(_, cdr) => store(cdr) match {
          case Value.Cons(cdar, _) => store(cdar)
          case _ => throw new Exception(s"cdar: invalid list")
        }
      }
    }
    object Cddr extends SingleArgumentPrim("cddr") {
      def fun = {
        case Value.Cons(_, cdr) => store(cdr) match {
          case Value.Cons(_, cddr) => store(cddr)
          case _ => throw new Exception(s"caar: invalid list")
        }
      }
    }
    object Cons extends Prim {
      val name = "cons"
      def call(args: List[Value]): Value = args match {
        case car :: cdr :: Nil =>
          val cara = newAddr()
          val cdra = newAddr()
          store = store + (cara -> car) + (cdra -> cdr)
          Value.Cons(cara, cdra)
        case _ => throw new Exception(s"cons: wrong number of arguments $args")
      }
    }
    object SetCar extends Prim {
      val name = "set-car!"
      def call(args: List[Value]): Value = args match {
        case Value.Cons(car, _) :: v :: Nil =>
          store = store + (car -> v)
          Value.Undefined(Position.none)
        case _ => throw new Exception(s"set-car!: invalid arguments $args")
      }
    }
    object SetCdr extends Prim {
      val name = "set-cdr!"
      def call(args: List[Value]): Value = args match {
        case Value.Cons(_, cdr) :: v :: Nil =>
          store = store + (cdr -> v)
          Value.Undefined(Position.none)
        case _ => throw new Exception(s"set-cdr!: invalid arguments $args")
      }
    }

    ///////////
    // Lists //
    ///////////
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
    case class Symbol(s: String) extends Value
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
