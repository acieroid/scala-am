package scalaam.language.scheme

import scalaam.core._
import scalaam.language.sexp._

/** This is an interpreter that, given a program and the result of an analysis of that program, will check that the results agree with what we see in the program's execution */
/* TODO: this should actually not compare but just allow to do anything with the values (e.g., log or compare) */
object SchemeInterpreter {

  trait Value
  trait Prim {
    def call(args: List[Value]): Value
  }
  type Addr = Int
  type Env = Map[String, Addr]
  type Store = Map[Addr, Value]
  object Value {
    case class Undefined(pos: Position) extends Value /* arises from undefined behavior */
    case class Unbound(id: Identifier) extends Value
    case class Clo(lambda: SchemeExp, env: Env) extends Value
    case class Primitive(p: Prim) extends Value
    case class Str(s: String) extends Value
    case class Symbol(s: String) extends Value
    case class Integer(n: Int) extends Value
    case class Real(r: Double) extends Value
    case class Bool(b: Boolean) extends Value
    case class Character(c: Char) extends Value
    case object Nil extends Value
    case class Cons(car: Value, cdr: Value) extends Value
    case class Quoted(quoted: SExp) extends Value
  }

  type Callback = (Position, Value) => Unit
  /**
    * Evaluates `program`.
    * Will check the analysis result by calling `compare` on all encountered values.
    */
  def run(program: SchemeExp, callback: Callback): Value = {
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
    def eval(e: SchemeExp, env: Env): Value = { check(e, e match {
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
    }
    eval(program, Map.empty)
  }

  def primitive(name: String): Option[Value] = name match {
    case "+" => Some(Value.Primitive(Prim.Plus))
    case _ => None
  }

  object Prim {
    case object Plus extends Prim {
      val default: Value = Value.Integer(0)
      def call(args: List[Value]) = args.foldLeft(default)({
          case (Value.Integer(n1), Value.Integer(n2)) => Value.Integer(n1 + n2)
          case (Value.Integer(n1), Value.Real(n2)) => Value.Real(n1 + n2)
          case (Value.Real(n1), Value.Integer(n2)) => Value.Real(n1 + n2)
          case (Value.Real(n1), Value.Real(n2)) => Value.Real(n1 + n2)
          case (x, y) => throw new Exception(s"+: invalid argument types ($x and $y)")
      })
    }
    case object Times extends Prim {
      val default: Value = Value.Integer(1)
      def call(args: List[Value]) = args.foldLeft(default)({
        case (Value.Integer(n1), Value.Integer(n2)) => Value.Integer(n1 * n2)
          case (Value.Integer(n1), Value.Real(n2)) => Value.Real(n1 * n2)
          case (Value.Real(n1), Value.Integer(n2)) => Value.Real(n1 * n2)
          case (Value.Real(n1), Value.Real(n2)) => Value.Real(n1 * n2)
          case (x, y) => throw new Exception(s"+: invalid argument types ($x and $y)")
      })
    }
  }
}
