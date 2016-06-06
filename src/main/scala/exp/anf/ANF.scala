/**
  * Abstract syntax of ANF programs
  */
import scala.util.parsing.input.Position

trait ANFExp {
  val pos: Position
}
object ANFExp {
  implicit val isExp: Expression[ANFExp] = new Expression[ANFExp] {
    def pos(e: ANFExp) = e.pos
  }
}
trait ANFAtomicExp extends ANFExp
case class ANFLambda(args: List[String], body: ANFExp, pos: Position) extends ANFAtomicExp {
  override def toString() = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
}
case class ANFIf(cond: ANFAtomicExp, cons: ANFExp, alt: ANFExp, pos: Position) extends ANFExp {
  override def toString() = s"(if $cond $cons $alt)"
}
case class ANFFuncall(f: ANFAtomicExp, args: List[ANFAtomicExp], pos: Position) extends ANFExp {
  override def toString() = {
    val a = args.mkString(" ")
    s"($f $a)"
  }
}
case class ANFLet(variable: String, value: ANFExp, body: ANFExp, pos: Position) extends ANFExp {
  override def toString() = s"(let (($variable $value)) $body)"
}
case class ANFLetrec(variable: String, value: ANFExp, body: ANFExp, pos: Position) extends ANFExp {
  override def toString() = s"(letrec (($variable $value)) $body)"
}
case class ANFSet(variable: String, value: ANFAtomicExp, pos: Position) extends ANFExp {
  override def toString() = s"(set! $variable $value)"
}
case class ANFQuoted(quoted: SExp, pos: Position) extends ANFExp {
  override def toString() = s"'$quoted"
}
case class ANFIdentifier(name: String, pos: Position) extends ANFAtomicExp {
  override def toString() = name
}
case class ANFValue(value: Value, pos: Position) extends ANFAtomicExp {
  override def toString() = value.toString
}

object ANFCompiler {
  /* TODO: do this in a functional way */
  var varCount = -1
  def newVar = {
    varCount = varCount + 1
    s"_$varCount"
  }

  def compile(exp: SchemeExp): ANFExp = compile(exp, true, e => e)

  def compile(exp: SchemeExp, tail: Boolean, k: ANFAtomicExp => ANFExp): ANFExp = exp match {
    case SchemeLambda(args, body, pos) =>
      k(ANFLambda(args, compileBody(body, pos, a => a), pos))
    case SchemeFuncall(f, args, pos) =>
      compile(f, false, a => compileList(args, as => ret(ANFFuncall(a, as, pos), tail, k)))
    case SchemeIf(cond, cons, alt, pos) =>
      compile(cond, false, a => ret(ANFIf(a, compile(cons), compile(alt), pos), tail, k))
    case SchemeLet(bindings, body, pos) =>
      bindings.reverse.foldLeft(compileBody(body, pos, a => a))(
        (e: ANFExp, binding: (String, SchemeExp)) =>
        ANFLet(binding._1, compile(binding._2), e, pos))
    case SchemeLetStar(bindings, body, pos) =>
      bindings.reverse.foldLeft(compileBody(body, pos, a => a))(
        (e: ANFExp, binding: (String, SchemeExp)) =>
        ANFLet(binding._1, compile(binding._2), e, pos))
    case SchemeLetrec(bindings, body, pos) =>
      /* TODO: we should at least warn when desugaring mutually-recursive functions,
       * as they are not supported. The best solution would be to desugar them
       * using set! to support them */
      bindings.reverse.foldLeft(compileBody(body, pos, a => a))(
        (e: ANFExp, binding: (String, SchemeExp)) =>
        ANFLetrec(binding._1, compile(binding._2), e, pos))
    case SchemeSet(variable, value, pos) =>
      compile(value, false, a => ret(ANFSet(variable, a, pos), tail, k))
    case SchemeBegin(body, pos) =>
      compileBody(body, pos, a => k(a))
    case SchemeDefineFunction(name, args, body, pos) =>
      compile(SchemeDefineVariable(name, SchemeLambda(args, body, pos), pos), tail, k)
    case SchemeDefineVariable(name, value, pos) =>
      throw new Exception("define not supported")
    case SchemeAnd(List(), pos) =>
      k(ANFValue(ValueBoolean(true), pos))
    case SchemeAnd(List(e), _) =>
      compile(e, tail, k)
    case SchemeAnd(e :: es, pos) =>
      compile(e, false, a => ret(ANFIf(a, compile(SchemeAnd(es, pos)), ANFValue(ValueBoolean(false), pos), pos), tail, k))
    case SchemeOr(List(), pos) =>
      k(ANFValue(ValueBoolean(false), pos))
    case SchemeOr(List(e), _) =>
      compile(e, tail, k)
    case SchemeOr(e :: es, pos) =>
      compile(e, false, a => ret(ANFIf(a, ANFValue(ValueBoolean(true), pos), compile(SchemeOr(es, pos)), pos), tail, k))
    case SchemeIdentifier(name, pos) =>
      k(ANFIdentifier(name, pos))
    case SchemeQuoted(quoted, pos) =>
      /* a quoted value is not atomic, as it may require an allocation to be evaluated */
      ret(ANFQuoted(quoted, pos), tail, k)
    case SchemeValue(value, pos) =>
      k(ANFValue(value, pos))
    case _ =>
      throw new Exception(s"Unhandled expression in ANF compiler: $exp")
  }

  /** Return a non-atomic value to a continuation, storing the result in a
    * let-bound variable. Don't call the continuation if tail is true. */
  def ret(e: ANFExp, tail: Boolean, k: ANFAtomicExp => ANFExp): ANFExp = {
    if (tail) {
      e
    } else {
      val v = newVar
      ANFLet(v, e, k(ANFIdentifier(v, e.pos)), e.pos)
    }
  }

  def compileBody(body: List[SchemeExp], pos: Position, k: ANFAtomicExp => ANFExp): ANFExp = body match {
    case Nil => throw new Exception("Cannot compile empty body")
    case e :: Nil => compile(e, true, a => a)
    case e :: rest => compile(e, false, a => {
      val v = newVar
      ANFLet(v, a, compileBody(rest, pos, a => a), pos)
    })
  }

  def compileList(exps: List[SchemeExp], k: List[ANFAtomicExp] => ANFExp): ANFExp =
    compileListHelper(exps, k, List())
  def compileListHelper(exps: List[SchemeExp], k: List[ANFAtomicExp] => ANFExp, acc: List[ANFAtomicExp]): ANFExp = exps match {
    case Nil => k(acc.reverse)
    case exp :: rest => compile(exp, false, e => compileListHelper(rest, k, e :: acc))
  }
}

object ANF {
  /**
   * Compile a Scheme expression into a mostly-equivalent ANF expression.
   * Input should be alpha-renamed before calling this function.
   * Incompatibilities are:
   *   - no support for mutual recursion
   */
  def compile(exp: SchemeExp): ANFExp = ANFCompiler.compile(exp)

  /**
   * Parse a string representing a Scheme program
   */
  def parse(s: String): ANFExp = compile(Scheme.rename(Scheme.parse(s)))
}
