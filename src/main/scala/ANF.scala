/**
  * Abstract syntax of ANF programs
  */

trait ANFExp extends scala.util.parsing.input.Positional
trait ANFAtomicExp extends ANFExp
case class ANFLambda(args: List[String], body: ANFExp) extends ANFAtomicExp {
  override def equals(that: Any) = that.isInstanceOf[ANFLambda] && pos == that.asInstanceOf[ANFLambda].pos && super.equals(that)
  override def toString() = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
}
case class ANFIf(cond: ANFAtomicExp, cons: ANFExp, alt: ANFExp) extends ANFExp {
  override def equals(that: Any) = that.isInstanceOf[ANFIf] && pos == that.asInstanceOf[ANFIf].pos && super.equals(that)
  override def toString() = s"(if $cond $cons $alt)"
}
case class ANFFuncall(f: ANFAtomicExp, args: List[ANFAtomicExp]) extends ANFExp {
  override def equals(that: Any) = that.isInstanceOf[ANFFuncall] && pos == that.asInstanceOf[ANFFuncall].pos && super.equals(that)
  override def toString() = {
    val a = args.mkString(" ")
    s"($f $a)"
  }
}
case class ANFLet(variable: String, value: ANFExp, body: ANFExp) extends ANFExp {
  override def equals(that: Any) = that.isInstanceOf[ANFLet] && pos == that.asInstanceOf[ANFLet].pos && super.equals(that)
  override def toString() = s"(let (($variable $value)) $body)"
}
case class ANFLetrec(variable: String, value: ANFExp, body: ANFExp) extends ANFExp {
  override def equals(that: Any) = that.isInstanceOf[ANFLetrec] && pos == that.asInstanceOf[ANFLetrec].pos && super.equals(that)
  override def toString() = s"(letrec (($variable $value)) $body)"
}
case class ANFSet(variable: String, value: ANFAtomicExp) extends ANFExp {
  override def equals(that: Any) = that.isInstanceOf[ANFSet] && pos == that.asInstanceOf[ANFSet].pos && super.equals(that)
  override def toString() = s"(set! $variable $value)"
}
case class ANFQuoted(quoted: SExp) extends ANFExp {
  override def equals(that: Any) = that.isInstanceOf[ANFQuoted] && pos == that.asInstanceOf[ANFQuoted].pos && super.equals(that)
  override def toString() = s"'$quoted"
}
case class ANFIdentifier(name: String) extends ANFAtomicExp {
  override def equals(that: Any) = that.isInstanceOf[ANFIdentifier] && pos == that.asInstanceOf[ANFIdentifier].pos && super.equals(that)
  override def toString() = name
}
case class ANFValue(value: Value) extends ANFAtomicExp {
  override def equals(that: Any) = that.isInstanceOf[ANFValue] && pos == that.asInstanceOf[ANFValue].pos && super.equals(that)
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
    case SchemeLambda(args, body) =>
      k(ANFLambda(args, compileBody(body, a => a)).setPos(exp.pos))
    case SchemeFuncall(f, args) =>
      compile(f, false, a => compileList(args, as => ret(ANFFuncall(a, as).setPos(exp.pos), tail, k)))
    case SchemeIf(cond, cons, alt) =>
      compile(cond, false, a => ret(ANFIf(a, compile(cons), compile(alt)).setPos(exp.pos), tail, k))
    case SchemeLet(bindings, body) =>
      bindings.reverse.foldLeft(compileBody(body, a => a))(
        (e: ANFExp, binding: (String, SchemeExp)) =>
        ANFLet(binding._1, compile(binding._2), e).setPos(exp.pos))
    case SchemeLetStar(bindings, body) =>
      bindings.reverse.foldLeft(compileBody(body, a => a))(
        (e: ANFExp, binding: (String, SchemeExp)) =>
        ANFLet(binding._1, compile(binding._2), e).setPos(exp.pos))
    case SchemeLetrec(bindings, body) =>
      /* TODO: we should at least warn when desugaring mutually-recursive functions,
       * as they are not supported. The best solution would be to desugar them
       * using set! to support them */
      bindings.reverse.foldLeft(compileBody(body, a => a))(
        (e: ANFExp, binding: (String, SchemeExp)) =>
        ANFLetrec(binding._1, compile(binding._2), e).setPos(exp.pos))
    case SchemeSet(variable, value) =>
      compile(value, false, a => ret(ANFSet(variable, a).setPos(exp.pos), tail, k))
    case SchemeBegin(body) =>
      compileBody(body, a => k(a))
    case SchemeDefineFunction(name, args, body) =>
      compile(SchemeDefineVariable(name, SchemeLambda(args, body)).setPos(exp.pos), tail, k)
    case SchemeDefineVariable(name, value) =>
      throw new Exception("define not supported")
    case SchemeAnd(List()) =>
      k(ANFValue(ValueBoolean(true)).setPos(exp.pos))
    case SchemeAnd(List(e)) =>
      compile(e, tail, k)
    case SchemeAnd(e :: es) =>
      compile(e, false, a => ret(ANFIf(a, compile(SchemeAnd(es).setPos(exp.pos)), ANFValue(ValueBoolean(false)).setPos(exp.pos)), tail, k))
    case SchemeOr(List()) =>
      k(ANFValue(ValueBoolean(false)).setPos(exp.pos))
    case SchemeOr(List(e)) =>
      compile(e, tail, k)
    case SchemeOr(e :: es) =>
      compile(e, false, a => ret(ANFIf(a, ANFValue(ValueBoolean(true)).setPos(exp.pos), compile(SchemeOr(es).setPos(exp.pos))).setPos(exp.pos), tail, k))
    case SchemeIdentifier(name) =>
      k(ANFIdentifier(name).setPos(exp.pos))
    case SchemeQuoted(quoted) =>
      /* a quoted value is not atomic, as it may require an allocation to be evaluated */
      ret(ANFQuoted(quoted).setPos(exp.pos), tail, k)
    case SchemeValue(value) =>
      k(ANFValue(value).setPos(exp.pos))
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
      ANFLet(v, e, k(ANFIdentifier(v)))
    }
  }

  def compileBody(body: List[SchemeExp], k: ANFAtomicExp => ANFExp): ANFExp = body match {
    case Nil => throw new Exception("Cannot compile empty body")
    case e :: Nil => compile(e, true, a => a)
    case e :: rest => compile(e, false, a => {
      val v = newVar
      ANFLet(v, a, compileBody(rest, a => a))
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
