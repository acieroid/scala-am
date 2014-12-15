/**
  * Abstract syntax of ANF programs
  */

sealed class ANFExp
sealed class ANFAtomicExp extends ANFExp
case class ANFLambda(args: List[String], body: ANFExp) extends ANFAtomicExp {
  override def toString() = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
}
case class ANFIf(cond: ANFAtomicExp, cons: ANFExp, alt: ANFExp) extends ANFExp {
  override def toString() = s"(if $cond $cons $alt)"
}
case class ANFFuncall(f: ANFAtomicExp, args: List[ANFAtomicExp]) extends ANFExp {
  override def toString() = {
    val a = args.mkString(" ")
    s"($f $a)"
  }
}
case class ANFLet(variable: String, value: ANFExp, body: ANFExp) extends ANFExp {
  override def toString() = s"(let (($variable $value)) $body)"
}
case class ANFLetrec(variable: String, value: ANFExp, body: ANFExp) extends ANFExp {
  override def toString() = s"(letrec (($variable $value)) $body)"
}
case class ANFSet(variable: String, value: ANFAtomicExp) extends ANFExp {
  override def toString() = s"(set! $variable $value)"
}
case class ANFQuoted(quoted: SExp) extends ANFExp {
  override def toString() = s"'$quoted"
}
case class ANFIdentifier(name: String) extends ANFAtomicExp {
  override def toString() = name
}
case class ANFValue(value: Value) extends ANFAtomicExp {
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

  /* TODO: add tail-call information to know whether we can avoid introducing
   * non-needed lets? */
  def compile(exp: SchemeExp, tail: Boolean, k: ANFAtomicExp => ANFExp): ANFExp = exp match {
    case SchemeLambda(args, body) =>
      k(ANFLambda(args, compileBody(body, a => a)))
    case SchemeFuncall(f, args) =>
      compile(f, false, a => compileList(args, as => ret(ANFFuncall(a, as), tail, k)))
    case SchemeIf(cond, cons, alt) =>
      compile(cond, false, a => ret(ANFIf(a, compile(cons), compile(alt)), tail, k))
    case SchemeSet(variable, value) =>
      compile(value, false, a => ret(ANFSet(variable, a), tail, k))
    case SchemeBegin(body) =>
      compileBody(body, a => k(a))
    case SchemeIdentifier(name) =>
      k(ANFIdentifier(name))
    case SchemeQuoted(quoted) =>
      ret(ANFQuoted(quoted), tail, k)
    case SchemeValue(value) =>
      k(ANFValue(value))
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
}
