/**
  * Abstract syntax of Scheme programs (probably far from complete)
  */

sealed class SchemeExp
case class SchemeLambda(args: List[String], body: SchemeExp) extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
}
case class SchemeIf(cond: SchemeExp, cons: SchemeExp, alt: SchemeExp) extends SchemeExp {
  override def toString() = s"(if $cond $cons $alt)"
}
case class SchemeFuncall(f: SchemeExp, args: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    s"($f $a)"
  }
}
case class SchemeIdentifier(name: String) extends SchemeExp {
  override def toString() = name
}
case class SchemeValue(value: Value) extends SchemeExp {
  override def toString() = value.toString
  def isTrue = value match {
    case ValueBoolean(true) => true
    case _ => false
  }
}

object Scheme {
  /**
    * Compiles a s-expression into a scheme expression
    */
  def compile(exp: SExp): SchemeExp = exp match {
    case SExpIdentifier(name) => SchemeIdentifier(name)
    case SExpValue(value) => SchemeValue(value)
    case SExpPair(SExpIdentifier("if"),
      SExpPair(cond, SExpPair(cons, SExpPair(alt, SExpValue(ValueNil()))))) =>
      SchemeIf(compile(cond), compile(cons), compile(alt))
    case SExpPair(SExpIdentifier("if"),
      SExpPair(cond, SExpPair(cons, SExpValue(ValueNil())))) =>
      /* Empty else branch is replaced by #f */
      SchemeIf(compile(cond), compile(cons), SchemeValue(ValueBoolean(false)))
  }

  def interpret(exp: SchemeExp): SchemeValue = exp match {
    case v: SchemeValue => v
    case SchemeIf(cond, cons, alt) => if (interpret(cond).isTrue) {
      interpret(cons)
    } else {
      interpret(alt)
    }
  }
}
