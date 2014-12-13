/**
  * Abstract syntax of Scheme programs (probably far from complete)
  */

sealed class SchemeExp
case class SchemeLambda(args: List[String], body: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
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
case class SchemeLet(bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
}
case class SchemeLetStar(bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
}
case class SchemeLetrec(bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
}
case class SchemeSet(variable: String, value: SchemeExp) extends SchemeExp {
  override def toString() = s"(set! $variable $value)"
}
case class SchemeBegin(exps: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
}
case class SchemeCond(clauses: List[(SchemeExp, List[SchemeExp])]) extends SchemeExp {
  override def toString() = {
    val c = clauses.map({ case (cond, cons) => {
      val b = cons.mkString(" ")
      s"($cond $b)"
    }}).mkString(" ")
    s"(cond $c)"
  }
}
/*
case class SchemeCase(key: SchemeExp, clauses: List[(List[SchemeValue], List[SchemeExp])]) extends SchemeExp {
  override def toString() = {
    val c = clauses.map({ case (datums, cons) => {
      val d = datums.mkString(" ")
      val b = cons.mkString(" ")
      s"(($d) $b)"
    }}).mkString(" ")
    s"(case $key $c)"
  }
}
*/
case class SchemeAnd(exps: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
}
case class SchemeOr(exps: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
}
case class SchemeDefineVariable(name: String, value: SchemeExp) extends SchemeExp {
  override def toString() = s"(define $name $value)"
}
case class SchemeDefineFunction(name: String, args: List[String], body: List[SchemeExp]) extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
}
case class SchemeIdentifier(name: String) extends SchemeExp {
  override def toString() = name
}
case class SchemeQuoted(quoted: SchemeExp) extends SchemeExp {
  override def toString() = s"'$quoted"
}
case class SchemeValue(value: Value) extends SchemeExp {
  override def toString() = value.toString
}

object Scheme {
  /**
    * Reserved keywords
    */
  val reserved: List[String] = List("lambda", "if", "let", "let*", "letrec", "cond", "case", "set!", "begin", "define")
  /**
    * Compiles a s-expression into a scheme expression
    */
  def compile(exp: SExp): SchemeExp = exp match {
    case SExpPair(SExpIdentifier("lambda"),
      SExpPair(args, SExpPair(first, rest))) =>
      SchemeLambda(compileArgs(args), compile(first) :: compileBody(rest))
    case SExpPair(SExpIdentifier("lambda"), _) =>
      throw new Exception(s"Invalid Scheme lambda: $exp")
    case SExpPair(SExpIdentifier("if"),
      SExpPair(cond, SExpPair(cons, SExpPair(alt, SExpValue(ValueNil()))))) =>
      SchemeIf(compile(cond), compile(cons), compile(alt))
    case SExpPair(SExpIdentifier("if"),
      SExpPair(cond, SExpPair(cons, SExpValue(ValueNil())))) =>
      /* Empty else branch is replaced by #f */
      SchemeIf(compile(cond), compile(cons), SchemeValue(ValueBoolean(false)))
    case SExpPair(SExpIdentifier("if"), _) =>
      throw new Exception(s"Invalid Scheme if: $exp")
    case SExpPair(SExpIdentifier("let"),
      SExpPair(bindings, SExpPair(first, rest))) =>
      SchemeLet(compileBindings(bindings), compile(first) :: compileBody(rest))
    case SExpPair(SExpIdentifier("let"), _) =>
      throw new Exception(s"Invalid Scheme let: $exp")
    case SExpPair(SExpIdentifier("let*"),
      SExpPair(bindings, SExpPair(first, rest))) =>
      SchemeLetStar(compileBindings(bindings), compile(first) :: compileBody(rest))
    case SExpPair(SExpIdentifier("let*"), _) =>
      throw new Exception(s"Invalid Scheme let*: $exp")
    case SExpPair(SExpIdentifier("letrec"),
      SExpPair(bindings, SExpPair(first, rest))) =>
      SchemeLetrec(compileBindings(bindings), compile(first) :: compileBody(rest))
    case SExpPair(SExpIdentifier("letrec"), _) =>
      throw new Exception(s"Invalid Scheme letrec: $exp")
    case SExpPair(SExpIdentifier("set!"),
      SExpPair(SExpIdentifier(variable), SExpPair(value, SExpValue(ValueNil())))) =>
      SchemeSet(variable, compile(value))
    case SExpPair(SExpIdentifier("set!"), _) =>
      throw new Exception(s"Invalid Scheme set!: $exp")
    case SExpPair(SExpIdentifier("begin"), body) =>
      SchemeBegin(compileBody(body))
    case SExpPair(SExpIdentifier("cond"), clauses) =>
      SchemeCond(compileCondClauses(clauses))
    case SExpPair(SExpIdentifier("case"), _) =>
      throw new Exception(s"TODO: case not yet handled")
    case SExpPair(SExpIdentifier("and"), args) =>
      SchemeAnd(compileBody(args))
    case SExpPair(SExpIdentifier("or"), args) =>
      SchemeOr(compileBody(args))
    case SExpPair(SExpIdentifier("define"),
      SExpPair(SExpIdentifier(name), SExpPair(value, SExpValue(ValueNil())))) =>
      SchemeDefineVariable(name, compile(value))
    case SExpPair(SExpIdentifier("define"),
      SExpPair(SExpPair(SExpIdentifier(name), args),
        SExpPair(first, rest))) =>
      SchemeDefineFunction(name, compileArgs(args), compile(first) :: compileBody(rest))
    case SExpPair(f, args) =>
      SchemeFuncall(compile(f), compileBody(args))
    case SExpIdentifier(name) => if (reserved.contains(name)) {
      throw new Exception(s"Invalid Scheme identifier (reserved): $exp")
    } else {
      SchemeIdentifier(name)
    }
    case SExpValue(value) => SchemeValue(value)
    case SExpQuoted(quoted) => SchemeQuoted(compile(quoted))
    case _ => throw new Exception(s"Invalid Scheme expression: $exp")
  }

  def compileArgs(args: SExp): List[String] = args match {
    case SExpPair(SExpIdentifier(id), rest) => id :: compileArgs(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme argument list: $args")
  }

  def compileBody(body: SExp): List[SchemeExp] = body match {
    case SExpPair(exp, rest) => compile(exp) :: compileBody(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme body: $body")
  }

  def compileBindings(bindings: SExp): List[(String, SchemeExp)] = bindings match {
    case SExpPair(SExpPair(SExpIdentifier(name),
      SExpPair(value, SExpValue(ValueNil()))), rest) =>
      (name, compile(value)) :: compileBindings(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme bindings: $bindings")
  }

  def compileCondClauses(clauses: SExp): List[(SchemeExp, List[SchemeExp])] = clauses match {
    case SExpPair(SExpPair(cond, SExpPair(first, rest)), restClauses) =>
      (compile(cond), compile(first) :: compileBody(rest)) :: compileCondClauses(restClauses)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme cond clauses: $clauses")
  }
}
