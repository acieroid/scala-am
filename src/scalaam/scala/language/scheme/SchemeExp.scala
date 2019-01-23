package scalaam.language.scheme

import scalaam.core.{Position, NoPosition, Identifier}
import scalaam.language.sexp._

/**
  * Abstract syntax of Scheme programs (probably far from complete)
  */
trait SchemeExp {
  val pos: Position
}

/**
  * A lambda expression: (lambda (args...) body...)
  * Not supported: "rest"-arguments, of the form (lambda arg body), or (lambda (arg1 . args) body...)
  */
case class SchemeLambda(args: List[Identifier], body: List[SchemeExp], pos: Position)
    extends SchemeExp {
  require(body.nonEmpty)
  override def toString = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
  }
}

/**
  * A function call: (f args...)
  */
case class SchemeFuncall(f: SchemeExp, args: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
}

/**
  * An if statement: (if cond cons alt)
  * If without alt clauses need to be encoded with an empty begin as alt clause
  */
case class SchemeIf(cond: SchemeExp, cons: SchemeExp, alt: SchemeExp, pos: Position)
    extends SchemeExp {
  override def toString = s"(if $cond $cons $alt)"
}

/**
  * Let-bindings: (let ((v1 e1) ...) body...)
  */
case class SchemeLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position)
    extends SchemeExp {
  override def toString = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
}

/**
  * Let*-bindings: (let* ((v1 e1) ...) body...)
  */
case class SchemeLetStar(bindings: List[(Identifier, SchemeExp)],
                         body: List[SchemeExp],
                         pos: Position)
    extends SchemeExp {
  override def toString = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
}

/**
  * Letrec-bindings: (letrec ((v1 e1) ...) body...)
  */
case class SchemeLetrec(bindings: List[(Identifier, SchemeExp)],
                        body: List[SchemeExp],
                        pos: Position)
    extends SchemeExp {
  override def toString = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
}

/**
  * Named-let: (let name ((v1 e1) ...) body...)
  */
case class SchemeNamedLet(name: Identifier,
                          bindings: List[(Identifier, SchemeExp)],
                          body: List[SchemeExp],
                          pos: Position)
    extends SchemeExp {
  override def toString = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let $name ($bi) $bo)"
  }
}

/**
  * A set! expression: (set! variable value)
  */
case class SchemeSet(variable: Identifier, value: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(set! $variable $value)"
}

/**
  * A begin clause: (begin body...)
  */
case class SchemeBegin(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
}
/**
  * Used to create a begin if there are multiple statements, and a single exp if there is only one
 */
object SchemeBody {
  def apply(exps: List[SchemeExp]): SchemeExp = exps match {
    case Nil => SchemeValue(ValueBoolean(false), NoPosition) /* undefined */
    case exp :: Nil => exp
    case exp :: _ => SchemeBegin(exps, exp.pos)
  }
}

/**
  * A cond expression: (cond (test1 body1...) ...).
  * Desugared according to R5RS
 */
object SchemeCond {
  def apply(clauses: List[(SchemeExp, List[SchemeExp])], pos: Position): SchemeExp =
    if (clauses.isEmpty) {
      throw new Exception(s"Invalid Scheme cond without clauses ($pos)")
    } else {
      clauses.foldRight[SchemeExp](SchemeValue(ValueBoolean(false /* undefined */), NoPosition))((clause, acc) => clause match {
        case (SchemeValue(ValueBoolean(true), _), body) => SchemeBody(body)
        case (cond, body) => SchemeIf(cond, SchemeBody(body), acc, cond.pos)
      })
    }
}

/**
  * A case expression: (case key ((vals1...) body1...) ... (else default...))
  */
case class SchemeCase(key: SchemeExp,
                      clauses: List[(List[SchemeValue], List[SchemeExp])],
                      default: List[SchemeExp],
                      pos: Position)
    extends SchemeExp {
  override def toString = {
    val c = clauses
      .map({
        case (datums, cons) => {
          val d = datums.mkString(" ")
          val b = cons.mkString(" ")
          s"(($d) $b)"
        }
      })
      .mkString(" ")
    if (default.isEmpty) {
      s"(case $key $c)"
    } else {
      s"(case $key $c (else ${default.mkString(" ")}))"
    }
  }
}

/**
  * An and expression: (and exps...)
  */
case class SchemeAnd(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
}

/**
  * An or expression: (or exps...)
  */
case class SchemeOr(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
}

/**
  * A variable definition: (define name value)
  */
case class SchemeDefineVariable(name: Identifier, value: SchemeExp, pos: Position)
    extends SchemeExp {
  override def toString = s"(define $name $value)"
}

/**
  * A function definition: (define (name args...) body...)
  */
case class SchemeDefineFunction(name: Identifier,
                                args: List[Identifier],
                                body: List[SchemeExp],
                                pos: Position)
    extends SchemeExp {
  override def toString = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
}

/**
  * Do notation: (do ((<variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...)
  */
case class SchemeDo(vars: List[(Identifier, SchemeExp, Option[SchemeExp])],
                    test: SchemeExp,
                    finals: List[SchemeExp],
                    commands: List[SchemeExp],
                    pos: Position)
    extends SchemeExp {
  override def toString = {
    val varsstr     = vars.map({ case (v, i, s) => s"($v $i $s)" }).mkString(" ")
    val finalsstr   = finals.mkString(" ")
    val commandsstr = commands.mkString(" ")
    s"(do ($varsstr) ($test $finalsstr) $commandsstr)"
  }
}

/**
  * An identifier: name
  */
case class SchemeVar(id: Identifier) extends SchemeExp {
  val pos               = id.pos
  override def toString = id.name
}

/**
  * A quoted expression: '(foo (bar baz))
  *  The quoted expression is *not* converted to a Scheme expression, and remains
  * a simple s-expression, because that's exactly what it should be.
  */
case class SchemeQuoted(quoted: SExp, pos: Position) extends SchemeExp {
  override def toString = s"'$quoted"
}

/**
  * A literal value (number, symbol, string, ...)
  */
case class SchemeValue(value: Value, pos: Position) extends SchemeExp {
  override def toString = value.toString
}
