package scalaam.language.scheme

import scalaam.core.{Position, NoPosition, Identifier, Exp}
import scalaam.language.sexp._

/**
  * Abstract syntax of Scheme programs
  */
trait SchemeExp extends Exp

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
  def fv = body.flatMap(_.fv).toSet -- args.map(_.name).toSet
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
  def fv = f.fv ++ args.flatMap(_.fv).toSet
}

/**
  * An if statement: (if cond cons alt)
  * If without alt clauses need to be encoded with an empty begin as alt clause
  */
case class SchemeIf(cond: SchemeExp, cons: SchemeExp, alt: SchemeExp, pos: Position)
    extends SchemeExp {
  override def toString = s"(if $cond $cons $alt)"
  def fv = cond.fv ++ cons.fv ++ alt.fv
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
  def fv = bindings.map(_._2).flatMap(_.fv).toSet ++ (body.flatMap(_.fv).toSet -- bindings.map(_._1.name).toSet)
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
  def fv = bindings.foldLeft((Set.empty[String] /* bound variables */, Set.empty[String] /* free variables */))((acc, binding) => binding match {
    case (id, e) => (acc._1 + id.name, acc._2 ++ (e.fv -- acc._1))
  })._2 ++ (body.flatMap(_.fv).toSet -- bindings.map(_._1.name).toSet)
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
  def fv = (bindings.map(_._2).flatMap(_.fv).toSet ++ body.flatMap(_.fv).toSet) -- bindings.map(_._1.name).toSet
}

/**
  * Named-let: (let name ((v1 e1) ...) body...)
  * TODO: desugar to letrec according to R5RS
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
  def fv = bindings.map(_._2).flatMap(_.fv).toSet ++ (body.flatMap(_.fv).toSet -- (bindings.map(_._1.name).toSet + name.name))
}

/**
  * A set! expression: (set! variable value)
  */
case class SchemeSet(variable: Identifier, value: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(set! $variable $value)"
  def fv = value.fv + variable.name
}

/**
  * A begin clause: (begin body...)
  */
case class SchemeBegin(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
  def fv = exps.flatMap(_.fv).toSet
}

/**
  * Used to create a begin if there are multiple statements, and a single exp if there is only one
  */
object SchemeBody {
  def apply(exps: List[SchemeExp]): SchemeExp = exps match {
    case Nil        => SchemeValue(ValueBoolean(false), NoPosition) /* undefined */
    case exp :: Nil => exp
    case exp :: _   => SchemeBegin(exps, exp.pos)
  }
}

/**
  * A cond expression: (cond (test1 body1...) ...).
  * Desugared according to R5RS.
  */
object SchemeCond {
  def apply(clauses: List[(SchemeExp, List[SchemeExp])], pos: Position): SchemeExp =
    if (clauses.isEmpty) {
      throw new Exception(s"Invalid Scheme cond without clauses ($pos)")
    } else {
      clauses.foldRight[SchemeExp](SchemeValue(ValueBoolean(false /* undefined */ ), NoPosition))(
        (clause, acc) =>
          clause match {
            case (SchemeValue(ValueBoolean(true), _), body) => SchemeBody(body)
            case (cond, Nil) =>
              /* Body is empty. R5RS states that "If the selected clause contains only the
               * test and no expressions ,then the value of the test is returned
               * as the result" */
              val id = Identifier("__cond-empty-body", cond.pos)
              SchemeLet(List((id, cond)), List(SchemeIf(SchemeVar(id), SchemeVar(id), acc, cond.pos)), cond.pos)
            case (cond, body)                               => SchemeIf(cond, SchemeBody(body), acc, cond.pos)
        })
    }
}

/**
  * A case expression: (case key ((vals1...) body1...) ... (else default...)).
  * Desugared according to R5RS.
  */
object SchemeCase {
  def apply(key: SchemeExp,
            clauses: List[(List[SchemeValue], List[SchemeExp])],
            default: List[SchemeExp],
            pos: Position): SchemeExp = key match {
    case _: SchemeVar | _: SchemeValue | SchemeQuoted(SExpId(_), _) =>
      /* Atomic key */
      val eqv = SchemeVar(Identifier("eq?", NoPosition)) /* TODO: should be eqv? instead of eq? */
      clauses.foldRight[SchemeExp](SchemeBody(default))(
        (clause, acc) =>
          /* In R5RS, the condition is desugared into a (memv key '(atoms ...)) call. This
           * would mean we would have to construct a list and go through it,
           * which would badly impact precision. Hence, we instead explicitly do
           * a big-or with eq? */
          SchemeIf(
            SchemeOr(
              clause._1.map(atom =>
                SchemeFuncall(eqv, List(key, atom match {
                  case SchemeValue(ValueSymbol(sym), pos) =>
                    SchemeQuoted(SExpId(Identifier(sym, pos)), pos)
                  case _ => atom
                }), atom.pos)),
              pos
            ),
            SchemeBody(clause._2),
            acc,
            pos
        ))
    case _ =>
      /* Non-atomic key, let-bind it */
      val id = Identifier("__case-atom-key", key.pos)
      SchemeLet(List((id, key)), List(SchemeCase(SchemeVar(id), clauses, default, pos)), key.pos)
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
  def fv = exps.flatMap(_.fv).toSet
}

/**
  * An or expression: (or exps...)
  */
case class SchemeOr(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
  def fv = exps.flatMap(_.fv).toSet
}

/**
  * A variable definition: (define name value)
  */
case class SchemeDefineVariable(name: Identifier, value: SchemeExp, pos: Position)
    extends SchemeExp {
  override def toString = s"(define $name $value)"
  def fv = value.fv
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
  def fv = body.flatMap(_.fv).toSet -- (args.map(_.name).toSet + name.name)
}

/**
  * Do notation: (do ((<variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...).
  * Desugared according to R5SR.
  */
object SchemeDo {
  def apply(vars: List[(Identifier, SchemeExp, Option[SchemeExp])],
            test: SchemeExp,
            finals: List[SchemeExp],
            commands: List[SchemeExp],
            pos: Position): SchemeExp = {
    val loopId = Identifier("__do_loop", pos)
    SchemeLetrec(
      List(
        (loopId,
         SchemeLambda(
           vars.map(_._1),
           List(SchemeIf(
             test,
             SchemeBody(finals),
             SchemeBody(commands :::
               List(SchemeFuncall(SchemeVar(loopId), vars.map({
               case (_, _, Some(step)) => step
               case (id, _, None)      => SchemeVar(id)
             }), pos))),
             pos
           )),
           pos
         ))),
      List(SchemeFuncall(SchemeVar(loopId), vars.map(_._2), pos)),
      pos
    )
  }
}

/**
  * An identifier: name
  */
case class SchemeVar(id: Identifier) extends SchemeExp {
  val pos               = id.pos
  override def toString = id.name
  def fv = Set(id.name)
}

/**
  * A quoted expression: '(foo (bar baz))
  *  The quoted expression is *not* converted to a Scheme expression, and remains
  * a simple s-expression, because that's exactly what it should be.
  */
case class SchemeQuoted(quoted: SExp, pos: Position) extends SchemeExp {
  override def toString = s"'$quoted"
  def fv = Set()
}

/**
  * A literal value (number, symbol, string, ...)
  */
case class SchemeValue(value: Value, pos: Position) extends SchemeExp {
  override def toString = value.toString
  def fv = Set()
}
