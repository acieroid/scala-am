package scalaam.language.lambda

import scalaam.core.{Position, Identifier, Exp}

/**
  * We want to support lambda expressions, which all implement the LambdaExp
  * trait. Expressions in Scala-AM have to extend the Exp trait, defining a few
  * helper functions and fields, such as a `pos` field indicating the position
  * of the expression in the source file.
  */
trait LambdaExp extends Exp

/**
  * A function in lambda-calculus: `(lambda (x y) (+ x y)` is a function with `x`
  * and `y` as argument, and `(+ x y)` as body.
  */
case class LambdaFun(args: List[Identifier], body: LambdaExp, pos: Position) extends LambdaExp {
  override def toString = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
  /** This `fv` function is required by the Exp trait, and returns the list of
    * free variables in an expression */
  def fv = body.fv -- args.map(_.name).toSet
}

/**
  * A function call, (f x y) where f is the called function (an expression
  * itself), and both x and y are the arguments (expressions themselves).
  */
case class LambdaCall(f: LambdaExp, args: List[LambdaExp], pos: Position) extends LambdaExp {
  override def toString = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
  def fv = f.fv ++ args.flatMap(_.fv)
}

/**
  * A variable, such as `f`, `x` or `y`, where `id` is the identifier that
  * contains the name of the variable (an identifier is a name with a position)
  */
case class LambdaVar(id: Identifier) extends LambdaExp {
  val pos               = id.pos
  override def toString = id.name
  def fv = Set(id.name)
}
