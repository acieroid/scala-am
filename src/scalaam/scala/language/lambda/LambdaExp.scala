package scalaam.language.lambda

import scalaam.core.{Position, Identifier, Exp}

trait LambdaExp extends Exp {
}

case class LambdaFun(args: List[Identifier], body: LambdaExp, pos: Position) extends LambdaExp {
  override def toString = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
  def fv = body.fv -- args.map(_.name).toSet
}

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

case class LambdaVar(id: Identifier) extends LambdaExp {
  val pos               = id.pos
  override def toString = id.name
  def fv = Set(id.name)
}
