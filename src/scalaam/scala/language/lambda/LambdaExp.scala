package scalaam.language.lambda

import scalaam.core.{Position, Identifier}

trait LambdaExp {
  val pos: Position
}

case class LambdaFun(args: List[Identifier], body: LambdaExp, pos: Position) extends LambdaExp {
  override def toString = {
    val a = args.mkString(" ")
    s"(lambda ($a) $body)"
  }
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
}

case class LambdaVar(id: Identifier) extends LambdaExp {
  val pos               = id.pos
  override def toString = id.name
}
