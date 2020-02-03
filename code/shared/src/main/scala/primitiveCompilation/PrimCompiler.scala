package primitiveCompilation

import primitiveCompilation.PrimSource.{OpCall, _}
import primitiveCompilation.PrimTarget._
import scalaam.core.Identifier
import scalaam.language.scheme._
import scalaam.language.sexp._

object PrimCompiler {

  type SE = PrimSource.Exp
  type TE = PrimTarget.Exp

  type SA = PrimSource.AExp
  type TA = PrimTarget.AExp

  /////////////////////////////
  // FIRST COMPILATION PHASE //
  /////////////////////////////

  case class UnsupportedExpressionException(e: SchemeExp) extends Exception
  case class ShouldNotEncounterException(id: String) extends Exception

  private def valueToSource(v: Value): SE = v match {
    case ValueBoolean(bool) => AE(Boo(bool))
    case ValueInteger(int) => AE(Num(int))
  }

  private def toSource(exp: SchemeExp, recursive: Boolean = true): SE = exp match {
    case SchemeFuncall(f, args, _) => ???
    case SchemeIf(cond, cons, alt, _) => toSource(cond) match {
      case AE(ae) => If(ae, toSource(cons), toSource(alt))
      case _ => throw UnsupportedExpressionException(cond) // TODO: support?
    }
    // For now, let behaves as a let* TODO?
    // Restrain a body to a single expression... Important: use foldRight!
    case SchemeLet(bnd, body :: Nil, _) => bnd.foldRight(toSource(body)){ case ((id, bnd), acc) => Let(PrimSource.Var(Id(id.name)), toSource(bnd), acc) }
    case SchemeLetStar(bnd, body :: Nil, _) => bnd.foldRight(toSource(body)){ case ((id, bnd), acc) => Let(PrimSource.Var(Id(id.name)), toSource(bnd), acc) }
    // Dysfunctional begin now.
    case SchemeBegin(e :: Nil, _) => toSource(e)
    case SchemeDefineVariable(id, exp, _) => Let(PrimSource.Var(Id(id.name)), toSource(exp), ???) // TODO
    case SchemeDefineFunction(_, _, body :: Nil, _) if !recursive => toSource(body) // Allow a top level define (prim = function definition)
    case SchemeVar(_) => ??? // TODO: choose between Var and Arg (=> remove distinction?)
    case SchemeValue(v, _) => valueToSource(v)
    case Identifier(nam, _) => throw ShouldNotEncounterException(nam)
    case e => throw UnsupportedExpressionException(e)
  }

  def toSource(exp: SchemeExp): SE = toSource(exp, false)

  //////////////////////////////
  // SECOND COMPILATION PHASE //
  //////////////////////////////

  def AExpToTarget(ae: SA): TA = ae match {
    case PrimSource.Arg(a) => PrimTarget.Arg(a)
    case PrimSource.Boo(b) => PrimTarget.Boo(b)
    case PrimSource.Num(n) => PrimTarget.Num(n)
    case PrimSource.Var(v) => PrimTarget.Var(v)
  }

  def varToTarget(v: PrimSource.Var): PrimTarget.Var = PrimTarget.Var(v.v)

  def toTarget(exp: SE): TE = exp match {
    case AE(ae) => Lat(Inj(AExpToTarget(ae)))
    case If(cond, cons, alt) =>
      val v0 = CVa()
      val v1 = CVa()
      val v2 = CVa()
      BindC(v0, Lat(Inj(AExpToTarget(cond))),
            BindC(v1, IfTrue(Inj(v0), toTarget(cons)),
                  BindC(v2, IfFalse(Inj(v0), toTarget(alt)),
                        Lat(Join(Inj(v1),
                                 Inj(v2))))))
    case Let(v, init, body) => Bind(varToTarget(v), toTarget(init), toTarget(body))
    case _ => ???
  }

  /////////////////////////////
  // THIRD COMPILATION PHASE //
  /////////////////////////////

  def toScala(exp: TE): String = exp.toString

}
