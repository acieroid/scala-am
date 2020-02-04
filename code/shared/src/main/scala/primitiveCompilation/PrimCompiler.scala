package scalaam.primitiveCompilation

import scalaam.primitiveCompilation.PrimSource._
import scalaam.primitiveCompilation.PrimTarget._
import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.sexp._

object Test extends App {
  val program = "(define (length l) (let ((n (null? l))) (if n 0 (let ((c (cdr l))) (let ((len (length c))) (+ 1 len))))))"
  println(program)
  val text = SchemeParser.parse(program)
  val source = PrimCompiler.toSource(text)
  println(source)
  val target = PrimCompiler.toTarget(source)
  println(target)
}

object PrimCompiler {

  type SE = PrimSource.Exp
  type TE = PrimTarget.Exp

  type SA = PrimSource.AExp
  type TA = PrimTarget.AExp

  trait CompilerException extends Exception

  case class  IllegalExpressionException(e: Expression) extends Exception // For expressions that should not occur in the language compiled in the current phase.
  case class ShouldNotEncounterException(e: Expression) extends Exception // For expressions that should not be encountered by the compiler in the current phase.

  def compile(exp: SchemeExp): String = toTarget(toSource(exp)).toString

  /////////////////////////////
  // FIRST COMPILATION PHASE //
  /////////////////////////////

  def toSource(exp: SchemeExp): SE = {

    def valueToSource(v: Value): SE = v match {
      case ValueBoolean(bool) => AE(PrimSource.Boo(bool))
      case ValueInteger(int) => AE(PrimSource.Num(int))
    }

    def toSource(exp: Expression, rec: Boolean): SE = exp match {
      case fc@SchemeFuncall(f, args, _) =>
        val argn = args.map(toSource(_, true))
        if (!argn.forall(_.isInstanceOf[PrimSource.AE])) throw IllegalExpressionException(fc)
        val argv: PrimSource.Args = argn.map{case AE(ae) => ae}.toArray
        toSource(f, true) match { // TODO maybe check arity?
          case AE(PrimSource.Var(Id(name))) if PrimitiveOperations.opNams.contains(name) => PrimSource.OpCall(PrimitiveOperations.ops.find(_.name == name).get, argv)
          case prim => PrimSource.PrimCall(prim, argv)
        }
      case SchemeIf(cond, cons, alt, _) => toSource(cond, true) match {
        case AE(ae) => If(ae, toSource(cons, true), toSource(alt, true))
        case _ => throw IllegalExpressionException(cond) // TODO: support?
      }
      // For now, let behaves as a let* TODO?
      // Restrain a body to a single expression... Important: use foldRight!
      case SchemeLet(bnd, body :: Nil, _) => bnd.foldRight(toSource(body, true)){ case ((id, bnd), acc) => Let(PrimSource.Var(Id(id.name)), toSource(bnd, true), acc) }
      case SchemeLetStar(bnd, body :: Nil, _) => bnd.foldRight(toSource(body, true)){ case ((id, bnd), acc) => Let(PrimSource.Var(Id(id.name)), toSource(bnd, true), acc) }
      // Dysfunctional begin now.
      case SchemeBegin(e :: Nil, _) => toSource(e, true)
      case SchemeDefineVariable(id, exp, _) => Let(PrimSource.Var(Id(id.name)), toSource(exp, true), ???) // TODO
      case SchemeDefineFunction(_, _, body :: Nil, _) if !rec => toSource(body, true) // Only allow a top level define (prim = function definition).
      case SchemeVar(id) => AE(PrimSource.Var(Id(id.name)))
      case SchemeValue(v, _) => valueToSource(v)
      case id@Identifier(nam, _) => throw ShouldNotEncounterException(id)
      case e => throw IllegalExpressionException(e)
    }

    toSource(exp, false)
  }

  //////////////////////////////
  // SECOND COMPILATION PHASE //
  //////////////////////////////

  def AExpToTarget(ae: SA): TA = ae match {
    case PrimSource.Boo(b) => PrimTarget.Boo(b)
    case PrimSource.Num(n) => PrimTarget.Num(n)
    case PrimSource.Var(v) => PrimTarget.Var(v)
  }

  def varToTarget(v: PrimSource.Var): PrimTarget.Var = PrimTarget.Var(v.v)

  def toTarget(exp: SE): TE = exp match {
    case AE(ae) => Lat(Inj(AExpToTarget(ae)))
    case If(cond, cons, alt) =>
      val v0 = PrimTarget.Var()
      val v1 = PrimTarget.Var()
      val v2 = PrimTarget.Var()
      Bind(v0, Lat(Inj(AExpToTarget(cond))), // BindC
           Bind(v1, IfTrue(Inj(v0), toTarget(cons)), // BindC
                Bind(v2, IfFalse(Inj(v0), toTarget(alt)), // BindC
                     Lat(Join(Inj(v1),
                              Inj(v2))))))
    case Let(v, init, body) => Bind(varToTarget(v), toTarget(init), toTarget(body))
    case PrimSource.PrimCall(prim, args) => PrimTarget.PrimCall(toTarget(prim), Args(args.map(AExpToTarget)))
    case PrimSource.OpCall(op, args) => PrimTarget.OpCall(op, Args(args.map(AExpToTarget)))
  }

  /////////////////////////////
  // THIRD COMPILATION PHASE //
  /////////////////////////////

  def toScala(exp: TE): String = exp.toString

}
