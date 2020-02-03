package primitiveCompilation

import primitiveCompilation.PrimSource._
import primitiveCompilation.PrimTarget._

object PrimCompiler {

  type SE = PrimSource.Exp
  type TE = PrimTarget.Exp

  type SA = PrimSource.AExp
  type TA = PrimTarget.AExp

  def compileAExp(ae: SA): TA = ae match {
    case PrimSource.Arg(a) => PrimTarget.Arg(a)
    case PrimSource.Boo(b) => PrimTarget.Boo(b)
    case PrimSource.Num(n) => PrimTarget.Num(n)
    case PrimSource.Var(v) => PrimTarget.Var(v)
  }

  def compileVar(v: PrimSource.Var): PrimTarget.Var = PrimTarget.Var(v.v)

  def compile(exp: SE): TE = exp match {
    case AE(ae) => Lat(Inj(compileAExp(ae)))
    case If(cond, cons, alt) =>
      val v0 = CVa()
      val v1 = CVa()
      val v2 = CVa()
      BindC(v0, Lat(Inj(compileAExp(cond))),
            BindC(v1, IfTrue(Inj(v0), compile(cons)),
                  BindC(v2, IfFalse(Inj(v0), compile(alt)),
                        Lat(Join(Inj(v1),
                                 Inj(v2))))))
    case Let(v, init, body) => Bind(compileVar(v), compile(init), compile(body))
    case _ => ???
  }

}
