package primitiveCompilation

import primitiveCompilation.PrimSource._
import primitiveCompilation.PrimTarget._

object PrimCompiler {

  type SE = PrimSource.Exp
  type TE = PrimTarget.Exp

  type SA = PrimSource.AExp
  type TA = PrimTarget.AExp

  def compileAExp(ae: SA): TA = ae match {
    case PrimSource.Arg(a) => Arg(a)
    case PrimSource.Boo(b) => Boo(b)
    case PrimSource.Num(n) => Num(n)
    case PrimSource.Var(v) => Var(v)
  }

  // TODO: Fix lexical scoping...
  def compile(exp: SE): TE = exp match {
    case AE(ae) => Lat(Inj(compileAExp(ae)))
    case If(cond, cons, alt) => BindC(Lat(Inj(compileAExp(cond))),
                                      BindC(IfTrue(Inj(CVa(0)), compile(cons)),
                                            BindC(IfFalse(Inj(CVa(0)), compile(alt)),
                                                  Lat(Join(Inj(CVa(0)),
                                                           Inj(CVa(1)))))))
    case Let(init, body) => Bind(compile(init), compile(body))
    case _ => ???
  }

}
