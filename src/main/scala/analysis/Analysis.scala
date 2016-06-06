trait Analysis[L, Exp, Abs, Addr, Time] {
  implicit def abs: JoinLattice[Abs]
  implicit def addr: Address[Addr]
  implicit def exp: Expression[Exp]
  implicit def time: Timestamp[Time]
  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: L): L
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: L): L
  def error(err: SemanticError, current: L): L
  def join(x: L, y: L): L
  def init: L
}

abstract class BaseAnalysis[L, Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends Analysis[L, Exp, Abs, Addr, Time] {
  /* wtf scala */
  def abs = implicitly[JoinLattice[Abs]]
  def addr = implicitly[Address[Addr]]
  def exp = implicitly[Expression[Exp]]
  def time = implicitly[Timestamp[Time]]
}

case class ProductAnalysis[L1, L2, Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp](analysis1: Analysis[L1, Exp, Abs, Addr, Time], analysis2: Analysis[L2, Exp, Abs, Addr, Time]) extends BaseAnalysis[(L1, L2), Exp, Abs, Addr, Time] {
  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: (L1, L2)) =
    (analysis1.stepEval(e, env, store, t, current._1),
      analysis2.stepEval(e, env, store, t, current._2))
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: (L1, L2)) =
    (analysis1.stepKont(v, frame, store, t, current._1),
      analysis2.stepKont(v, frame, store, t, current._2))
  def error(err: SemanticError, current: (L1, L2)) =
    (analysis1.error(err, current._1),
      analysis2.error(err, current._2))
  def join(x: (L1, L2), y: (L1, L2)) = (analysis1.join(x._1, y._1), analysis2.join(x._2, y._2))
  def init = (analysis1.init, analysis2.init)
}
