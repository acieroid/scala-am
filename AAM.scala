import AbstractValue._

/**
  * Implementation of a CESK machine for ANF following the AAM approach
  */
class AAM[Abs, Addr, Exp <: Expression](implicit abs: AbstractValue[Abs], i: AbstractInjection[Abs],
                                        addr: Address[Addr], addri: AddressInjection[Addr]) {
  /** The control component represents what needs to be evaluated; it can either be an expression or a continuation */
  sealed abstract class Control {
    def subsumes(that: Control): Boolean
  }
  case class ControlEval(exp: Exp, env: Environment[Addr]) extends Control {
    override def toString() = s"ev(${exp.toString})"
    def subsumes(that: Control) = that match {
      case ControlEval(exp2, env2) => exp.equals(exp2) && env.subsumes(env2)
      case ControlKont(_) => false
    }
  }
  case class ControlKont(v: Abs) extends Control {
    override def toString() = s"ko(${v.toString})"
    def subsumes(that: Control) = that match {
      case ControlEval(_, _) => false
      case ControlKont(v2) => abs.subsumes(v, v2)
    }
  }

  val primitives = new Primitives[Abs, Addr]()
  case class State(control: Control, σ: Store[Addr, Abs], a: Addr) {
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
                              Store.empty[Addr, Abs]().extend(primitives.forStore), addri.halt)
    override def toString() = control.toString
  }
}
