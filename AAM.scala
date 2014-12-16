/**
  * Implementation of a CESK machine for ANF following the AAM approach
  */
object Primitives {
  type Primitive = List[AbstractValue] => AbstractValue

  val all: List[(String, Primitive)] = List(
    ("+" -> opPlus),
    ("-" -> opMinus),
    ("=" -> opNumEqual)
  )

  val forEnv: List[(String, Address)] =
    all.map({ case (name, _) => (name, PrimitiveAddress(name)) })
  val forStore: List[(Address, AbstractValue)] =
      all.map({ case (name, f) => (PrimitiveAddress(name), AbstractPrimitive(name, f)) })

  def binNumOp(f: (Integer, Integer) => Integer): Primitive = {
    case AbstractSimpleValue(ValueInteger(x)) :: AbstractSimpleValue(ValueInteger(y)) :: Nil =>
      AbstractSimpleValue(ValueInteger(f(x, y)))
    case _ =>
      throw new Exception("TODO: binary numeric operators implementation not complete yet")
  }

  def binCmp(f: (Integer, Integer) => Boolean): Primitive = {
    case AbstractSimpleValue(ValueInteger(x)) :: AbstractSimpleValue(ValueInteger(y)) :: Nil =>
      AbstractSimpleValue(ValueBoolean(f(x, y)))
    case _ =>
      throw new Exception("TODO: binary comparison implementation not complete yet")
  }

  val opPlus = binNumOp((x, y) => x+y)
  val opMinus = binNumOp((x, y) => x+y)
  val opNumEqual = binCmp((x, y) => x == y)
}

sealed abstract class Address
case class VariableAddress(name: String) extends Address
case class PrimitiveAddress(name: String) extends Address
case class KontAddress(exp: ANFExp) extends Address

case class Env(content: Map[String, Address]) {
  def this() = this(Map[String, Address]())
  def lookup(name: String): Option[Address] = content.get(name)
  def apply(name: String): Option[Address] = lookup(name)
  def extend(name: String, addr: Address): Env = Env(content + (name -> addr))
  def +(v: (String, Address)): Env = extend(v._1, v._2)
  def ++(l: List[(String, Address)]): Env = l.foldLeft(this)((ρ, v) => ρ + v)
}

object Env {
  val initial = {
    new Env() ++ Primitives.forEnv
  }
}

sealed abstract class Kont
case class KontLet(v: String, body: ANFExp, env: Env, addr: KontAddress) extends Kont
case class KontLetrec(v: String, body: ANFExp, env: Env, addr: KontAddress) extends Kont
case class KontHalt extends Kont

sealed abstract class AbstractValue
case class AbstractSimpleValue(value: Value) extends AbstractValue
case class AbstractClosure(λ: ANFLambda, ρ: Env) extends AbstractValue
case class AbstractPrimitive(name: String, f: List[AbstractValue] => AbstractValue) extends AbstractValue
case class AbstractPair(car: AbstractValue, cdr: AbstractValue) extends AbstractValue
case class AbstractKont(κ: Kont) extends AbstractValue

sealed abstract class Control
case class ControlEval(exp: ANFExp, env: Env) extends Control
case class ControlKont(v: AbstractValue) extends Control

/** TODO: use a more generic lattice? */
case class Store(content: Map[Address, Set[AbstractValue]]) {
  def this() = this(Map[Address, Set[AbstractValue]]())
  def lookup(addr: Address): Set[AbstractValue] = content.getOrElse(addr, Set[AbstractValue]())
  def apply(addr: Address): Set[AbstractValue] = lookup(addr)
  def extend(addr: Address, v: AbstractValue): Store = Store(content + (addr -> (lookup(addr) + v)))
  def ⊔(v: (Address, AbstractValue)): Store = extend(v._1, v._2)
  def ++(l: List[(Address, AbstractValue)]): Store = l.foldLeft(this)((σ, v) => σ ⊔ v)
}
object Store {
  val initial = new Store() ++ Primitives.forStore
}

case class State(control: Control, σ: Store, κ: Kont) {
  def this(exp: ANFExp) = this(ControlEval(exp, Env.initial), Store.initial, KontHalt())
  def step: Set[State] = control match {
    case ControlEval(e, ρ) => stepEval(e, ρ, σ, κ)
    case ControlKont(v) => stepKont(v, σ, κ)
  }

  def atomicEval(e: ANFAtomicExp, ρ: Env, σ: Store): Set[AbstractValue] = e match {
    case λ: ANFLambda => Set(AbstractClosure(λ, ρ))
    case ANFIdentifier(name) => ρ(name) match {
      case Some(a) => σ(a)
      case None => throw new Exception(s"Unbound variable: $name")
    }
    case ANFValue(value) => Set(AbstractSimpleValue(value))
  }

  def stepEval(e: ANFExp, ρ: Env, σ: Store, κ: Kont): Set[State] = e match {
    case ae: ANFAtomicExp => reachedValue(atomicEval(ae, ρ, σ), σ, κ)
  }

  def stepKont(v: AbstractValue, σ: Store, κ: Kont): Set[State] = κ match {
    case KontHalt() => Set(this)
  }

  def reachedValue(vs: Set[AbstractValue], σ: Store, κ: Kont): Set[State] =
    vs.foldLeft(Set[State]())((s, v) => s + State(ControlKont(v), σ, κ))
}
