package scalaam.core

trait Frame extends SmartHash

trait Semantics[E <: Exp, Addr <: Address, V, T, C] {
  implicit val timestamp: Timestamp[T, C]
  implicit val lattice: Lattice[V]
  val allocator: Allocator[Addr, T, C]

  object Action {
    trait A
    case class Value(v: V, store: Store[Addr, V])                                        extends A
    case class Push(frame: Frame, e: E, env: Environment[Addr], store: Store[Addr, V]) extends A
    case class Eval(e: E, env: Environment[Addr], store: Store[Addr, V])               extends A
    case class StepIn(fexp: E,
                      clo: (E, Environment[Addr]),
                      e: E,
                      env: Environment[Addr],
                      store: Store[Addr, V])
        extends A
    case class Err(err: Error) extends A

    val None: Set[A] = Set.empty

    implicit def actionToSet(act: A): Set[A] = Set(act)
    implicit def fromMF(mf: MayFail[A, Error]): Set[A] = mf match {
      case MayFailSuccess(a)    => Set(a)
      case MayFailError(errs)   => errs.toSet.map(e => Err(e))
      case MayFailBoth(a, errs) => errs.toSet.map(e => Err(e)) ++ (Set(a))
    }
  }

  def stepEval(e: E, env: Environment[Addr], store: Store[Addr, V], t: T): Set[Action.A]

  def stepKont(v: V, frame: Frame, store: Store[Addr, V], t: T): Set[Action.A]

  def initialBindings: Iterable[(String, Addr, V)] = List()
  def initialEnv: Iterable[(String, Addr)]         = initialBindings.map({ case (name, a, _) => (name, a) })
  def initialStore: Iterable[(Addr, V)]            = initialBindings.map({ case (_, a, v) => (a, v) })
}
