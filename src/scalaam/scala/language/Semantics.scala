package scalaam.language

import scalaam.core._
import scalaam.lattice._

trait Frame

trait Semantics[Exp, V, Addr <: Address, T, C] {
  implicit val timestamp: Timestamp[T, C]
  implicit val lattice: Lattice[V]

  object Action {
    trait A
    case class Value(v: V, store: Store[Addr, V]) extends A
    case class Push(frame: Frame, e: Exp, env: Environment[Addr], store: Store[Addr, V]) extends A
    case class Eval(e: Exp, env: Environment[Addr], store: Store[Addr, V]) extends A
    case class StepIn(fexp: Exp, clo: (Exp, Environment[Addr]), e: Exp, env: Environment[Addr], store: Store[Addr, V]) extends A
    case class Err(err: Error) extends A

    import scala.language.implicitConversions
    implicit def actionToSet(act: A): Set[A] = Set(act)
  }


  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, V], t: T): Set[Action.A]

  def stepKont(v: V, frame: Frame, store: Store[Addr, V], t: T): Set[Action.A]

  def initialBindings: Iterable[(String, Addr, V)] = List()
  def initialEnv: Iterable[(String, Addr)] = initialBindings.map({ case (name, a, _) => (name, a) })
  def initialStore: Iterable[(Addr, V)] = initialBindings.map({ case (_, a, v) => (a, v) })
}
