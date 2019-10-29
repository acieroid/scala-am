package scalaam.machine

import scalaam.core._
import scalaam.util.Show

/** A number of things are factored out of the AAM-based analysis classes to
  * enable reusability by similar machine abstractions */
trait AAMUtils[E <: Expression, A <: Address, V, T] {

  /** Control component of the machine. */
  trait Control extends SmartHash
  case class ControlEval(exp: E, env: Environment[A]) extends Control {
    override def toString = s"ev(${exp})"
  }
  case class ControlKont(v: V) extends Control {
    override def toString = s"ko(${v})"
  }
  case class ControlError(err: Error) extends Control {
    override def toString = s"err($err)"
  }
  case class ControlCall(f: V, fexp: E, args: List[(V, E)]) extends Control {
    override def toString = s"call(${f})"
  }

  /** Kontinuation addresses */
  trait KA extends Address with SmartHash {
    def printable = true
    def primitive = false
  }
  case class KontAddr(exp: E, time: T) extends KA {
    override def toString = s"Kont(${exp.toString.take(10)})"
  }
  case object HaltKontAddr extends KA {
    override def toString = "Halt"
  }

  case class Kont(f: Frame, next: KA) extends SmartHash
  implicit val kontShow = new Show[Kont] {
    def show(k: Kont) = "kont($f)"
  }
  implicit val kontSetLattice = Lattice.SetLattice[Kont]

  /** Some machine abstractions use a local continuation */
  object LKont {
    def empty(next: KA): LKont = LKont(List.empty, next)
  }
  case class LKont(contents: List[Frame], next: KA) extends Frame {
    override def toString  = "[" + contents.mkString("; ") + "]"
    def isEmpty: Boolean   = contents.isEmpty
    def push(frame: Frame) = LKont(frame :: contents, next)
    def get: Option[(Frame, LKont)] = contents match {
      case head :: tail => Some((head, LKont(tail, next)))
      case Nil          => None
    }
    def findKonts(kstore: Store[KA, Set[LKont]]): Set[LKont] = {
      def helper(todo: Set[KA], visited: Set[KA], acc: Set[LKont]): Set[LKont] =
        todo.headOption match {
          case None               => acc
          case Some(HaltKontAddr) => acc + (LKont.empty(HaltKontAddr))
          case Some(a) =>
            if (visited.contains(a)) {
              helper(todo - a, visited, acc)
            } else {
              val (todo2, acc2) = kstore
                .lookupDefault(a, Set.empty[LKont])
                .foldLeft((Set.empty[KA], Set.empty[LKont]))(
                  (localAcc, lkont) =>
                    if (lkont.isEmpty) {
                      (localAcc._1 + lkont.next, localAcc._2)
                    } else {
                      (localAcc._1, localAcc._2 + lkont)
                    }
                )
              helper(todo - a ++ todo2, visited + a, acc ++ acc2)
            }
        }
      helper(Set(next), Set(), Set())
    }
  }
  implicit val lkontShow = new Show[LKont] {
    def show(lkont: LKont) = s"lkont(${lkont.contents.mkString(",")}, lkont.next)"
  }
  implicit val lkontSetLattice = Lattice.SetLattice[LKont]

}
