package scalaam.machine

import scalaam.graph._
import Graph.GraphOps
import scalaam.core._
import scalaam.util.Show

/** A number of things are factored out of the AAM-based analysis classes to
    * enable reusability by similar machine abstractions */
trait AAMUtils[E <: Exp, A <: Address, V, T] {
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
                .foldLeft((Set.empty[KA], Set.empty[LKont]))((localAcc, lkont) =>
                  if (lkont.isEmpty) {
                    (localAcc._1 + lkont.next, localAcc._2)
                  } else {
                    (localAcc._1, localAcc._2 + lkont)
                })
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

/**
  * Implementation of a CESK machine following the AAM approach (Van Horn, David,
  * and Matthew Might. "Abstracting abstract machines." ACM Sigplan
  * Notices. Vol. 45. No. 9. ACM, 2010).
  *
  * A difference with the paper is that we separate the continuation store
  * from the value store. That simplifies the implementation
  * of both stores, and the only change it induces is that we are not able to
  * support first-class continuation as easily (we don't support them at all, but
  * they could be added).
  *
  * Also, in the paper, a CESK state is made of 4 components: Control,
  * Environment, Store, and Kontinuation. Here, we include the environment in the
  * control component, and we distinguish "eval" states from "continuation"
  * states. An eval state has an attached environment, as an expression needs to
  * be evaluated within this environment, whereas a continuation state only
  * contains the value reached.

  * E are used as context for the timestamp
  */
class AAM[E <: Exp, A <: Address, V, T](val sem: Semantics[E, A, V, T, E])(
    implicit val timestamp: Timestamp[T, E],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[E, A, V, T, E] with AAMUtils[E, A, V, T] {

  val Action = sem.Action

  /**
    * A machine state is made of a control component, a value store, a
    * continuation store, and an address representing where the current
    * continuation lives.
    */
  case class State(control: Control, store: Store[A, V], kstore: Store[KA, Set[Kont]], a: KA, t: T)
      extends GraphElement
      with SmartHash {
    override def toString = control.toString

    override def label = toString
    override def color = if (halted) { Colors.Yellow } else {
      control match {
        case _: ControlEval  => Colors.Green
        case _: ControlKont  => Colors.Pink
        case _: ControlError => Colors.Red
      }
    }
    override def metadata =
      GraphMetadataMap(
        Map(
          "halted" -> GraphMetadataBool(halted),
          "type" -> (control match {
            case _: ControlEval  => GraphMetadataString("eval")
            case _: ControlKont  => GraphMetadataString("kont")
            case _: ControlError => GraphMetadataString("error")
          })
        ) ++ (control match {
          case ControlKont(v) => Map("value" -> GraphMetadataValue[V](v))
          case _              => Map()
        }))

    /**
      * Checks if the current state is a final state. It is the case if it
      * reached the end of the computation, or an error
      */
    def halted: Boolean = control match {
      case _: ControlEval  => false
      case _: ControlKont  => a == HaltKontAddr
      case _: ControlError => true
    }

    /**
      * Integrates a set of actions (returned by the semantics, see
      * Semantics.scala), in order to generate a set of states that succeeds this
      * one.
      */
    private def integrate(a: KA, actions: Set[Action.A]): Set[State] =
      actions.flatMap({
        /* When a value is reached, we go to a continuation state */
        case Action.Value(v, store) =>
          Set(State(ControlKont(v), store, kstore, a, Timestamp[T, E].tick(t)))
        /* When a continuation needs to be pushed, push it in the continuation store */
        case Action.Push(frame, e, env, store) => {
          val next = KontAddr(e, t)
          Set(
            State(ControlEval(e, env),
                  store,
                  kstore.extend(next, Set(Kont(frame, a))),
                  next,
                  Timestamp[T, E].tick(t)))
        }
        /* When a value needs to be evaluated, we go to an eval state */
        case Action.Eval(e, env, store) =>
          Set(State(ControlEval(e, env), store, kstore, a, Timestamp[T, E].tick(t)))
        /* When a function is stepped in, we also go to an eval state */
        case Action.StepIn(fexp, _, e, env, store) =>
          Set(State(ControlEval(e, env), store, kstore, a, Timestamp[T, E].tick(t, fexp)))
        /* When an error is reached, we go to an error state */
        case Action.Err(err) =>
          Set(State(ControlError(err), store, kstore, a, Timestamp[T, E].tick(t)))
      })

    /**
      * Computes the set of states that follow the current state
      */
    def step: Set[State] = control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e, env) => integrate(a, sem.stepEval(e, env, store, t))
      /* In a continuation state, call the semantics' continuation method */
      case ControlKont(v) =>
        kstore.lookup(a) match {
          case Some(konts) =>
            konts.flatMap({
              case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, store, t))
            })
          case None => Set()
        }
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => Set()
    }
  }

  object State {
    def inject(exp: E, env: Environment[A], store: Store[A, V]) =
      State(ControlEval(exp, env),
            store,
            Store.empty[KA, Set[Kont]],
            HaltKontAddr,
            Timestamp[T, E].initial(""))

    /* TODO: do this without typeclass, e.g., class State extends WithKey[KA](a) */
    implicit val stateWithKey = new WithKey[State] {
      type K = KA
      def key(st: State) = st.a
    }
  }

  type Transition = NoTransition
  val empty = new NoTransition

  /**
    * Performs the evaluation of an expression @param exp (more generally, a
    * program) under the given semantics @param sem. If @param graph is true, it
    * will compute and generate the graph corresponding to the execution of the
    * program (otherwise it will just visit every reachable state). A @param
    * timeout can also be given.
    */
  def run[G](program: E, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    import scala.language.higherKinds
    /* The fixpoint computation loop. @param todo is the set of states that need to
     * be visited (the worklist). @param visited is the set of states that have
     * already been visited. @param halted is the set of "final" states, where
     * the program has finished its execution (it is only needed so that it can
     * be included in the output, to return the final values computed by the
     * program). @param graph is the current graph that has been computed (if we
     * need to compute it). If we don't need to compute the graph, @param graph
     * is None (see type definition for G above in this file).  Note that the
     * worklist and visited set are "parameterized" and not tied to concrete
     * implementations; but they are basically similar as Set[State].
     */
    @scala.annotation.tailrec
    def loop[WL[_]: WorkList, VS[_]: VisitedSet](todo: WL[State],
                                                 visited: VS[State],
                                                 graph: G): G = {
      if (timeout.reached) {
        /* If we exceeded the maximal time allowed, we stop the evaluation and return what we computed up to now */
        graph
      } else {
        /* Pick an element from the worklist */
        WorkList[WL].pick(todo) match {
          /* We have an element, hence pick returned a pair consisting of the state to visit, and the new worklist */
          case Some((s, newTodo)) =>
            if (VisitedSet[VS].contains(visited, s)) {
              /* If we already visited the state, or if it is subsumed by another already
               * visited state (i.e., we already visited a state that contains
               * more information than this one), we ignore it. The subsumption
               * part reduces the number of visited states. */
              loop(newTodo, visited, graph)
            } else if (s.halted) {
              /* If the state is a final state, add it to the list of final states and
               * continue exploring the graph */
              loop(newTodo, VisitedSet[VS].add(visited, s), graph)
            } else {
              /* Otherwise, compute the successors of this state, update the graph, and push
               * the new successors on the todo list */
              val succs    = s.step /* as.step returns the set of successor states for s */
              val newGraph = graph.addEdges(succs.map(s2 => (s, empty, s2))) /* add the new edges to the graph: from s to every successor */
              /* then, add new successors to the worklist, add s to the visited set, and loop with the new graph */
              loop(WorkList[WL].append(newTodo, succs), VisitedSet[VS].add(visited, s), newGraph)
            }
          /* No element returned by pick, this means the worklist is empty and we have visited every reachable state */
          case None => graph
        }
      }
    }
    val fvs = program.fv
    val initialEnv = Environment.initial[A](sem.initialEnv).restrictTo(fvs)
    val initialStore = Store.initial[A, V](sem.initialStore)
    loop(
      /* Start with the initial state resulting from injecting the program */
      Vector(State.inject(program, initialEnv, initialStore)).toSeq,
      /* Initially we didn't visit any state */
      VisitedSet.MapVisitedSet.empty[State],
      /* The initial graph is given */
      Graph[G, State, Transition].empty
    )
  }
}
