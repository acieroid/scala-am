import AbstractValue._

/**
 * Implementation of a CESK machine following the AAM approach (Van Horn, David,
 * and Matthew Might. "Abstracting abstract machines." ACM Sigplan
 * Notices. Vol. 45. No. 9. ACM, 2010.).
 *
 * A difference with the paper is that we separate the continuation store
 * (KontStore) from the value store (Store). That simplifies the implementation
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
 */
case class AAM[Exp : Expression, Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr])
    extends AbstractMachine[Exp, Abs, Addr]{
  def abs = implicitly[AbstractValue[Abs]]
  def absi = implicitly[AbstractInjection[Abs]]
  def addr = implicitly[Address[Addr]]
  def addri = implicitly[AddressInjection[Addr]]
  def exp = implicitly[Expression[Exp]]

  def name = "AAM"

  /**
   * The control component of the machine
   */
  trait Control {
    def subsumes(that: Control): Boolean
    def toString(store: Store[Addr, Abs]): String = toString()
  }
  /**
   * It can either be an eval component, where an expression needs to be
   * evaluated in an environment
   */
  case class ControlEval(exp: Exp, env: Environment[Addr]) extends Control {
    override def toString() = s"ev(${exp})"
    def subsumes(that: Control) = that match {
      case ControlEval(exp2, env2) => exp.equals(exp2) && env.subsumes(env2)
      case _ => false
    }
  }
  /**
   * Or it can be a continuation component, where a value has been reached and a
   * continuation should be popped from the stack to continue the evaluation
   */
  case class ControlKont(v: Abs) extends Control {
    override def toString() = s"ko(${v})"
    override def toString(store: Store[Addr, Abs]) = s"ko(${abs.toString(v, store)})"
    def subsumes(that: Control) = that match {
      case ControlKont(v2) => abs.subsumes(v, v2)
      case _ => false
    }
  }
  /**
   * Or an error component, in case an error is reached (e.g., incorrect number
   * of arguments in a function call)
   */
  case class ControlError(reason: String) extends Control {
    override def toString() = s"err($reason)"
    def subsumes(that: Control) = that.equals(this)
  }

  /**
   * The store used for continuations is a KontStore (defined in
   * Kontinuation.scala). It is parameterized by continuation addresses, that
   * are element of the KontAddress typeclass.
   */
  trait KontAddr
  case class NormalKontAddress(exp: Exp, addr: Addr) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  val primitives = new Primitives[Addr, Abs]()

  /**
   * A machine state is made of a control component, a value store, a
   * continuation store, and an address representing where the current
   * continuation lives.
   */
  case class State(control: Control, σ: Store[Addr, Abs], kstore: KontStore[KontAddr], a: KontAddr) {
    /**
     * Builds the state with the initial environment and stores
     */
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
      Store.initial[Addr, Abs](primitives.forStore),
      new KontStore[KontAddr](), HaltKontAddress)
    override def toString() = control.toString(σ)
    /**
     * Checks whether a states subsumes another, i.e., if it is "bigger". This
     * is used to perform subsumption checking when exploring the state space,
     * in order to avoid exploring states for which another state that subsumes
     * them has already been explored.
     */
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && a == that.a && kstore.subsumes(that.kstore)

    /**
     * Integrates a set of actions (returned by the semantics, see
     * Semantics.scala), in order to generate a set of states that succeeds this
     * one.
     */
    private def integrate(a: KontAddr, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
      actions.flatMap({
        /* When a value is reached, we go to a continuation state */
        case ActionReachedValue(v, σ) => Set(State(ControlKont(v), σ, kstore, a))
        /* When a continuation needs to be pushed, push it in the continuation store */
        case ActionPush(e, frame, ρ, σ) => {
          val next = NormalKontAddress(e, addri.variable("__kont__")) // Hack to get infinite number of addresses in concrete mode
          Set(State(ControlEval(e, ρ), σ, kstore.extend(next, Kont(frame, a)), next))
        }
        /* When a value needs to be evaluated, we go to an eval state */
        case ActionEval(e, ρ, σ) => Set(State(ControlEval(e, ρ), σ, kstore, a))
        /* When a function is stepped in, we also go to an eval state */
        case ActionStepIn(_, e, ρ, σ, _) => Set(State(ControlEval(e, ρ), σ, kstore, a))
        /* When an error is reached, we go to an error state */
        case ActionError(err) => Set(State(ControlError(err), σ, kstore, a))
      })

    /**
     * Computes the set of states that follow the current state
     */
    def step(sem: Semantics[Exp, Abs, Addr]): Set[State] = control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e, ρ) => integrate(a, sem.stepEval(e, ρ, σ))
      /* In a continuation state, if the value reached is not an error, call the
       * semantic's continuation method */
      case ControlKont(v) if abs.isError(v) => Set()
      case ControlKont(v) => kstore.lookup(a).flatMap({
        case Kont(frame, next) => integrate(next, sem.stepKont(v, σ, frame))
      })
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => Set()
    }
    /**
     * Checks if the current state is a final state. It is the case if it
     * reached the end of the computation, or an error
     */
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress || abs.isError(v)
      case ControlError(_) => true
    }
  }

  case class AAMOutput(halted: Set[State], graph: Option[Graph[State]])
      extends Output[Abs] {

    /**
     * Checks if a halted state contains a value that subsumes @param v
     */
    def containsFinalValue(v: Abs) = halted.exists((st) => st.control match {
      case ControlKont(v2) => abs.subsumes(v2, v)
      case _ => false
    })

    /**
     * Outputs the graph in a dot file
     */
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toString.take(40),
        (s) => if (halted.contains(s)) { "#FFFFDD" } else { s.control match {
          case ControlEval(_, _) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
          case ControlError(_) => "#FF0000"
        }})
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  /**
   * Explores the state graph generated by State's step function.
   * @param todo is the set of states that needs to be visited
   * @param visited is the set of states already visited, they won't be visited again
   * @param halted is the set of final states reached
   * @param graph is the graph in its current form
   * @return the final states as well as the computed graph
   */
  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Option[Graph[State]], sem: Semantics[Exp, Abs, Addr]): AAMOutput =
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s) || visited.exists(s2 => s2.subsumes(s))) {
          /* Non-determinism arises in the number of explored states because of the
           * subsumption checking, and the fact that sets are not ordered: if a
           * "bigger" state is explored first, it might cut off a large chunk of
           * the exploration space, while if it is explored later, the explored
           * state space might be bigger. Disabling subsumption checking leads
           * to a deterministic amount of states, but enabling it can reduce
           * this number of states, and never increases it. */
          loop(todo.tail, visited, halted, graph, sem)
        } else if (s.halted) {
          loop(todo.tail, visited + s, halted + s, graph, sem)
        } else {
          val succs = s.step(sem)
          val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, s2))))
          loop(todo.tail ++ succs, visited + s, halted, newGraph, sem)
        }
      case None => AAMOutput(halted, graph)
    }

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr], graph: Boolean): Output[Abs] =
    loop(Set(new State(exp)), Set(), Set(),
      if (graph) { Some(new Graph[State]()) } else { None },
      sem)
}
