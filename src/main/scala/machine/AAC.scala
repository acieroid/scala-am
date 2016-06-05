import scalaz.Scalaz._

/**
 * Implementation of Johnson's CESIK*Ξ machine with a global continuation store
 * (Johnson, James Ian, and David Van Horn. "Abstracting abstract control."
 * Proceedings of the 10th ACM Symposium on Dynamic languages. ACM, 2014).
 *
 * A difference with the paper is that both the call site (i.e., the expression)
 * and the value of every argument is stored in the context when stepping into a
 * function. The paper also contains some typos in the formalisation, which are
 * fixed here. The state exploration strategy is also frontier based, but
 * completely discards seen states upon modification of the continuation store.
 *
 * TODO: There still is a bug, preventing nqueens to converge
 * TODO: Subsumption is defined for every component of the machine, but not
 * used. Use it or remove the definitions.
 * TODO: Investigating AAC with a global value store might be interesting.
 */
class AAC[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "AAC"

  /**
   * A context is basically a continuation's address, is allocated when stepping
   * into a function body, and stores some information to have good precision:
   * the closure itself, the expressions and values of the arguments, and the
   * store at calling time.
   */
  case class Context(clo: (Exp, Environment[Addr]), argsv: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) {
    def subsumes(that: Context) = {
      clo._1.equals(that.clo._1) && clo._2.subsumes(that.clo._2) && store.subsumes(that.store) &&
      argsv.zip(that.argsv).forall({ case ((e1, v1), (e2, v2)) => e1 == e2 && abs.subsumes(v1, v2) })
    }
  }

  /** A Kont is just an address in the continuation store */
  sealed abstract class Kont {
    def subsumes(that: Kont) : Boolean
  }
  /** It can be a context itself */
  case class KontCtx(ctx: Context) extends Kont {
    def subsumes(that: Kont) = that match {
      case KontCtx(ctx2) => ctx.subsumes(ctx2)
      case _ => false
    }
    override def toString = s"KontCtx(${ctx.clo._1}, ${ctx.argsv})"
  }
  /** Or the address of the empty continuation */
  object KontEmpty extends Kont {
    def subsumes(that: Kont) = that.equals(this)
    override def toString = "KontEmpty"
  }

  /** Actual frames are stored in local continuations */
  case class LocalKont(frames: List[Frame]) {
    def this() = this(List())
    def subsumes(that: LocalKont) = frames.zip(that.frames).forall({ case (f1, f2) => f1.subsumes(f2) })
    def isEmpty = frames.isEmpty
    def deconstruct = frames match {
      case List() => None
      case h :: t => Some((h, LocalKont(t)))
    }
    def push(frame: Frame): LocalKont = new LocalKont(frame :: frames)
  }

  /**
   * The continuation store maps continuation addresses to a set of
   * continuations, made of a local continuation and an address for the rest of
   * the continuation.
   *
   * TODO: This basically redefines Kontinuation.scala's KontStore, because the
   * codomain of the map is different (Set[Kont[KontAddr]] vs. Set[(LocalKont,
   * Kont)], where Kont is a different class). Unifying both definition would be
   * better.
   */
  case class KontStore(content: Map[Context, Set[(LocalKont, Kont)]]) {
    def this() = this(Map())
    def lookup(ctx: Context): Set[(LocalKont, Kont)] = content.getOrElse(ctx, Set())
    def extend(ctx: Context, v: (LocalKont, Kont)): KontStore = {
      /*
      content.get(ctx) match {
        case Some(vals) if !vals.contains(v) => println(s"Joining at $ctx: $v + $vals")
        case _ => ()
      } */
      KontStore(content + (ctx -> (lookup(ctx) + v)))
    }
    def join(that: KontStore): KontStore = KontStore(content |+| that.content)
    /** Useful for debugging purposes, in order to have a visualization of the
      * kontinuation store */
    def toDotFile(file: String): Unit = {
      val graph = content.foldLeft(new Graph[Kont, LocalKont]())({ case (g, (ctx, succs)) =>
        succs.foldLeft(g)({ case (g, (local, k)) =>
          g.addEdge(KontCtx(ctx), local, k)
        })
      })
      graph.toDotFile(file, {
        case KontCtx(ctx) => List(scala.xml.Text(ctx.toString.take(40)))
        case KontEmpty => List(scala.xml.Text("ε"))
      }, x => Colors.White, x => List(scala.xml.Text(x.toString)))
    }
  }

  /**
   * The state of the machine contains the control component, the local store, a
   * local continuation and the address of the rest of the continuation
   */
  case class State(control: Control, store: Store[Addr, Abs], lkont: LocalKont, kont: Kont, t: Time) {
    override def toString = control.toString
    def subsumes(that: State): Boolean = control.subsumes(that.control) && store.subsumes(that.store) && lkont.subsumes(that.lkont) && kont.subsumes(that.kont) && t == that.t

    /* TODO: There are a few functions inspecting the continuation (pop,
     * computeKont, kontCanBeEmpty). They should be factored into a single
     * function with the stack-walking mechanics, and some helper functions
     * telling what to do when encountering each kind of stack */

    /**
     * Administrative pop function, popping a frame from the stack. That
     * requires some exploration in the continuation store in case the local
     * continuation is empty. G is the set of continuations already visited, to
     * avoid looping indefinitely when there is a cycle in the continuation
     * store.
     */
    private def pop(lkont: LocalKont, kont: Kont, kstore: KontStore, G: Set[Kont]): Set[(Frame, LocalKont, Kont)] = lkont.deconstruct match {
      case None => kont match {
        case KontEmpty =>
          /* No local continuation and no continuation, the stack is empty and there is no top frame */
          Set()
        case KontCtx(ctx) => {
          /* G2 contains the continuation addresses to which no local continuation are associated */
          val G2: Set[Kont] = kstore.lookup(ctx).flatMap({
            case (lkont, kont) => lkont.deconstruct match {
              case None => Set(kont)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2) /* This is the new visited set */
          /* For every continuation that has a local continuation associated to it, return
           * the top frame of this local continuation. To this, add the popped
           * frame for every continuation to which no local continuation are
           * associated. */
          kstore.lookup(ctx).flatMap({
            case (lkont, kont) => lkont.deconstruct match {
              case None => Set[(Frame, LocalKont, Kont)]()
              case Some((top, rest)) => Set((top, rest, kont))
            }
          }).union(G2.flatMap((kont) => pop(new LocalKont(), kont, kstore, GuG2)))
        }
      }
      case Some((top, rest)) =>
        /* Local continuation is not empty, just return the top frame */
        Set((top, rest, kont))
    }

    private def pop(lkont: LocalKont, kont: Kont, kstore: KontStore): Set[(Frame, LocalKont, Kont)] =
      pop(lkont, kont, kstore, Set())

    /** Debugging function that computes the full current continuation. It is very similar to pop. */
    private def computeKont(lkont: LocalKont, kont: Kont, kstore: KontStore, G: Set[Kont]): Set[List[Frame]] = lkont.deconstruct match {
      case None => kont match {
        case KontEmpty => Set(List())
        case KontCtx(ctx) => {
          val G2: Set[Kont] = kstore.lookup(ctx).flatMap({
            case (lkont, kont) => lkont.deconstruct match {
              case None => Set(kont)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2)
          kstore.lookup(ctx).flatMap({
            case (lkont, kont) => lkont.deconstruct match {
              case None => Set[List[Frame]]()
              case Some((top, rest)) => computeKont(rest, kont, kstore, GuG2).map(k => top :: k)
            }
          }).union(G2.flatMap((kont) => computeKont(new LocalKont(), kont, kstore, GuG2)))
        }
      }
      case Some((top, rest)) => computeKont(rest, kont, kstore, G).map(k => top :: k)
    }

    private def computeKont(lkont: LocalKont, kont: Kont, kstore: KontStore): Set[List[Frame]] =
      computeKont(lkont, kont, kstore, Set())

    /**
     *  Checks whether the continuaton can be empty. That is, will the evaluation be
     * done once we reach a control state?
     */
    private def kontCanBeEmpty(lkont: LocalKont, kont: Kont, kstore: KontStore, G: Set[Kont]): Boolean = lkont.deconstruct match {
      case None => kont match {
        case KontEmpty => true
        case KontCtx(ctx) => {
          val G2: Set[Kont] = kstore.lookup(ctx).flatMap({
            case (lkont, kont) => lkont.deconstruct match {
              case None => Set(kont)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2)
          kstore.lookup(ctx).map({
            case (lkont, KontEmpty) => lkont.isEmpty
            case (lkont, kont) => lkont.deconstruct match {
              case None => false
              case Some((top, rest)) => false
            }
          }).union(G2.map(kont => kontCanBeEmpty(new LocalKont(), kont, kstore, GuG2))).foldLeft(false)((x, y) => x || y)
        }
      }
      case Some((top, rest)) => false
    }

    private def kontCanBeEmpty(lkont: LocalKont, kont: Kont, kstore: KontStore): Boolean =
      kontCanBeEmpty(lkont, kont, kstore, Set())

    /**
     * Integrate a set of actions to generate successor states, and returns
     * these states as well as the updated continuation store (which is global)
     * and the set of contexts changed in this store.
     */
    private def integrate(lkont: LocalKont, κ: Kont, kstore: KontStore, actions: Set[Action[Exp, Abs, Addr]]): (Set[State], KontStore, Set[Context]) =
      actions.foldLeft((Set[State](), kstore, Set[Context]()))({ (acc, act) =>
        val (states, kstore, contexts) = acc
        act match {
          case ActionReachedValue(v, store, _) => (states + State(ControlKont(v), store, lkont, kont, time.tick(t)), kstore, contexts)
          case ActionPush(e, frame, ρ, store, _) => (states + State(ControlEval(e, ρ), store, lkont.push(frame), kont, time.tick(t)), kstore, contexts)
          case ActionEval(e, ρ, store, _) => (states + State(ControlEval(e, ρ), store, lkont, kont, time.tick(t)), kstore, contexts)
          case ActionStepIn(fexp, clo, e, ρ, store, argsv, _) => {
            val ctx = Context(clo, argsv, store, t)
            (states + State(ControlEval(e, ρ), store, new LocalKont(), new KontCtx(ctx), time.tick(t, fexp)),
             kstore.extend(ctx, (lkont, kont)), contexts + ctx)
          }
          case ActionError(err) => (states + State(ControlError(err), store, lkont, kont, time.tick(t)), kstore, contexts)
        }})

    /**
     * Performs an evaluation step, relying on the given semantics (@param sem)
     */
    def step(kstore: KontStore, sem: Semantics[Exp, Abs, Addr, Time]): (Set[State], KontStore, Set[Context]) = control match {
      case ControlEval(e, ρ) => integrate(lkont, kont, kstore, sem.stepEval(e, ρ, store, t))
      case ControlKont(v) => pop(lkont, kont, kstore).foldLeft((Set[State](), kstore, Set[Context]()))((acc, popped) => {
        val (states, kstore1, contexts) = integrate(popped._2, popped._3, acc._2, sem.stepKont(v, popped._1, store, t))
        (acc._1 ++ states, kstore1, acc._3 ++ contexts)
      })
      case ControlError(_) => (Set(), kstore, Set[Context]())
    }

    /**
     * Checks whether this state, with the given continuation store, is a state
     * where the computation could be finished.
     */
    def halted(kstore: KontStore) = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => (lkont.isEmpty && kontCanBeEmpty(lkont, kont, kstore))
      case ControlError(_) => true
    }
  }
  object State {
    def inject(exp: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]) =
      State(ControlEval(exp, Environment.initial[Addr](env)),
        Store.initial[Addr, Abs](store), new LocalKont(), KontEmpty, time.initial(""))
  }

  /**
   * Output of the machine
   */
  case class AACOutput(halted: Set[State], numberOfStates: Int, time: Double, graph: Option[Graph[State, Unit]], timedOut: Boolean)
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, node => List(scala.xml.Text(node.toString.take(40))),
        (s) => if (halted.contains(s)) { Colors.Yellow } else { s.control match {
          case ControlEval(_, _) => Colors.Green
          case ControlKont(_) => Colors.Pink
          case ControlError(_) => Colors.Red
        }}, _ => List())
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  /** Performs the frontier-based state exploration.
   * TODO: This is still WIP. Some states needs to be reexplored when the stack store changes. Possibilities are the following:
   *   - When the stack store changes, empty the set of visited states (used solution, but doesn't terminate on nqueens)
   *   - Track which states use which contexts, and when contexts are modified, reexplore these states (not implemented) */
  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], startingTime: Long, timeout: Option[Long], graph: Option[Graph[State, Unit]],
    kstore: KontStore, sem: Semantics[Exp, Abs, Addr, Time]): AACOutput = {
    if (todo.isEmpty || timeout.map(System.nanoTime - startingTime > _).getOrElse(false)) {
      /*
      graph.map(g => {
        println(s"There are ${g.nodes.size} states")
        println(s"c: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control) }).size}")
        println(s"store: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (store) }).size}")
        println(s"lkont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (lkont) }).size}")
        println(s"kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (kont) }).size}")

        println(s"c, store: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control, store) }).size}")
        println(s"c, lkont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control, lkont) }).size}")
        println(s"c, kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control, kont) }).size}")
        println(s"store, lkont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (store, lkont) }).size}")
        println(s"store, kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (store, kont) }).size}")
        println(s"lkont, kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (lkont, kont) }).size}")

        println(s"c, store, lkont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control, store, lkont) }).size}")
        println(s"c, store, kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control, store, kont) }).size}")
        println(s"c, lkont, kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control, lkont, kont) }).size}")
        println(s"store, lkont, kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (store, lkont, kont) }).size}")

        println(s"c, store, lkont, kont: ${g.nodes.groupBy({ case State(control, store, lkont, kont) => (control, store, lkont, kont) }).size}")
        // grouped.toList.map({ case (k, v) => println(s"Group $k has ${v.size} items") })
      }) */
      /* No more element to visit, outputs the result */
      AACOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph,
        timeout.map(System.nanoTime - startingTime > _).getOrElse(false))
    } else {
      /* Steps every state in the todo list, and merge all the resulting kstores
       * together */
      val (edges, kstore2) = todo.foldLeft((Set[(State, State)](), kstore))({ (acc, ς) =>
        ς.step(kstore, sem) match {
          case (next, kstore2, contexts) =>
            (acc._1 ++ next.map((ς2) => (ς, ς2)), acc._2.join(kstore2))
        }
      })
      if (kstore.equals(kstore2)) {
        /* Continuation store stayed the same, continue exploring the state space */
        loop(edges.map({ case (_, ς2) => ς2 }).diff(visited),
          visited ++ todo,
          halted ++ todo.filter((ς) => ς.halted(kstore)),
          startingTime, timeout,
          graph.map(_.addEdges(edges.map({ case (n1, n2) => (n1, (), n2) }))),
          kstore2,
          sem)
      } else {
        /* Continuation store changed, discard set of seen states */
        loop(edges.map({ case (_, ς2) => ς2 }),
          Set(),
          halted ++ todo.filter((ς) => ς.halted(kstore)),
          startingTime, timeout,
          graph.map(_.addEdges(edges.map({ case (n1, n2) => (n1, (), n2) }))),
          kstore2,
          sem)
      }
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Option[Long]): Output[Abs] =
    loop(Set(State.inject(exp, sem.initialEnv, sem.initialStore)), Set(), Set(), System.nanoTime, timeout,
      if (graph) { Some(new Graph[State, Unit]()) } else { None },
      new KontStore(), sem)
}
