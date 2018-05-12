import scalaz.Scalaz._
import scalaz._

class ModularAAM[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends AbstractMachine[Exp, Abs, Addr, Time] {
  def name = "ModularAAM"

  type G = Graph[FunctionState, Unit, Unit]
  implicit val graphNode = new GraphNode[FunctionState, Unit] {
    override def labelXml(n: FunctionState) = n.toXml
    override def color(n: FunctionState) = if (n.hasError) { Colors.Red } else if (n.halted) { Colors.Yellow } else { Colors.White }
  }
  object G {
    def apply(): G = Graph.empty[FunctionState, Unit, Unit]
    def apply(s: FunctionState): G = Graph.node[FunctionState, Unit, Unit](s)
  }

  trait KontAddr
  case class NormalKontAddress(exp: Exp, time: Time) extends KontAddr
  case object HaltKontAddress extends KontAddr
  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  type KStoreDelta = Map[KontAddr, Set[Kont[KontAddr]]]
  object KStoreDelta {
    def empty: KStoreDelta = Map().withDefaultValue(Set.empty)
  }
  type StoreDelta = Map[Addr, Abs]
  object StoreDelta {
    def empty: StoreDelta = Map()
  }
  case class GlobalStore(store: DeltaStore[Addr, Abs], kstore: TimestampedKontStore[KontAddr]) {
    def push(a: KontAddr, kont: Kont[KontAddr]): GlobalStore =
      if (kstore.lookup(a).contains(kont)) { this } else {
        this.copy(kstore = kstore.extend(a, kont).asInstanceOf[TimestampedKontStore[KontAddr]])
      }
    def include(s2: GlobalStore): GlobalStore =
      this.copy(
        store = store.addDelta(s2.store.d),
        kstore = kstore.join(s2.kstore).asInstanceOf[TimestampedKontStore[KontAddr]])
  }
  object GlobalStore {
    def initial(storeMappings: Iterable[(Addr, Abs)]): GlobalStore = {
      val store = DeltaStore[Addr, Abs](storeMappings.toMap, Map())
      val kstore = TimestampedKontStore[KontAddr](Map(), 0)
      new GlobalStore(store, kstore)
    }
  }

  object ActionHelpers extends ActionHelpers[Exp, Abs, Addr]
  import ActionHelpers._

  trait Control
  case class ControlEval(e: Exp, env: Environment[Addr]) extends Control {
    override def toString = s"ev($e)"
  }
  case class ControlKont(v: Abs) extends Control {
    override def toString = s"ko($v)"
  }
  case class ControlError(err: SemanticError) extends Control {
    override def toString = s"err($err)"
  }

  implicit val stateWithKey = new WithKey[FunctionState] {
    type K = KontAddr
    def key(st: FunctionState) = st.kont
  }

  type Clo = (String, Exp, Environment[Addr])

  case class FunctionState(control: Control, kont: KontAddr, t: Time) {
    def toXml: List[scala.xml.Node] = control match {
      case ControlEval(e, _) => List(<font color="forestgreen">{e.toString.take(40)}</font>)
      case ControlKont(v) => List(<font color="rosybrown1">{v.toString.take(40)}</font>)
      case ControlError(err) => List(<font color="black">{err.toString}</font>)
    }
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(_) => kont == HaltKontAddress
      case ControlError(_) => true
    }
    def hasError: Boolean = control match {
      case ControlError(_) => true
      case _ => false
    }
    def integrate(act: Act, returned: Map[Clo, Abs], store: GlobalStore):
        (/* successor state */
          Option[FunctionState],
          /* resulting store */
          GlobalStore,
          /* functions called */
          Option[Clo],
          /* return value */
          Option[Abs]
        ) = Profiler.profile("integrate") { act match {
      case ActionReachedValue(v, store2, effs) =>
        (Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t))),
          store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)), Option.empty, Option.empty)
      case ActionPush(frame, e, env, store2, effs) =>
        val next = NormalKontAddress(e, t)
        (Some(this.copy(control = ControlEval(e, env), kont = next, t = Timestamp[Time].tick(t))),
          store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)).push(next, Kont(frame, kont)),
          Option.empty, Option.empty)
      case ActionEval(e, env, store2, effs) =>
        (Some(this.copy(control = ControlEval(e, env), t = Timestamp[Time].tick(t))),
          store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)), Option.empty, Option.empty)
      case ActionStepIn(fexp, clo, e, env, store2, argsv, effs) =>
        val v = returned(("clo", e, env))
        (Some(this.copy(control = ControlKont(v), t = Timestamp[Time].tick(t, fexp))),
          store.copy(store = store.store.copy(d = store.store.d |+| store2.asInstanceOf[DeltaStore[Addr, Abs]].d)), Some(("clo" /* fexp.toString */, e, env)), Option.empty)
      case ActionError(err) =>
        (Some(this.copy(control = ControlError(err))),
          store, Option.empty, Option.empty)
        }
    }
    def step(sem: Semantics[Exp, Abs, Addr, Time], returned: Map[Clo, Abs], store: GlobalStore):
        (/* successor states */
          Set[FunctionState],
          /* called functions */
          Set[Clo],
          /* return value */
          Abs,
          /* resulting store */
          GlobalStore
        ) = Profiler.profile("step") {
      val init: (Set[FunctionState], Set[Clo], Abs, GlobalStore) = (Set.empty, Set.empty, JoinLattice[Abs].bottom, store)
      control match {
        case ControlEval(e, env) =>
          sem.stepEval(e, env, store.store, t).foldLeft(init)((acc, action) =>
          integrate(action, returned, acc._4) match { case (s, store2, called, result) =>
            (acc._1 ++ s.toSet, acc._2 ++ called.toSet, JoinLattice[Abs].join(acc._3, result.getOrElse(JoinLattice[Abs].bottom)), store2)
          })
        case ControlKont(v) if kont != HaltKontAddress =>
          store.kstore.lookup(kont).foldLeft(init)((acc, kont) => kont match {
            case Kont(frame, next) => sem.stepKont(v, frame, acc._4.store, t).foldLeft(acc)((acc, action) =>
              this.copy(kont = next).integrate(action, returned, acc._4) match {
                case (s, store2, called, result) => (acc._1 ++ s.toSet, acc._2 ++ called.toSet, JoinLattice[Abs].join(acc._3, result.getOrElse(JoinLattice[Abs].bottom)), store2)
              })
          })
        case ControlKont(v) if kont == HaltKontAddress =>
          /* Either execution is finished, or the function returns value v */
          (Set.empty, Set.empty, v, store)
        case ControlError(_) =>
          (Set.empty, Set.empty, JoinLattice[Abs].bottom, store)
      }
    }
  }

  case class ModularAAMOutput(time: Double, graphs: Map[Clo, Option[G]], timedOut: Boolean) extends Output {
    def numberOfStates = 0
    def finalValues: Set[Abs] = Set()
    def toFile(path: String)(output: GraphOutput) =
      graphs.foreach({
        case (k, Some(g)) => output.toFile(g, ())(path + "-" + k.hashCode + ".dot")
        case (_, None) => ()
      })
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean, timeout: Timeout): Output = {
    type WL[A] = Set[A]
    type VS[A] = KeyMap[A]
    val startingTime = System.nanoTime
    case class InnerLoopState(
      /* The closure analyzed */
      clo: Clo,
      /* The worklist */
      todo: WL[FunctionState],
      /* Set of seen states */
      visited: VS[FunctionState],
      /* Graph computed */
      graph: Option[G],
      /* Functions called */
      called: Set[Clo],
      /* Return value */
      returned: Abs
    )
    implicit val innerLoopStateWithKey = new WithKey[InnerLoopState] {
      type K = Clo
      def key(st: InnerLoopState) = st.clo
    }

    @scala.annotation.tailrec
    def innerLoop(st: InnerLoopState, returned: Map[Clo, Abs], store: GlobalStore):
        (/* final state */ InnerLoopState, /* resulting store */ GlobalStore) =
      if (st.todo.isEmpty || timeout.reached) {
        //println(s"delta: ${store.store.d}")
        (st, store)
      } else {
        val init = (Set[(FunctionState, Unit, FunctionState)](), store, Set[Clo](), JoinLattice[Abs].bottom)
        //st.clo._3.lookup("x").foreach(a => println(store.store.lookup(a)))
        val (edges, store2, called, result) = st.todo.foldLeft(init)((acc, state) => {
          //println(state.control)
          state.step(sem, returned, acc._2) match {
            case (succs, called, result, store2) =>
              (acc._1 ++ succs.map(state2 => (state, (), state2)), store2, acc._3 ++ called, JoinLattice[Abs].join(acc._4, result))
          }
        })
        val newTodo = edges.map(_._3)
        val newGraph = st.graph.map(_.addEdges(edges))
        if (store2 == store) {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            visited = VisitedSet[VS].append(st.visited, st.todo),
            graph = newGraph,
            called = st.called ++ called,
            returned = JoinLattice[Abs].join(st.returned, result)
          ), returned, store2)
        } else {
          innerLoop(st.copy(
            todo = newTodo.filter(s => !VisitedSet[VS].contains(st.visited, s)),
            visited = VisitedSet[VS].empty,
            graph = newGraph,
            called = st.called ++ called,
            returned = JoinLattice[Abs].join(st.returned, result)
          ), returned, store2)
        }
      }

    case class OuterLoopState(
      /* What's still to visit */
      todo: WL[InnerLoopState],
      /* Which inner loop states correspond to which functions */
      funs: Map[Clo, Set[InnerLoopState]],
      /* global store */
      store: GlobalStore,
      /* called functions */
      called: Map[Clo, Set[Clo]],
      /* returned values */
      returned: Map[Clo, Abs],
      /* Graphs */
      graphs: Map[Clo, Option[G]]
    )

    /* Closure `clo` called the closures in `called`. We want to extract what states should be re-analyzed, and an updated version of the call dependencies, and of the initial states (funs) */
    def fromCalled(caller: Clo, called: Set[Clo], calledDeps: Map[Clo, Set[Clo]], funs: Map[Clo, Set[InnerLoopState]]):
        (Set[InnerLoopState], Map[Clo, Set[Clo]], Map[Clo, Set[InnerLoopState]]) =
      Profiler.profile("fromCalled") {
      called.foldLeft((Set[InnerLoopState](), calledDeps, funs))((acc, clo) => {
        //println(s"Called ${clo._1}")
        val state = FunctionState(ControlEval(clo._2, clo._3), HaltKontAddress, Timestamp[Time].initial("" /* TODO: support context sensitivity here */))
        if (acc._3(clo).exists((inner: InnerLoopState) => inner.todo.contains(state))) {
          (acc._1, acc._2 + (caller -> (acc._2(caller) + clo)), acc._3)
        } else {
          val inner = InnerLoopState(clo, Set(state), VisitedSet[VS].empty, Option(G()), Set.empty, JoinLattice[Abs].bottom)
          (acc._1 + inner, acc._2 + (caller -> (acc._2(caller) + clo)), acc._3 + (clo -> (acc._3(clo) + inner)))
        }
      })
      }

    /* Closure `clo` returned `result`. We extract what has to be re-analyzed and an updated set of returned values */
    def fromReturned(clo: Clo, result: Abs, calledDeps: Map[Clo, Set[Clo]], returned: Map[Clo, Abs], funs: Map[Clo, Set[InnerLoopState]]):
        (Set[Clo], Map[Clo, Abs]) = Profiler.profile("fromReturned") {
      //println(s"Closure ${clo._1} returned: $result")
      (calledDeps.foldLeft(Set[Clo]())((acc, kv) => {
        val (k, v) = kv
        if (v.contains(clo)) {
          acc + k
        } else {
          acc
        }
      }), returned + (clo -> JoinLattice[Abs].join(returned(clo), result)))
    }
    def outerLoop(st: OuterLoopState, visited: Set[Clo], iteration: Int): Output = {
      if (st.todo.isEmpty || timeout.reached) {
        new ModularAAMOutput(timeout.time, st.graphs, timeout.reached)
      } else {
        println(s"=========\nIteration: $iteration (${st.todo.length} closures to analyze)")
        st.todo.headOption match {
          case Some(s) if (visited.exists(s2 => s2 == s.clo)) =>
            outerLoop(st.copy(todo = st.todo.tail), visited, iteration+1)
          case Some(s) =>
            //println(s"Analyzing ${s.clo}")
            val (ist, store2) = Profiler.profile("innerLoop") { innerLoop(s, st.returned, st.store) }
            val (todoCalled, called, funs) = fromCalled(ist.clo, ist.called, st.called, st.funs)
            val (todoReturned, returned) = fromReturned(ist.clo, ist.returned, st.called, st.returned, st.funs)
            val newTodo = (ist.called ++ todoReturned).map(clo => {
              val state = FunctionState(ControlEval(clo._2, clo._3), HaltKontAddress, Timestamp[Time].initial(""))
              InnerLoopState(clo, Set(state), VisitedSet[VS].empty, Option(G()), Set.empty, JoinLattice[Abs].bottom)
            })
            val newOuter = OuterLoopState(newTodo ++ st.todo.tail, st.funs |+| funs, st.store.include(store2), st.called |+| called, st.returned |+| returned, st.graphs + (ist.clo -> ist.graph))
            if (newOuter.store == st.store && newOuter.returned == st.returned) {
              outerLoop(newOuter, visited + ist.clo, iteration+1)
            } else {
              outerLoop(newOuter, Set.empty, iteration+1)
            }
          case None =>
            outerLoop(st, visited, iteration+1)
        }
      }
    }
    /*
    @scala.annotation.tailrec
    def outerLoopFrontier(st: OuterLoopState, iteration: Int): Output = {
      if (st.todo.isEmpty || timeout.reached) {
        new ModularAAMOutput(timeout.time, st.graphs, timeout.reached)
      } else {
        println(s"=====================\nIteration: $iteration (${st.todo.length} closures to analyze)")
        val init = (Set[Clo](), st.funs, st.store.include(st.store), st.called, st.returned, st.graphs)
        val todopar = st.todo.par
        val (todoNoDeps, todoDeps) = todopar.partition { ist => false && st.called(ist.clo).isEmpty }
        val __succ = if (todoNoDeps.isEmpty) { todoDeps } else { todoNoDeps }
        //println(st.store.store)
        val _succ = Profiler.profile("outerLoop - 1") { __succ.map(funState => {
          // println(s"Analyzing ${funState.clo._1}, ${funState.clo._2}")
          // println(s"Analyze : ${funState.clo}")
          val (ist, store2) = Profiler.profile("innerLoop") { innerLoop(funState, st.returned, st.store) }
          val (todoCalled, called, funs) = fromCalled(ist.clo, ist.called, st.called, st.funs)
          val (todoReturned, returned) = fromReturned(ist.clo, ist.returned, st.called, st.returned, st.funs)
          (ist.called ++ todoReturned, funs, store2, called, returned, (ist.clo -> ist.graph))
        })}
        val succ = Profiler.profile("outerLoop - 2") { _succ.foldLeft(init)((acc, v) =>
            (acc._1 ++ v._1, acc._2 |+| v._2, acc._3.include(v._3), acc._4 |+| v._4, acc._5 |+| v._5, acc._6 + v._6)) }

        // val succ = st.todo.foldLeft(init)((acc, funState) => {
        //   val (ist, store2) = innerLoop(funState, acc._5, acc._3)
        //   val (todoCalled, called, funs) = fromCalled(ist.clo, ist.called, acc._4, acc._2)
        //   val (todoReturned, returned) = fromReturned(ist.clo, ist.returned, acc._4, acc._5, acc._2)
        //   (/* acc._1 ++ todoCalled ++ todoReturned */
        //     acc._1 ++ ist.called ++ todoReturned, funs, store2, called, returned, acc._6 + (ist.clo -> ist.graph))
        // })
        /* simpler strategy: re-analyze all functions until no change (old one still computed but not used as it is incorrect) */
        val newTodo = /* (if (todoNoDeps.isEmpty) Set().par else todoDeps) ++ succ._1.par */ succ._1.par.map(clo => {
          val state = FunctionState(ControlEval(clo._2, clo._3), HaltKontAddress, Timestamp[Time].initial("" /* TODO: support context sensitivity here */))
          InnerLoopState(clo, Set(state), VisitedSet[VS].empty, Option(G()), Set.empty, JoinLattice[Abs].bottom)
        })
        val newOuter = OuterLoopState(newTodo.seq /* old strategy: succ._1 */, succ._2, succ._3, succ._4, succ._5, succ._6)
        //println(s"Main unchanged: ${newOuter.store.mainIsUnchanged}, called=?: ${newOuter.called == st.called}, returned=?: ${newOuter.returned == st.returned}")
        //Thread.sleep(1000)
        if (newOuter.called == st.called && newOuter.returned == st.returned && newOuter.store == st.store) {
          outerLoop(newOuter.copy(todo = Set.empty), iteration+1)
        } else {
          outerLoop(newOuter, iteration+1)
        }
      }
    }
     */
    def inject(e: Exp, env: Iterable[(String, Addr)], store: Iterable[(Addr, Abs)]): (Clo, FunctionState, GlobalStore) = {
      val initEnv = Environment.initial[Addr](env)
      (("main", e, initEnv),
       FunctionState(ControlEval(e, initEnv), HaltKontAddress, Timestamp[Time].initial("")),
       GlobalStore.initial(store))
    }
    val (initialClo, initialState, store) = inject(exp, sem.initialEnv, sem.initialStore)
    val initialInner = InnerLoopState(initialClo, Set(initialState), VisitedSet[VS].empty, Option(G()), Set.empty, JoinLattice[Abs].bottom)
    Profiler.profile("outerLoop") { outerLoop(OuterLoopState(Set(initialInner),
      Map[Clo, Set[InnerLoopState]]().withDefaultValue(Set.empty) + ((initialClo -> Set(initialInner))),
      store,
      Map[Clo, Set[Clo]]().withDefaultValue(Set.empty),
      Map[Clo, Abs]().withDefaultValue(JoinLattice[Abs].bottom),
      Map[Clo, Option[G]]().withDefaultValue(Option(G()))), Set.empty, 0) }
  }
}
