import scalaz.Scalaz._

case class ConcurrentAAM[Exp : Expression, Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr])
    extends AbstractMachine[Exp, Abs, Addr] {
  def abs = implicitly[AbstractValue[Abs]]
  def absi = implicitly[AbstractInjection[Abs]]
  def addr = implicitly[Address[Addr]]
  def addri = implicitly[AddressInjection[Addr]]
  def exp = implicitly[Expression[Exp]]

  def name = "ConcurrentAAM"
  val aam = new AAM[Exp, Abs, Addr]
  import aam._

  type KontAddr = aam.KontAddr
  type TID = Int /* TODO: tid */
  var maxtid = 1
  def newtid(): Int = {
    maxtid = maxtid + 1
    maxtid
  }

  /* TODO: switch Set[(TID, Context)] to the ThreadMap, as thread creation/update/removal decisions impact it */
  case class Context(control: Control, kstore: KontStore[KontAddr], a: KontAddr) {
    def integrate1(tid: TID, a: KontAddr, action: Action[Exp, Abs, Addr])(threads: ThreadMap, store: Store[Addr, Abs]):
        (ThreadMap, Store[Addr, Abs]) = action match {
      case ActionReachedValue(v, σ) => (threads.update(tid, Context(ControlKont(v), kstore, a)), store.join(σ))
      case ActionPush(e, frame, ρ, σ) => {
        val next = NormalKontAddress(e, addri.variable("__kont__"))
        (threads.update(tid, Context(ControlEval(e, ρ), kstore.extend(next, Kont(frame, a)), next)), store.join(σ))
      }
      case ActionEval(e, ρ, σ) => (threads.update(tid, Context(ControlEval(e, ρ), kstore, a)), store.join(σ))
      case ActionStepIn(_, e, ρ, σ, _) => (threads.update(tid, Context(ControlEval(e, ρ), kstore, a)), store.join(σ))
      case ActionSpawn(e, ρ, act) => integrate1(tid, a, act)(threads.add(newtid(), Context(ControlEval(e, ρ), new KontStore[KontAddr](), HaltKontAddress)), store)
    }

    def integrate(tid: TID, a: KontAddr, actions: Set[Action[Exp, Abs, Addr]], threads: ThreadMap):
        (ThreadMap, Store[Addr, Abs]) =
      actions.foldLeft((threads, Store.empty[Addr, Abs]()(abs, absi, addr)))((acc, action) => integrate1(tid, a, action)(acc._1, acc._2))

    def step(sem: Semantics[Exp, Abs, Addr], tid: TID, store: Store[Addr, Abs], threads: ThreadMap):
        (ThreadMap, Store[Addr, Abs]) = control match {
      case ControlEval(e, ρ) => integrate(tid, a, sem.stepEval(e, ρ, store), threads)
      case ControlKont(v) if abs.isError(v) => (threads, store)
      case ControlKont(v) => kstore.lookup(a).foldLeft((threads, Store.empty[Addr, Abs]()(abs, absi, addr)))((acc, k) => k match {
        case Kont(frame, next) =>
          val (threads2: ThreadMap, store2: Store[Addr, Abs]) = integrate(tid, next, sem.stepKont(v, store, frame), threads)
          (acc._1.join(threads2), acc._2.join(store2))
      })
      case ControlError(_) => (threads, store)
    }

    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => a == HaltKontAddress || abs.isError(v)
      case ControlError(_) => true
    }
  }

  case class ThreadMap(content: Map[TID, Set[Context]]) {
    def get(tid: TID): Set[Context] = content.getOrElse(tid, Set())
    def tids: Set[TID] = content.keys.toSet
    def update(tid: TID, context: Context) = ThreadMap(content + (tid -> Set(context))) /* TODO: abstract thread counting, join */
    def add(tid: TID, context: Context) = ThreadMap(content + (tid -> (get(tid) + context)))
    def join(that: ThreadMap) = ThreadMap(this.content |+| that.content) /* TODO: does this correctly joins sets? */
    def forall(f: ((TID, Set[Context])) => Boolean): Boolean = content.forall(f)
  }

  case class State(threads: ThreadMap, store: Store[Addr, Abs]) {
    def step(sem: Semantics[Exp, Abs, Addr], tid: TID): State = {
      val (threads2, store2) = threads.get(tid).foldLeft((threads, Store.empty[Addr, Abs]()(abs, absi, addr)))((acc, ctx) => {
        val (threads3, store2) = ctx.step(sem, tid, store, acc._1)
        (threads3, acc._2.join(store2))
      })
      State(threads2, store2)
    }
    def stepAll(sem: Semantics[Exp, Abs, Addr]): Set[(TID, State)] =
      threads.tids.foldLeft(Set[(TID, State)]())((acc, tid) => acc + (tid -> step(sem, tid)))

    def halted: Boolean = threads.forall({
      case (_, ctxs) => ctxs.forall(_.halted)
    })
  }

  object State {
    def inject(exp: Exp) = {
      val st = new aam.State(exp)
      State(ThreadMap(Map[TID, Set[Context]](1 -> Set(Context(st.control, st.kstore, st.a)))), st.σ)
    }
  }


  case class ConcurrentAAMOutput(halted: Set[State], count: Int, t: Double, graph: Option[Graph[State]])
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.threads.get(0).flatMap(ctx => ctx.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    }))
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def numberOfStates = count
    def time = t
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toString.take(40),
        (s) => if (halted.contains(s)) { "#FFFFDD" } else { "#FFFFFF" })
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], startingTime: Long, graph: Option[Graph[State]],
    sem: Semantics[Exp, Abs, Addr]): ConcurrentAAMOutput =
    todo.headOption match {
      case Some(s) =>
        if (visited.contains(s)) {
          loop(todo.tail, visited, halted, startingTime, graph, sem)
        } else if (s.halted) {
          loop(todo.tail, visited + s, halted + s, startingTime, graph, sem)
        } else {
          val succs = s.stepAll(sem).map(_._2)
          val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, s2))))
          loop(todo.tail ++ succs, visited + s, halted, startingTime, newGraph, sem)
        }
      case None => ConcurrentAAMOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
    }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr], graph: Boolean): Output[Abs] =
    loop(Set(State.inject(exp)), Set(), Set(), System.nanoTime,
      if (graph) { Some (new Graph[State]()) } else { None },
      sem)

}
