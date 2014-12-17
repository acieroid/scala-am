/**
  * Implementation of a CESK machine for ANF following the AAM approach
  */
object Primitives {
  type Primitive = List[AbstractValue] => AbstractValue

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

  def unBoolOp(f: Boolean => Boolean): Primitive = {
    case AbstractSimpleValue(ValueBoolean(x)) :: Nil =>
      AbstractSimpleValue(ValueBoolean(f(x)))
    case _ =>
      throw new Exception("Unexpected number of argument for unary operation")
  }

  val opPlus = binNumOp((x, y) => x+y)
  val opMinus = binNumOp((x, y) => x-y)
  val opTimes = binNumOp((x, y) => x*y)
  val opNumEqual = binCmp((x, y) => x == y)
  val opNumLt = binCmp((x, y) => x < y)
  val opNumLe = binCmp((x, y) => x <= y)
  val opNumGt = binCmp((x, y) => x > y)
  val opNumGe = binCmp((x, y) => x >= y)
  val opNot = unBoolOp(x => !x)

  val all: List[(String, Primitive)] = List(
    ("+" -> opPlus),
    ("-" -> opMinus),
    ("*" -> opTimes),
    ("=" -> opNumEqual),
    ("<" -> opNumLt),
    ("<=" -> opNumLe),
    (">" -> opNumGt),
    (">=" -> opNumGe),
    ("not" -> opNot)
  )

  val forEnv: List[(String, Address)] =
    all.map({ case (name, _) => (name, PrimitiveAddress(name)) })
  val forStore: List[(Address, AbstractValue)] =
    all.map({ case (name, f) => (PrimitiveAddress(name), AbstractPrimitive(name, f)) })
}

sealed abstract class Address
case class VariableAddress(name: String, id: Integer) extends Address
case class PrimitiveAddress(name: String) extends Address
case class KontAddress(exp: ANFExp, id: Integer) extends Address

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
case class KontLet(v: String, body: ANFExp, env: Env, next: KontAddress) extends Kont {
  override def toString(): String = s"KontLet(${v.toString})"
}
case class KontLetrec(v: String, a: Address, body: ANFExp, env: Env, next: KontAddress) extends Kont {
  override def toString(): String = s"KontLetrec(${v.toString})"
}
case class KontHalt() extends Kont {
  override def toString(): String = s"KontHalt()"
}

sealed abstract class AbstractValue {
  def isTrue: Boolean
  def isFalse: Boolean = !isTrue
}
case class AbstractSimpleValue(value: Value) extends AbstractValue {
  def isTrue = value match {
    case ValueBoolean(false) => false
    case _ => true
  }
}
case class AbstractClosure(λ: ANFLambda, ρ: Env) extends AbstractValue {
  def isTrue = true
  override def toString(): String = λ.toString
}
case class AbstractPrimitive(name: String, f: List[AbstractValue] => AbstractValue) extends AbstractValue {
  def isTrue = true
}
case class AbstractPair(car: AbstractValue, cdr: AbstractValue) extends AbstractValue {
  def isTrue = true
}
case class AbstractKont(κ: Kont) extends AbstractValue {
  def isTrue = false
  override def isFalse = false
}

sealed abstract class Control
case class ControlEval(exp: ANFExp, env: Env) extends Control {
  override def toString(): String = s"ev(${exp.toString})"
}
case class ControlKont(v: AbstractValue) extends Control {
  override def toString(): String = s"ko(${v.toString})"
}

/** TODO: use a more generic lattice */
case class Store(content: Map[Address, Set[AbstractValue]]) {
  def this() = this(Map[Address, Set[AbstractValue]]())
  def lookup(addr: Address): Set[AbstractValue] = content.getOrElse(addr, Set[AbstractValue]())
  def apply(addr: Address): Set[AbstractValue] = lookup(addr)
  def extend(addr: Address, v: AbstractValue): Store = Store(content + (addr -> (lookup(addr) + v)))
  def defineBottom(addr: Address): Store = Store(content + (addr -> Set()))
  def ⊔(v: (Address, AbstractValue)): Store = extend(v._1, v._2)
  def ++(l: List[(Address, AbstractValue)]): Store = l.foldLeft(this)((σ, v) => σ ⊔ v)
}
object Store {
  val haltAddress = KontAddress(ANFQuoted(SExpIdentifier("halt")), 0)
  val initial = new Store() ++ List(haltAddress -> AbstractKont(KontHalt())) ++ Primitives.forStore
}

case class State(control: Control, σ: Store, a: KontAddress) {
  /** Inject an expression into a state */
  def this(exp: ANFExp) = this(ControlEval(exp, Env.initial), Store.initial, Store.haltAddress)

  override def toString(): String = control.toString

  def step: Set[State] = control match {
    case ControlEval(e, ρ) => stepEval(e, ρ, σ, a)
    case ControlKont(v) => σ(a).foldLeft(Set[State]())((s, kont) => kont match {
      case AbstractKont(κ) => s ++ stepKont(v, σ, κ)
      case _ => s
    })
  }

  /** Check whether this state is in a final configuration */
  def halted: Boolean = control match {
    case ControlEval(_, _) => false
    case ControlKont(v) => this.a == Store.haltAddress
  }

  /** Atomic evaluator that returns the set of possible value of an atomic
      expression, without modifying the env or store */
  def atomicEval(e: ANFAtomicExp, ρ: Env, σ: Store): Set[AbstractValue] = e match {
    case λ: ANFLambda => Set(AbstractClosure(λ, ρ))
    case ANFIdentifier(name) => ρ(name) match {
      case Some(a) => σ(a)
      case None => throw new Exception(s"Unbound variable: $name")
    }
    case ANFValue(value) => Set(AbstractSimpleValue(value))
  }

  /** Atomic evaluator for a list of values. Return every possible combination
      of the values */
  def atomicEvalList(l: List[ANFAtomicExp], ρ: Env, σ: Store): Set[List[AbstractValue]] = l match {
    case Nil => Set(List())
    case e :: rest => {
      val restv = atomicEvalList(rest, ρ, σ)
      atomicEval(e, ρ, σ).foldLeft(Set[List[AbstractValue]]())((s, v) =>
        s ++ restv.map(vs => v :: vs))
    }
  }

  /** Bind arguments into the environment and store */
  def bindArgs(l: List[(String, AbstractValue)], ρ: Env, σ: Store): (Env, Store) =
    l.foldLeft((ρ, σ))({ case ((ρ, σ), (name, value)) => {
      val a = allocVariable(name, σ)
      (ρ + (name -> a), σ ⊔ (a -> value))
    }})

  def stepEval(e: ANFExp, ρ: Env, σ: Store, κ: KontAddress): Set[State] = e match {
    case ae: ANFAtomicExp => reachedValue(atomicEval(ae, ρ, σ), σ, κ)
    case ANFFuncall(f, args) =>
      atomicEvalList(f :: args, ρ, σ).foldLeft(Set[State]())((s: Set[State], l: List[AbstractValue]) => {
        l match {
        case AbstractClosure(ANFLambda(args, body), ρ) :: argsv => if (args.length == argsv.length) {
          bindArgs(args.zip(argsv), ρ, σ) match {
              case (ρ, σ) => s + State(ControlEval(body, ρ), σ, κ)
          }
        } else {
            throw new Exception(s"Arity error (${args.length} arguments expected, got ${argsv.length}")
        }
        case AbstractPrimitive(name, f) :: argsv => s ++ reachedValue(Set(f(argsv)), σ, κ)
        case _ => throw new Exception(s"Incorrect application $l")
      }})
    case ANFIf(cond, cons, alt) =>
      atomicEval(cond, ρ, σ).foldLeft(Set[State]())((s: Set[State], v: AbstractValue) => {
        val t = State(ControlEval(cons, ρ), σ, κ)
        val f = State(ControlEval(alt, ρ), σ, κ)
        if (v.isTrue && v.isFalse) {
          s + t + f
        } else if (v.isTrue) {
          s + t
        } else if (v.isFalse) {
          s + f
        } else {
          s
        }
      })
    case ANFLet(variable, exp, body) =>
      push(exp, KontLet(variable, body, ρ, κ), ρ, σ)
    case ANFLetrec(variable, exp, body) => {
      val a = allocVariable(variable, σ)
      push(exp, KontLetrec(variable, a, body, ρ + (variable -> a), κ), ρ + (variable -> a), σ defineBottom a)
    }
    case ANFSet(variable, value) => ρ(variable) match {
      case Some(addr) => atomicEval(value, ρ, σ).foldLeft(Set[State]())((s: Set[State], v: AbstractValue) => {
        s ++ reachedValue(Set(v), σ ⊔ (addr -> v), κ)
      })
      case None => throw new Exception(s"Undbound variable: $variable")
    }
    case ANFQuoted(sexp) => throw new Exception("TODO: Quoted values not yet handled")
  }

  def stepKont(v: AbstractValue, σ: Store, κ: Kont): Set[State] = κ match {
    case KontHalt() => Set(this)
    case KontLet(variable, body, ρ, next) => {
      val addr = allocVariable(variable, σ)
      Set(State(ControlEval(body, ρ + (variable -> addr)), σ ⊔ (addr -> v), next))
    }
    case KontLetrec(variable, addr, body, ρ, next) =>
      Set(State(ControlEval(body, ρ), σ ⊔ (addr -> v), next))
  }

  def push(e: ANFExp, κ: Kont,  ρ: Env, σ: Store): Set[State] = {
    val a = allocKont(e, κ, σ)
    Set(State(ControlEval(e, ρ), σ ⊔ (a -> AbstractKont(κ)), a))
  }

  def allocKont(e: ANFExp, κ: Kont, σ: Store): KontAddress = KontAddress(e, σ.hashCode())
  def allocVariable(variable: String, σ: Store): VariableAddress = VariableAddress(variable, σ.hashCode())

  def reachedValue(vs: Set[AbstractValue], σ: Store, κ: KontAddress): Set[State] =
    vs.foldLeft(Set[State]())((s, v) => s + State(ControlKont(v), σ, κ))
}

/** TODO: parameterize */
case class Graph(ids: Map[State, Int], next: Int, nodes: Set[State], edges: Map[State, Set[State]]) {
  def this() = this(Map[State, Int](), 0, Set[State](), Map[State, Set[State]]())
  def this(state: State) = this(Map[State, Int]() + (state -> 0), 1, Set[State](state), Map[State, Set[State]]())
  def addNode(node: State): Graph =
    if (nodes.contains(node)) { this } else {
      Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }
  def addEdge(node1: State, node2: State): Graph =
      addNode(node1).addNode(node2).addEdgeNoCheck(node1, node2)
  def addEdges(l: Traversable[(State, State)]): Graph =
    l.foldLeft(this)({ case (g, (n1, n2)) => g.addEdge(n1, n2) })
  def addEdgeNoCheck(node1: State, node2: State): Graph =
    if (edges.contains(node1) && edges(node1).contains(node2)) { this } else {
      val existing: Set[State] = edges.getOrElse(node1, Set[State]())
      Graph(ids, next, nodes, edges + (node1 -> (existing ++ Set(node2))))
    }
  def toDot(): String = {
      val sb = new StringBuilder("digraph G {\n")
      nodes.foreach((n) => {
        val color = n.control match {
          case ControlEval(_, _) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
        }
        sb.append("node_" + ids(n) + "[label=\"" + n.toString.replaceAll("\"", "\\\\\"") + "\", fillcolor=\"" + color + "\" style=\"filled\"];\n")
      })
      edges.foreach({ case (n1, ns) => ns.foreach((n2) => sb.append(s"node_${ids(n1)} -> node_${ids(n2)}\n")) })
      sb.append("}")
      return sb.toString
    }
}

object AAM {
  def outputDot(graph: Graph, path: String) = {
    val dot = graph.toDot()
    val f = new java.io.File(path)
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    bw.write(dot)
    bw.close()
  }

  def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Graph): (Set[State], Graph) = todo.headOption match {
    case Some(s) =>
      if (visited.contains(s)) {
        loop(todo.tail, visited, halted, graph)
      } else if (s.halted) {
        loop(todo.tail, visited + s, halted + s, graph)
      } else {
        if (visited.size % 100 == 0) {
          println(visited.size)
        }
        if (true || visited.size < 10000) {
          val succs = s.step
          val newGraph = graph //.addEdges(succs.map(s2 => (s, s2)))
          loop(todo.tail ++ succs, visited + s, halted, newGraph)
        } else {
          (halted, graph)
        }
      }
    case None => (halted, graph)
  }

  def eval(exp: ANFExp) = {
    val state = new State(exp)
    loop(Set(state), Set(), Set(), new Graph(state)) match {
      case (halted, graph: Graph) => {
        outputDot(graph, "foo.dot")
        halted
      }
    }
  }
}
