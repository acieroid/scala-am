/**
  * Implementation of a CESK machine for ANF following the AAM approach
  */
object Primitives {
  type Primitive = List[AbstractValue] => AbstractValue

  def unOp(name: String, f: AbstractValue => AbstractValue): (String, Primitive) = (name, {
    case x :: Nil => f(x)
    case _ => throw new Exception(s"Arity error in unary operator $name")
  })

  def binOp(name: String, f: (AbstractValue, AbstractValue) => AbstractValue): (String, Primitive) = (name, {
    case x :: y :: Nil => f(x, y)
    case _ => throw new Exception(s"Arity error in binary operator $name")
  })

  val all: List[(String, Primitive)] = List(
    binOp("+", (x, y) => x + y),
    binOp("-", (x, y) => x - y),
    binOp("*", (x, y) => x * y),
    binOp("=", (x, y) => x absEq y),
    binOp("<", (x, y) => x < y),
    binOp("<=", (x, y) => x <= y),
    binOp(">", (x, y) => x > y),
    binOp(">=", (x, y) => x >= y),
    unOp("not", x => !x)
  )

  val forEnv: List[(String, Address)] =
    all.map({ case (name, _) => (name, PrimitiveAddress(name)) })
  val forStore: List[(Address, AbstractValue)] =
    all.map({ case (name, f) => (PrimitiveAddress(name), AbstractPrimitive(name, f)) })
}

sealed abstract class Address
case class VariableAddress(name: String, id: Integer) extends Address
case class PrimitiveAddress(name: String) extends Address
abstract class KontAddress extends Address
case class NormalKontAddress(exp: ANFExp, id: Integer) extends KontAddress
object HaltKontAddress extends KontAddress

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
  /** Fold a function over the values contained in this abstract value. This
      should be redefined only for container-like abstract values. */
  def foldValues[A](f: AbstractValue => Set[A]): Set[A] = f(this)
  def ⊔(x: AbstractValue): AbstractValue = if (this.equals(x)) { this } else { x match {
    case v: AbstractValueSet => v ⊔ x /* TODO: is there a better solution than hardcoding this here? */
    case _ => AbstractValueSet(Set(this, x))
  }}
  def ⊓(x: AbstractValue): AbstractValue = if (this.equals(x)) { this } else { AbstractBottom() }

  /* Primitive operations */
  def +(x: AbstractValue): AbstractValue = AbstractBottom()
  def -(x: AbstractValue): AbstractValue = AbstractBottom()
  def *(x: AbstractValue): AbstractValue = AbstractBottom()
  def <(x: AbstractValue): AbstractValue = AbstractBottom()
  def <=(x: AbstractValue): AbstractValue = AbstractBottom()
  def >(x: AbstractValue): AbstractValue = AbstractBottom()
  def >=(x: AbstractValue): AbstractValue = AbstractBottom()
  def absEq(x: AbstractValue): AbstractValue = AbstractBottom()
  def unary_!(): AbstractValue = AbstractBottom()
}
object AbstractBottom {
  def apply() = AbstractValueSet(Set())
}
case class AbstractClosure(λ: ANFLambda, ρ: Env) extends AbstractValue {
  override def toString: String = s"#<clo>"
  def isTrue = true
}
case class AbstractPrimitive(name: String, f: List[AbstractValue] => AbstractValue) extends AbstractValue {
  override def toString: String = s"#<prim $name>"
  def isTrue = true
}
case class AbstractKont(κ: Kont) extends AbstractValue {
  override def toString: String = s"#<kont $κ>"
  def isTrue = false
  override def isFalse = false
}
case class AbstractSimpleValue(value: Value) extends AbstractValue {
  override def toString: String = value.toString
  def isTrue = value match {
    case ValueBoolean(false) => false
    case _ => true
  }

  /*
  override def ⊔(x: AbstractValue): AbstractValue = (value, x) match {
    case (ValueInteger(a), AbstractSimpleValue(ValueInteger(b))) if a != b => AbstractInt
    case _ => super.⊔(x)
  }
  */

  def numOp(f: (Integer, Integer) => Integer): (Value, AbstractValue) => AbstractValue = {
    case (ValueInteger(a), AbstractSimpleValue(ValueInteger(b))) => AbstractSimpleValue(ValueInteger(f(a,b)))
    case (x, AbstractValueSet(s)) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ numOp(f)(x,y))
    case _ => AbstractBottom()
  }

  def cmpOp(f: (Integer, Integer) => Boolean): (Value, AbstractValue) => AbstractValue = {
    case (ValueInteger(a), AbstractSimpleValue(ValueInteger(b))) => AbstractSimpleValue(ValueBoolean(f(a,b)))
    case (x, AbstractValueSet(s)) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ cmpOp(f)(x,y))
    case _ => AbstractBottom()
  }

  override def +(x: AbstractValue) = numOp((a, b) => a + b)(value, x)
  override def -(x: AbstractValue) = numOp((a, b) => a - b)(value, x)
  override def *(x: AbstractValue) = numOp((a, b) => a * b)(value, x)
  override def <(x: AbstractValue) = cmpOp((a, b) => a < b)(value, x)
  override def <=(x: AbstractValue) = cmpOp((a, b) => a <= b)(value, x)
  override def >(x: AbstractValue) = cmpOp((a, b) => a > b)(value, x)
  override def >=(x: AbstractValue) = cmpOp((a, b) => a >= b)(value, x)
  override def absEq(x: AbstractValue) = cmpOp((a, b) => a == b)(value, x)
  override def unary_!() = value match {
    case ValueBoolean(false) => AbstractSimpleValue(ValueBoolean(true))
    case _ => AbstractSimpleValue(ValueBoolean(false))
  }
}
object AbstractInt extends AbstractValue {
  override def toString: String = "Int"
  def isTrue = true
  override def ⊔(x: AbstractValue): AbstractValue = x match {
    case AbstractSimpleValue(ValueInteger(_)) => AbstractInt
    case _ => super.⊔(x)
  }

  def numOp(x: AbstractValue): AbstractValue = x match {
    case AbstractInt => AbstractInt
    case AbstractSimpleValue(ValueInteger(_)) => AbstractInt
    case AbstractValueSet(s) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ numOp(y))
    case _ => AbstractBottom()
  }

  def cmpOp(x: AbstractValue): AbstractValue = x match {
    case AbstractInt => AbstractBool()
    case AbstractSimpleValue(ValueInteger(_)) => AbstractBool()
    case AbstractValueSet(s) => s.foldLeft(AbstractValueSet())((acc, y) => acc ⊔ cmpOp(y))
    case _ => AbstractBottom()
  }

  override def +(x: AbstractValue) = numOp(x)
  override def -(x: AbstractValue) = numOp(x)
  override def *(x: AbstractValue) = numOp(x)
  override def <(x: AbstractValue) = cmpOp(x)
  override def <=(x: AbstractValue) = cmpOp(x)
  override def >(x: AbstractValue) = cmpOp(x)
  override def >=(x: AbstractValue) = cmpOp(x)
  override def absEq(x: AbstractValue) = cmpOp(x)
  override def unary_!() = AbstractSimpleValue(ValueBoolean(false))
}
object AbstractBool {
  def apply(): AbstractValue = AbstractValueSet(Set(AbstractSimpleValue(ValueBoolean(true)),
                                                    AbstractSimpleValue(ValueBoolean(false))))
}
case class AbstractValueSet(values: Set[AbstractValue]) extends AbstractValue {
  override def toString(): String = "{" + values.mkString(", ") + "}"
  def isTrue = values.exists(_.isTrue)
  override def isFalse = values.exists(_.isFalse)
  override def foldValues[A](f: AbstractValue => Set[A]): Set[A] =
    values.foldLeft(Set[A]())((s: Set[A], v: AbstractValue) => s ++ v.foldValues(f))
  /** The join result will remain a set of values */
  override def ⊔(x: AbstractValue): AbstractValueSet = x match {
    case AbstractValueSet(set) => AbstractValueSet(values ++ set)
    case _ => AbstractValueSet(values + x)
  }
  override def ⊓(x: AbstractValue): AbstractValue = x match {
    case AbstractValueSet(s) => AbstractValueSet(s.intersect(values))
    case _ => if (values.contains(x)) { x } else { AbstractBottom() }
  }
  def op(f: (AbstractValue, AbstractValue) => AbstractValue): (Set[AbstractValue], AbstractValue) => AbstractValue = {
    case (s, x) => s.foldLeft(AbstractValueSet())((acc, v) => acc ⊔ (f(v, x)))
  }
  override def +(x: AbstractValue) = op((a, b) => a + b)(values, x)
  override def -(x: AbstractValue) = op((a, b) => a - b)(values, x)
  override def *(x: AbstractValue) = op((a, b) => a * b)(values, x)
  override def <(x: AbstractValue) = op((a, b) => a < b)(values, x)
  override def <=(x: AbstractValue) = op((a, b) => a <= b)(values, x)
  override def >(x: AbstractValue) = op((a, b) => a > b)(values, x)
  override def >=(x: AbstractValue) = op((a, b) => a >= b)(values, x)
  override def absEq(x: AbstractValue) = op((a, b) => a absEq b)(values, x)
  override def unary_!() = values.foldLeft(AbstractValueSet())((acc, v) => acc ⊔ !v)
}
object AbstractValueSet {
  def apply(): AbstractValueSet = AbstractValueSet(Set[AbstractValue]())
}

/* TODO: abstract pairs */

sealed abstract class Control
case class ControlEval(exp: ANFExp, env: Env) extends Control {
  override def toString(): String = s"ev(${exp.toString})"
}
case class ControlKont(v: AbstractValue) extends Control {
  override def toString(): String = s"ko(${v.toString})"
}

case class Store(content: Map[Address, AbstractValue]) {
  def this() = this(Map[Address, AbstractValue]())
  def lookup(addr: Address): AbstractValue = content.getOrElse(addr, AbstractBottom())
  def apply(addr: Address): AbstractValue = lookup(addr)
  def extend(addr: Address, v: AbstractValue): Store = Store(content + (addr -> (lookup(addr) ⊔ v)))
  def ⊔(v: (Address, AbstractValue)): Store = extend(v._1, v._2)
  def ++(l: List[(Address, AbstractValue)]): Store = l.foldLeft(this)((σ, v) => σ ⊔ v)
}
object Store {
  val initial = new Store() ++ List(HaltKontAddress -> AbstractKont(KontHalt())) ++ Primitives.forStore
}

case class State(control: Control, σ: Store, a: KontAddress) {
  /** Inject an expression into a state */
  def this(exp: ANFExp) = this(ControlEval(exp, Env.initial), Store.initial, HaltKontAddress)

  override def toString(): String = control.toString

  /** Performs a step */
  def step: Set[State] = control match {
    case ControlEval(e, ρ) => stepEval(e, ρ, σ, a)
    case ControlKont(v) => σ(a).foldValues({
      case AbstractKont(κ) => stepKont(v, σ, κ)
      case _ => Set()
    })
  }

  /** Check whether this state is in a final configuration */
  def halted: Boolean = control match {
    case ControlEval(_, _) => false
    case ControlKont(v) => this.a == HaltKontAddress
  }

  /** Atomic evaluator that returns the value of an atomic expression, without
      modifying the env or store */
  def atomicEval(e: ANFAtomicExp, ρ: Env, σ: Store): AbstractValue = e match {
    case λ: ANFLambda => AbstractClosure(λ, ρ)
    case ANFIdentifier(name) => ρ(name) match {
      case Some(a) => σ(a)
      case None => throw new Exception(s"Unbound variable: $name")
    }
    case ANFValue(ValueInteger(_)) => AbstractInt
    case ANFValue(value) => AbstractSimpleValue(value)
  }

  /** Bind arguments into the environment and store */
  def bindArgs(l: List[(String, AbstractValue)], ρ: Env, σ: Store): (Env, Store) =
    l.foldLeft((ρ, σ))({ case ((ρ, σ), (name, value)) => {
      val a = allocVariable(name, σ)
      (ρ + (name -> a), σ ⊔ (a -> value))
    }})

  /** Performs an evaluation step */
  def stepEval(e: ANFExp, ρ: Env, σ: Store, κ: KontAddress): Set[State] = e match {
          case ae: ANFAtomicExp => reachedValue(atomicEval(ae, ρ, σ), σ, κ)
    case ANFFuncall(f, args) =>
      val argsv = args.map(a => atomicEval(a, ρ, σ))
      val fv = atomicEval(f, ρ, σ)
      fv.foldValues({
        case AbstractClosure(ANFLambda(args, body), ρ) => if (args.length == argsv.length) {
          bindArgs(args.zip(argsv), ρ, σ) match {
            case (ρ, σ) => Set(State(ControlEval(body, ρ), σ, κ))
          }
        } else {
            throw new Exception(s"Arity error (${args.length} arguments expected, got ${argsv.length}")
        }
        case AbstractPrimitive(name, f) => reachedValue(f(argsv), σ, κ)
        case _ => Set()
      })
    case ANFIf(cond, cons, alt) =>
      val v = atomicEval(cond, ρ, σ)
      val t = State(ControlEval(cons, ρ), σ, κ)
      val f = State(ControlEval(alt, ρ), σ, κ)
      if (v.isTrue && v.isFalse) {
        Set(t, f)
      } else if (v.isTrue) {
        Set(t)
      } else if (v.isFalse) {
        Set(f)
      } else {
        Set()
      }
    case ANFLet(variable, exp, body) =>
      push(exp, KontLet(variable, body, ρ, κ), ρ, σ)
    case ANFLetrec(variable, exp, body) => {
      val a = allocVariable(variable, σ)
      push(exp, KontLetrec(variable, a, body, ρ + (variable -> a), κ), ρ + (variable -> a), σ ⊔ (a -> AbstractBottom()))
    }
    case ANFSet(variable, value) => ρ(variable) match {
      case Some(addr) => {
        val v = atomicEval(value, ρ, σ)
        reachedValue(v, σ ⊔ (addr -> v), κ)
      }
      case None => throw new Exception(s"Undbound variable: $variable")
    }
    case ANFQuoted(sexp) => throw new Exception("TODO: Quoted values not yet handled")
  }

  /** Performs a continuation step */
  def stepKont(v: AbstractValue, σ: Store, κ: Kont): Set[State] = κ match {
    case KontHalt() => Set(this)
    case KontLet(variable, body, ρ, next) => {
      val addr = allocVariable(variable, σ)
      Set(State(ControlEval(body, ρ + (variable -> addr)), σ ⊔ (addr -> v), next))
    }
    case KontLetrec(variable, addr, body, ρ, next) =>
      Set(State(ControlEval(body, ρ), σ ⊔ (addr -> v), next))
  }

  /** Push a continuation and evaluate the given expression */
  def push(e: ANFExp, κ: Kont,  ρ: Env, σ: Store): Set[State] = {
    val a = allocKont(e, κ, σ)
    Set(State(ControlEval(e, ρ), σ ⊔ (a -> AbstractKont(κ)), a))
  }

  def reachedValue(v: AbstractValue, σ: Store, κ: KontAddress): Set[State] =
    Set(State(ControlKont(v), σ, κ))


  def allocKont(e: ANFExp, κ: Kont, σ: Store): KontAddress = NormalKontAddress(e, 0)
  def allocVariable(variable: String, σ: Store): VariableAddress = VariableAddress(variable, 0)
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
  def size(): Integer = nodes.size
  def toDot(): String = {
      val sb = new StringBuilder("digraph G {\n")
      nodes.foreach((n) => {
        val color = n.control match {
          case ControlEval(_, _) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
        }
        sb.append("node_" + ids(n) + "[label=\"" + n.toString.take(40).replaceAll("\"", "\\\\\"") + "\", fillcolor=\"" + color + "\" style=\"filled\"];\n")
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
        if (visited.size < 500) {
          val succs = s.step
          println(s"Visiting $s leads to ${succs.size} successors")
          val newGraph = graph.addEdges(succs.map(s2 => (s, s2)))
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
        println(s"${graph.size} states")
        outputDot(graph, "foo.dot")
        halted
      }
    }
  }
}
