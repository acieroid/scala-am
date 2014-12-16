import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.io.dot._

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

  val opPlus = binNumOp((x, y) => x+y)
  val opMinus = binNumOp((x, y) => x-y)
  val opTimes = binNumOp((x, y) => x*y)
  val opNumEqual = binCmp((x, y) => x == y)
  val opNumLt = binCmp((x, y) => x < y)
  val opNumLe = binCmp((x, y) => x <= y)
  val opNumGt = binCmp((x, y) => x > y)
  val opNumGe = binCmp((x, y) => x >= y)

  val all: List[(String, Primitive)] = List(
    ("+" -> opPlus),
    ("-" -> opMinus),
    ("*" -> opTimes),
    ("=" -> opNumEqual),
    ("<" -> opNumLt),
    ("<=" -> opNumLe),
    (">" -> opNumGt),
    (">=" -> opNumGe)
  )

  val forEnv: List[(String, Address)] =
    all.map({ case (name, _) => (name, PrimitiveAddress(name)) })
  val forStore: List[(Address, AbstractValue)] =
    all.map({ case (name, f) => (PrimitiveAddress(name), AbstractPrimitive(name, f)) })

}

sealed abstract class Address
case class VariableAddress(name: String) extends Address
case class PrimitiveAddress(name: String) extends Address
case class KontAddress(exp: ANFExp) extends Address

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
  val haltAddress = KontAddress(ANFQuoted(SExpIdentifier("halt")))
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
      val a = allocVariable(name)
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
      val a = allocVariable(variable)
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
      val addr = allocVariable(variable)
      Set(State(ControlEval(body, ρ + (variable -> addr)), σ ⊔ (addr -> v), next))
    }
    case KontLetrec(variable, addr, body, ρ, next) =>
      Set(State(ControlEval(body, ρ), σ ⊔ (addr -> v), next))
  }

  def push(e: ANFExp, κ: Kont,  ρ: Env, σ: Store): Set[State] = {
    val a = allocKont(e, κ)
    Set(State(ControlEval(e, ρ), σ ⊔ (a -> AbstractKont(κ)), a))
  }

  def allocKont(e: ANFExp, κ: Kont): KontAddress = KontAddress(e)
  def allocVariable(variable: String): VariableAddress = VariableAddress(variable)

  def reachedValue(vs: Set[AbstractValue], σ: Store, κ: KontAddress): Set[State] =
    vs.foldLeft(Set[State]())((s, v) => s + State(ControlKont(v), σ, κ))
}

object AAM {
  def outputDot(graph: Graph[State, DiEdge]) = {
    val dot = graph.toDot(root, edgeTransformer)
    val f = new java.io.File("foo.dot")
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    bw.write(dot)
    bw.close()
  }

  val root = DotRootGraph(directed = true, id = None)
  def edgeTransformer(innerEdge: Graph[State, DiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    import scala.language.existentials
    val edge = innerEdge.edge
    Some((root, DotEdgeStmt(edge.from.toString, edge.to.toString, Nil)))
  }

  def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Graph[State, DiEdge]): (Set[State], Graph[State, DiEdge]) = todo.headOption match {
    case Some(s) =>
      if (visited.contains(s)) {
        loop(todo.tail, visited, halted, graph)
      } else if (s.halted) {
        loop(todo.tail, visited + s, halted + s, graph)
      } else {
        if (visited.size < 50) {
          val succs: Set[State] = s.step
          val succEdges: Set[DiEdge[State]] = succs.map(s2 => DiEdge(s, s2))
          val newGraph = graph ++ Graph.from(succs + s, succEdges)
          outputDot(graph)
          loop(todo.tail ++ succs, visited + s, halted, newGraph)
        } else {
          (halted, graph)
        }
      }
    case None => (halted, graph)
  }

  def eval(exp: ANFExp) = {
    val state = new State(exp)
    loop(Set(state), Set(), Set(), Graph(state)) match {
      case (halted, graph) => {
        outputDot(graph)
        halted
      }
    }
  }
}
