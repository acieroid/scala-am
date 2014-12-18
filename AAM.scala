/**
  * Implementation of a CESK machine for ANF following the AAM approach
  */

import AbstractValue._

object Options {
  var debug = false
}
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
object KontHalt extends Kont {
  override def toString(): String = s"KontHalt"
}

sealed abstract class Control {
  def ⊒(x: Control): Boolean
}
case class ControlEval(exp: ANFExp, env: Env) extends Control {
  override def toString(): String = s"ev(${exp.toString})"
  def ⊒(x: Control): Boolean = this == x
}
case class ControlKont(v: AbstractValue) extends Control {
  override def toString(): String = s"ko(${v.toString})"
  def ⊒(x: Control): Boolean = x match {
    case ControlEval(_, _) => false
    case ControlKont(v2) => v ⊒ v2
  }
}

case class Store(content: Map[Address, AbstractValue]) {
  def this() = this(Map[Address, AbstractValue]())
  def lookup(addr: Address): AbstractValue = content.getOrElse(addr, AbstractBottom())
  def apply(addr: Address): AbstractValue = lookup(addr)
  def extend(addr: Address, v: AbstractValue): Store = Store(content + (addr -> (lookup(addr) ⊔ v)))
  def ⊔(v: (Address, AbstractValue)): Store = extend(v._1, v._2)
  def ++(l: List[(Address, AbstractValue)]): Store = l.foldLeft(this)((σ, v) => σ ⊔ v)
  def ⊒(σ: Store): Boolean = {
    val k1 = content.keySet
    val k2 = σ.content.keySet
    /* k1 should contain k2, and for every element in k2, the corresponding
       element in k1 should subsume it */
    val res = k1.intersect(k2) == k2 && k2.foldLeft(true)((acc, k) => acc && this(k) ⊒ σ(k))
    /*
    if (res && Options.debug) {
      println("Store is subsumed")
      println(k2.foreach(k => if (this(k) != σ(k) && this(k) ⊒ σ(k)) { println(s"${this(k)} ⊒ ${σ(k)}") }))
    }
    */
    res
  }
  def diff(σ: Store): Unit = {
    val a = content.keySet
    val b = σ.content.keySet
    val aNotB = a.diff(a.intersect(b))
    val bNotA = b.diff(a.intersect(b))
    if (aNotB.size != 0 || bNotA.size != 0) {
      println(s"Different keys: $aNotB, and $bNotA")
    }
    content.foreach({ case (k, v) => {
      val v2 = σ(k)
      if (v != v2) {
        println(s"$k: $v != $v2")
      }}})
  }
}
object Store {
  val initial = new Store() ++ List(HaltKontAddress -> AbstractKont(KontHalt)) ++ Primitives.forStore
}

case class State(control: Control, σ: Store, a: KontAddress) {
  /** Inject an expression into a state */
  def this(exp: ANFExp) = this(ControlEval(exp, Env.initial), Store.initial, HaltKontAddress)

  override def toString(): String = control.toString

  def ⊒(x: State): Boolean = control ⊒ x.control && σ ⊒ x.σ && a == x.a

  def diff(x: State): Unit = {
    println("=====")
    if (control != x.control) {
      println("Controls are different")
    }
    if (a != x.a) {
      println("Continuation addresses are different")
    }
    println(s"Store difference:")
    σ.diff(x.σ)
    println("====")
  }

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
    /* case ANFValue(ValueInteger(_)) => AbstractInt */
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
    case KontHalt => Set(this)
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

object AAM {
  def outputDot(graph: Graph[State], path: String) = {
    graph.toDotFile(path, _.toString.take(40), _.control match {
          case ControlEval(_, _) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
    })
  }

  /** Explore the state space by calling the step function until it reaches a
      final state. Returns the set of final state as well as the constructed
      graph */
  def loop(todo: Set[State], visited: Set[State], halted: Set[State], graph: Graph[State]): (Set[State], Graph[State]) = todo.headOption match {
    case Some(s) =>
      if (visited.contains(s) || visited.exists(s2 => s2 ⊒ s)) {
        /*
        visited.find(s2 => s2 ⊒ s) match {
          case Some(s2) => {
            Options.debug = true
            println(s"$s is subsumed by $s2: ${s2 ⊒ s}")
            s.diff(s2)
            Unit
          }
          case None => Unit
        }
        */
        loop(todo.tail, visited, halted, graph)
      } else if (s.halted) {
        loop(todo.tail, visited + s, halted + s, graph)
      } else {
        if (visited.size % 100 == 0) {
          println(visited.size)
        }
        if (visited.size < 10000) {
          val succs = s.step
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
    loop(Set(state), Set(), Set(), new Graph[State](state)) match {
      case (halted, graph: Graph[State]) => {
        println(s"${graph.size} states")
        outputDot(graph, "foo.dot")
        halted
      }
    }
  }
}
