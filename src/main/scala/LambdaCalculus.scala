/* We show here how to add support for a new language and how to build static
 * analyses for the language. We do it for lambda calculus */

import scala.util.parsing.input.Position

/** A lambda calculus expression is represented by a LamExp. It needs to have a
  * position to identify its location in the input file. */
trait LamExp {
  val pos: Position
}
/** An abstraction: lambda x. e */
case class Lam(x: String, e: LamExp, pos: Position) extends LamExp {
  override def toString = s"(lambda ($x) $e)"
}
/** An application: (e1 e2) */
case class App(e1: LamExp, e2: LamExp, pos: Position) extends LamExp {
  override def toString = s"($e1 $e2)"
}
/** A variable reference: x */
case class Var(x: String, pos: Position) extends LamExp {
  override def toString = s"$x"
}

/** Our value domain should form a lattice, but we need support for a bit more than just join operations */
trait LamLattice[L] extends JoinLattice[L] {
  /** We can inject a closure inside the value domain */
  def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L
  /** We can get the closures out of an element of the value domain */
  def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])]
}

/** Here's an implementation of this lattice */
object LamLatticeImpl {
  sealed trait Value
  /** The only value we can have is a closure */
  case class Closure[Exp : Expression, Addr : Address](exp: Exp, env: Environment[Addr]) extends Value {
    override def toString = s"#<clo $exp>"
  }
  /** We define a lattice that is a powerset of closures */
  case class L(elements: Set[Value]) {
    override def toString = elements.mkString(", ")
  }
  implicit val isLamLattice: LamLattice[L] = new LamLattice[L] {
    /** The bottom element of our lattice is the empty set */
    def bottom: L = L(Set[Value]())
    /** Joining two elements is just concatenating their values */
    def join(x: L, y: L): L = L(x.elements ++ y.elements)
    /** One element x subsumes y if x contains at least all the elements of y */
    def subsumes(x: L, y: L): Boolean = y.elements.subsetOf(x.elements)
    /** We need a name for our lattice */
    def name = "LamLattice"
    /** We don't need abstract counting */
    def counting = false
    /** We don't model errors, so x cannot be an error */
    def isError(x: L) = false
    /** We don't have primitive values (the only values we have are closures) */
    def isPrimitiveValue(x: L) = false

    /** To inject a closure into our lattice, we just wrap it in a L */
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = L(Set[Value](Closure[Exp, Addr](x._1, x._2)))
    /** And we can extract closures from an abstract value */
    def getClosures[Exp : Expression, Addr : Address](x: L) = {
      def getClo(x: Value): (Exp, Environment[Addr]) = x match {
        case Closure(exp : Exp @unchecked, env : Environment[Addr] @unchecked) => (exp, env)
      }
      x.elements.map(x => getClo(x))
    }
  }
}

/** This defines the semantics of call-by-value lambda expressions */
class LamSemantics[Abs : LamLattice, Addr : Address, Time : Timestamp]
    extends BaseSemantics[LamExp, Abs, Addr, Time] {
  /** We inherit the value abs that is bound to a JoinLattice[Abs], but we need
    * access to our inject and getClosures, so we need a LamLattice[Abs] as
    * well. */
  def labs = implicitly[LamLattice[Abs]]
  /** Shortcut for the environment */
  type Env = Environment[Addr]
  /** Shorcuct for the store */
  type Sto = Store[Addr, Abs]
  /** We need some frames */
  trait ANFFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
  }
  /** One frame to remember the operand when we evaluate the operator */
  case class FrameArg(e: LamExp, env: Env) extends ANFFrame
  /** And one frame to remember the operator value when we evaluate the operand */
  case class FrameFun(v: Abs) extends ANFFrame

  /** The stepEval function defines how to perform an evaluation step on an
    * expression */
  def stepEval(e: LamExp, env: Env, store: Sto, t: Time) = e match {
    /* A lambda evaluate to a closure by pairing it with the current environment,
     * and injecting this in the abstract domain */
    case Lam(_, _, _) => Set(ActionReachedValue(labs.inject((e, env)), store))
    /* To evaluate an application, we first have to evaluate e1, and we push a
     * continuation to remember to evaluate e2 in the environment env */
    case App(e1, e2, _) => Set(ActionPush(e1, FrameArg(e2, env), env, store))
    /* To evaluate a variable, just look it up in the store */
    case Var(x, _) => env.lookup(x) match {
      case Some(a) => Set(ActionReachedValue(store.lookup(a), store))
      case None => Set(ActionError(s"unbound variable $x"))
    }
  }

  /** The stepKont function defines how to behave when we reached a value v and we
    * have frame as the top continuation on the stack */
  def stepKont(v: Abs, frame: Frame, store: Sto, t: Time) = frame match {
    /* We have evaluated the operator v but still need to evaluate the operator e */
    case FrameArg(e, env) => Set(ActionPush(e, FrameFun(v), env, store))
    /* We have evaluated both the operator (fun) and the operand (v). We go through
     * the possible closures bound to the operator and for each of them, we
     * have to evaluate their body by extending their environment with their
     * argument */
    case FrameFun(fun) => labs.getClosures[LamExp, Addr](fun).map({
      case (Lam(x, e, _), env) => {
        val a = addr.variable(x, v, t)
        ActionEval(e, env.extend(x, a), store.extend(a, v))
      }
    })
  }

  /** The parse function just parses an expression from a string. We rely on an
    * already-defined s-expression parser, and compile an s-expression in a
    * lambda expression. There is some verbosity due to the fact that every
    * s-expression is associated to a position information. */
  def parse(program: String): LamExp = {
    def compile(exp: SExp): LamExp = exp match {
      /* case: (lambda (x) e) */
      case SExpPair(SExpIdentifier("lambda", _), SExpPair(SExpPair(SExpIdentifier(arg, _), SExpValue(ValueNil, _), _), SExpPair(body, SExpValue(ValueNil, _), _), _), _) =>
        Lam(arg, compile(body), exp.pos)
      /* case: x */
      case SExpIdentifier(x, _) =>
        Var(x, exp.pos)
      /* case: (operator operand) */
      case SExpPair(operator, SExpPair(operand, SExpValue(ValueNil, _), _), _) =>
        App(compile(operator), compile(operand), exp.pos)
      case _ => throw new Exception(s"Invalid lambda expression: $exp")
    }
    /* SExpParser.parse returns a list of s-expression, but we only want one. This
     * is not really safe but we are not focusing on that. */
    compile(SExpParser.parse(program).head)
  }
}

/** We want to perform some simple static analyses on lambda-calculus
  * programs. We compute, for each variable used in the program, if it can be
  * unbound, and if not, what are its possible values during evaluation. */
object LamAnalysis {
  /** Our analysis takes an input program as a string, and returns two elements:
   * 1. A set of expressions where unbound variables are evaluated
   * 2. A mapping from variable names to the values they can have
   */
  def analyze[L : LamLattice](program: String): (Set[LamExp], Map[String, L]) = {
    /* We first instantiate our semantics. It needs to know which lattice to use (we
     * use the type parameter L for that), which addresses (we use classical
     * addresses), and which timestamps (since we perform static analysis, we
     * need a finite set of timestamps, ZeroCFA gives us exactly this). */
    val sem = new LamSemantics[L, ClassicalAddress.A, ZeroCFA.T]
    /* We also instantiate an abstract machine, which needs the same components as
     * the semantics, as well as the kind of expression that we are working
     * with. We use an AAM machine here. */
    val machine = new AAM[LamExp, L, ClassicalAddress.A, ZeroCFA.T]
    /* We can then run the machine on the given program (that we have to parse), using our semantics */
    val result = machine.eval(sem.parse(program), sem,
      /* we need to generate a graph */
      true,
      /* we don't include a timeout for the analysis */
      None)

    val abs = implicitly[LamLattice[L]]
    /* This is the initial value of our analysis: by default, no variable is used,
     * so we have no unbound variable and no value associated to a variable */
    val init = (Set[LamExp](), Map[String, L]().withDefaultValue(abs.bottom))
    /* result contains sufficient information for us to find what we need. We need
     * to access its internals though (and that's ugly, ideas on how to cleanly
     * improve that are more than welcome!). */
    result.asInstanceOf[machine.AAMOutput].graph match {
      case None => println("No graph was computed!"); init /* oops, we don't have a graph and we needed one, we cannot run the analysis */
      case Some(g) =>
        /* g is a graph in which each node is an AAM state. We can extract the current
         * expression from these nodes: a ControlEval node is a node where we
         * evaluate an expression. We are only interested in nodes evaluating a
         * Var(x) expression. For each of these nodes, if x is bound, we remember the current
         * value of the variable x, otherwise we remember that it's unbound. */
        g.nodes.foldLeft(init)((acc, node) => node match {
          case machine.State(machine.ControlEval(exp @ Var(x, _), env), store, _, _, _) =>
            env.lookup(x) match {
              /* x bound to address a, we look it up in the store and join its value with the
               * information we already computed */
              case Some(a) => (acc._1, acc._2 + (x -> abs.join(acc._2(x), store.lookup(a))))
              /* unbound variable, we add its expression to the unboud variables */
              case None => (acc._1 + exp, acc._2)
            }
          /* We don't care about other nodes */
          case _ => acc
        })
    }
  }
  def main(args: Array[String]) {
    /* We run our analysis on a simple program */
    analyze[LamLatticeImpl.L]("((lambda (x) (x x)) (lambda (y) z))") match {
      case (unbound, mappings) =>
        /* We can extract positional information from the expressions of unbound variables */
        val unboundstr = unbound.map(x => s"$x at position ${x.pos}").mkString("\n")
        println(s"Unbound variables:\n$unboundstr")
        /* And we can list values of the bound variables that are used */
        val mappingsstr = mappings.toList.map({ case (x, v) => s"$x: $v" }).mkString("\n")
        println(s"Mappings:\n$mappingsstr")
    }
  }
}
