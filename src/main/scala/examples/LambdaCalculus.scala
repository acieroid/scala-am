/* We show here how to add support for a new language and how to build static
 * analyses for the language. We do it for lambda calculus */

/** A lambda calculus expression is represented by a LamExp. It needs to have a
  * position to identify its location in the input file. */
trait LamExp {
  val pos: Position
}
/** We have to tell scala-am that LamExp are actually expressions */
object LamExp {
  implicit val isExp: Expression[LamExp] = new Expression[LamExp] {
    def pos(e: LamExp) = e.pos
  }
}
/** An abstraction: lambda x. e */
case class Lam(x: Identifier, e: LamExp, pos: Position) extends LamExp {
  override def toString = s"(lambda ($x) $e)"
}
/** An application: (e1 e2) */
case class App(e1: LamExp, e2: LamExp, pos: Position) extends LamExp {
  override def toString = s"($e1 $e2)"
}
/** A variable reference: x */
case class Var(x: Identifier, pos: Position) extends LamExp {
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
    /** We don't have primitive values (the only values we have are closures) */
    def isPrimitiveValue(x: L) = false
    /** The cardinality is just the number of elements in the set */
    def cardinality(x: L) = CardinalityNumber(x.elements.size)

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
  trait LamFrame extends Frame
  /** One frame to remember the operand when we evaluate the operator */
  case class FrameArg(e: LamExp, env: Env) extends LamFrame
  /** And one frame to remember the operator value when we evaluate the operand */
  case class FrameFun(v: Abs) extends LamFrame

  /** The stepEval function defines how to perform an evaluation step on an
    * expression */
  def stepEval(e: LamExp, env: Env, store: Sto, t: Time) = e match {
    /* A lambda evaluate to a closure by pairing it with the current environment,
     * and injecting this in the abstract domain */
    case Lam(_, _, _) => Action.value(labs.inject((e, env)), store)
    /* To evaluate an application, we first have to evaluate e1, and we push a
     * continuation to remember to evaluate e2 in the environment env */
    case App(e1, e2, _) => Action.push(FrameArg(e2, env), e1, env, store)
    /* To evaluate a variable, just look it up in the store */
    case Var(x, _) => env.lookup(x.name) match {
      case Some(a) => store.lookup(a) match {
        case Some(v) => Action.value(v, store)
        case None => Action.error(UnboundAddress(a.toString))
      }
      case None => Action.error(UnboundVariable(x))
    }
  }

  /** The stepKont function defines how to behave when we reached a value v and we
    * have frame as the top continuation on the stack */
  def stepKont(v: Abs, frame: Frame, store: Sto, t: Time) = frame match {
    /* We have evaluated the operator v but still need to evaluate the operator e */
    case FrameArg(e, env) => Set(ActionPush(FrameFun(v), e, env, store))
    /* We have evaluated both the operator (fun) and the operand (v). We go through
     * the possible closures bound to the operator and for each of them, we
     * have to evaluate their body by extending their environment with their
     * argument */
    case FrameFun(fun) => labs.getClosures[LamExp, Addr](fun).map({
      case (Lam(x, e, _), env) => {
        val a = addr.variable(x, v, t)
        Action.eval(e, env.extend(x.name, a), store.extend(a, v))
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
      case SExpPair(SExpId(Identifier("lambda", _)), SExpPair(SExpPair(SExpId(arg), SExpValue(ValueNil, _), _), SExpPair(body, SExpValue(ValueNil, _), _), _), _) =>
        Lam(arg, compile(body), exp.pos)
      /* case: x */
      case SExpId(x) =>
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

/** This is our unbound variables analysis. We want to detect, for a
  * lambda-calculus program, which evaluated variables may be unbound. We
  * represent this by a set of lambda expressions, which is the lattice computed
  * by this analysis: Set[LamExp]. This class defines how to update the current
  * state of the analysis. */
case class UnboundVariablesAnalysis[Abs : JoinLattice, Addr : Address, Time: Timestamp]()
    extends BaseAnalysis[Set[LamExp], LamExp, Abs, Addr, Time] {
  /** stepEval is called when the semantics' stepEval is called */
  def stepEval(e: LamExp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: Set[LamExp]) = e match {
    /* When we evaluate a variable... */
    case Var(x, _) => env.lookup(x.name) match {
      case Some(a) => current /* if it is bound, then we don't care about it and don't change the result of the analysis */
      case None => current + e /* if it's an unbound variable, we add it to the results */
    }
    /* we ignore any other expression */
    case _ => current
  }
  /** No unbound variables appear when a continuation is boing popped */
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: Set[LamExp]) = current
  /** The checking for unbound variables could be done here, by looking at the
   * "err" parameter, which is UnboundVariable(name). But that's not how we
   * proceed for this analysis, because we want the full expression (and also,
   * we want to show how to describe the analysis in terms of stepEval). */
  def error(err: SemanticError, current: Set[LamExp]) = current
  /** Joining two results is done by taking their union */
  def join(x: Set[LamExp], y: Set[LamExp]) = x ++ y
  /** At the beginning of the program, no unbound variable has been evaluated */
  def init = Set[LamExp]()
}

/** We want to perform a simple static analysis on lambda-calculus programs. We
  * compute the possible unbound variables that are evaluated in the execution
  * of a program. */
object LamAnalysis {
  /** Our analysis takes an input program as a string, and returns a set of
   *  expressions where unbound variables are evaluated.
   */
  def analyze[L : LamLattice](program: String): Set[LamExp] = {
    /* We first instantiate our semantics. It needs to know which lattice to use (we
     * use the type parameter L for that), which addresses (we use classical
     * addresses), and which timestamps (since we perform static analysis, we
     * need a finite set of timestamps, ZeroCFA gives us exactly this). */
    val sem = new LamSemantics[L, ClassicalAddress.A, ZeroCFA.T]
    /* We also instantiate an abstract machine, which needs the same components as
     * the semantics, as well as the kind of expression that we are working
     * with. We use an AAM machine here. */
    val machine = new AAM[LamExp, L, ClassicalAddress.A, ZeroCFA.T]
    /* We finally instantiate our analysis itself */
    val analysis = UnboundVariablesAnalysis[L, ClassicalAddress.A, ZeroCFA.T]
    /* We can then analyze the given program using the machine, our semantics and our analysis */
    machine.analyze(sem.parse(program), sem, analysis,
      /* we don't include a timeout for the analysis */
      None) match {
      case Some(v) => v
      case None => println("Analysis did not succeed..."); Set()
    }
  }
  def main(args: Array[String]) {
    /* We run our analysis on a simple program */
    val unbound = analyze[LamLatticeImpl.L]("((lambda (x) (x x)) (lambda (y) z))")
    /* We can extract positional information from the expressions of unbound variables */
    val unboundstr = unbound.map(x => s"$x at position ${x.pos}").mkString("\n")
    println(s"Unbound variables:\n$unboundstr")
  }
}
