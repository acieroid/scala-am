import scala.util.parsing.input.Position

object DotLanguage {
  trait Term {
    val pos: Position
  }
  type Variable = String
  type TermMember = String
  case class Var(x: Variable, pos: Position) extends Term {
    override def toString = x
  }
  case class Lam(x: Variable, t: Term, pos: Position) extends Term {
    override def toString = s"λ($x) $t"
  }
  case class App(x: Variable, y: Variable, pos: Position) extends Term {
    override def toString = s"$x $y"
  }
  case class Let(x: Variable, t: Term, u: Term, pos: Position) extends Term {
    override def toString = s"let $x = $t in $u"
  }
  case class Sel(x: Variable, a: TermMember, pos: Position) extends Term {
    override def toString = s"$x.$a"
  }
  case class Obj(x: Variable, d: Definition, pos: Position) extends Term {
    override def toString = s"ν(x) $d"
  }

  trait Definition {
    val pos: Position
  }
  case class Field(a: TermMember, t: Term, pos: Position) extends Definition {
    override def toString = s"{$a = $t}"
  }
  case class Aggregate(d1: Definition, d2: Definition, pos: Position) extends Definition {
    override def toString = s"$d1 ∧ $d2"
  }

  object Term {
    implicit val isExp: Expression[Term] = new Expression[Term] {
      def pos(t: Term) = t.pos
    }
  }

  trait DotLattice[L] extends JoinLattice[L] {
    /* Injects closures, which can either be a lambda (function), or a nu (object) */
    def lambda[Addr : Address](v: Variable, body: Term, env: Environment[Addr]): L
    def obj[Addr : Address](v: Variable, defs: Definition, env: Environment[Addr]): L
    def getClosures[Addr : Address](x: L): Set[(Variable, Term, Environment[Addr])]
    def getObjects[Addr : Address](x: L): Set[(Variable, Definition, Environment[Addr])]
  }

  object DotLatticeImpl {
    sealed trait Value
    case class Closure[Addr : Address](v: Variable, body: Term, env: Environment[Addr]) extends Value {
      override def toString = s"#<λ ($v) $body>"
    }
    case class Obj[Addr : Address](v: Variable, defs: Definition, env: Environment[Addr]) extends Value {
      override def toString = s"#<ν ($v) $defs>"
    }
    case class L(elements: Set[Value]) {
      override def toString = elements.mkString(", ")
    }
    implicit val isDotLattice: DotLattice[L] = new DotLattice[L] {
      def bottom: L = L(Set[Value]())
      def join(x: L, y: L): L = L(x.elements ++ y.elements)
      def subsumes(x: L, y: L): Boolean = y.elements.subsetOf(x.elements)
      def name = "DotLattice"
      def counting = false
      def isPrimitiveValue(x: L) = false

      def lambda[Addr : Address](v: Variable, body: Term, env: Environment[Addr]) =
        L(Set[Value](Closure[Addr](v, body, env)))
      def obj[Addr : Address](v: Variable, defs: Definition, env: Environment[Addr]) =
        L(Set[Value](Obj[Addr](v, defs, env)))
      def getClosures[Addr : Address](x: L) = {
        def getClo(x: Value): Option[(Variable, Term, Environment[Addr])] = x match {
          case Closure(v, body, env : Environment[Addr] @unchecked) => Some((v, body, env))
          case _ => None
        }
        x.elements.flatMap(getClo)
      }
      def getObjects[Addr : Address](x: L) = {
        def getObj(x: Value): Option[(Variable, Definition, Environment[Addr])] = x match {
          case Obj(v, defs, env: Environment[Addr] @unchecked) => Some((v, defs, env))
          case _ => None
        }
        x.elements.flatMap(getObj)
      }
    }
  }

  class DotSemantics[Abs : DotLattice, Addr : Address, Time : Timestamp]
      extends BaseSemantics[Term, Abs, Addr, Time] {
    def dabs = implicitly[DotLattice[Abs]]
    type Env = Environment[Addr]
    type Sto = Store[Addr, Abs]
    trait DotFrame extends Frame {
      def subsumes(that: Frame) = that.equals(this)
    }
    case class FrameLet(x: Variable, u: Term, env: Env) extends DotFrame

    private def evalVar(x: Variable, env: Env, store: Sto): MayFail[Abs] = env.lookup(x) match {
      case Some(a) => store.lookup(a) match {
        case Some(v) => MayFailSuccess(v)
        case None => MayFailError(List(UnboundAddress(a.toString)))
      }
      case None => MayFailError(List(UnboundVariable(x)))
    }

    private def toErr(err: SemanticError): Set[Action[Term, Abs, Addr]] = Set(ActionError(err))
    private def findTermMember(defs: Definition, a: TermMember): Option[Term] = defs match {
      case Field(a2, t, _) if a2 == a => Some(t)
      case Field(_, _, _) => None
      case Aggregate(d1, d2, _) => findTermMember(d1, a) match {
        case Some(t) => Some(t)
        case None => findTermMember(d2, a)
      }
    }


    def stepEval(t: Term, env: Env, store: Sto, time: Time) = t match {
      case Var(x, _) => evalVar(x, env, store).collect(v => Set(ActionReachedValue(v, store)), err => Set(ActionError(err)))
      case Lam(x, t, _) => Set(ActionReachedValue(dabs.lambda(x, t, env), store))
      case App(x, y, _) => evalVar(x, env, store).bind(fun =>
        evalVar(y, env, store).map(arg => {
          val res: Set[Action[Term, Abs, Addr]] = dabs.getClosures[Addr](fun).map({
            case (x, body, env) => {
              val a = addr.variable(x, arg, time)
              ActionEval[Term, Abs, Addr](t, env.extend(x, a), store.extend(a, arg))
            }
          })
          res
        })).collect(actions => actions, toErr)
      case Let(x, t, u, _) => Set(ActionPush(FrameLet(x, u, env), t, env, store))
      case Sel(x, a, _) => evalVar(x, env, store).map(obj => {
        val res: Set[Action[Term, Abs, Addr]] =  dabs.getObjects[Addr](obj).map({
          case (x, defs, env) =>
            findTermMember(defs, a) match {
              case Some(t) => {
                val ad = addr.variable(x, obj, time)
                ActionEval(t, env.extend(x, ad), store.extend(ad, obj))
              }
              case None => ActionError[Term, Abs, Addr](UserError("no term member $a in object $obj", t.pos))
            }
        })
        res
      }).collect(actions => actions, toErr)
      case Obj(x, d, _) => Set(ActionReachedValue(dabs.obj(x, d, env), store))
    }

    def stepKont(v: Abs, frame: Frame, store: Sto, time: Time) = frame match {
      case FrameLet(x, u, env) => {
        val a = addr.variable(x, v, time)
        Set(ActionEval(u, env.extend(x, a), store.extend(a, v)))
      }
    }

    def parse(program: String): Term = {
      def compileDefs(exp: SExp): Definition = exp match {
        /* case: (aggregate def def) */
        case SExpPair(SExpIdentifier("aggregate", _), SExpPair(def1, SExpPair(def2, SExpValue(ValueNil, _), _), _), _) =>
          Aggregate(compileDefs(def1), compileDefs(def2), exp.pos)
        /* case: (a t) */
        case SExpPair(SExpIdentifier(a, _), SExpPair(t, SExpValue(ValueNil, _), _), _) =>
          Field(a, compile(t), exp.pos)
      }
      def compile(exp: SExp): Term = exp match {
        /* case: (lambda (x) e) */
        case SExpPair(SExpIdentifier("lambda", _), SExpPair(SExpPair(SExpIdentifier(arg, _), SExpValue(ValueNil, _), _),
          SExpPair(body, SExpValue(ValueNil, _), _), _), _) =>
          Lam(arg, compile(body), exp.pos)
        /* case: (nu (x) defs) */
        case SExpPair(SExpIdentifier("nu", _), SExpPair(SExpPair(SExpIdentifier(arg, _), SExpValue(ValueNil, _), _),
          SExpPair(defs, SExpValue(ValueNil, _), _), _), _) =>
          Obj(arg, compileDefs(defs), exp.pos)
        /* case: (let ((x t)) u) */
        case SExpPair(SExpIdentifier("let", _), SExpPair(SExpPair(SExpPair(SExpIdentifier(x, _), SExpPair(t, SExpValue(ValueNil, _), _), _), SExpValue(ValueNil, _), _), SExpPair(u, SExpValue(ValueNil, _), _), _), _)=>
          Let(x, compile(t), compile(u), exp.pos)
        /* case: (sel x a) */
        case SExpPair(SExpIdentifier("sel", _), SExpPair(SExpIdentifier(x, _), SExpPair(SExpIdentifier(a, _), SExpValue(ValueNil, _), _), _), _) =>
          Sel(x, a, exp.pos)
        /* case: (x y) */
        case SExpPair(SExpIdentifier(x, _), SExpPair(SExpIdentifier(y, _), SExpValue(ValueNil, _), _), _) =>
          App(x, y, exp.pos)
        /* case : x */
        case SExpIdentifier(x, _) =>
          Var(x, exp.pos)
      }
      compile(SExpParser.parse(program).head)
    }
  }
}

object Dot {
  import DotLanguage._
  def main(args: Array[String]) {
    if (args.length >= 1) {
      val lattice = DotLatticeImpl
      implicit val isDotLattice = lattice.isDotLattice
      val sem = new DotSemantics[lattice.L, ClassicalAddress.A, ZeroCFA.T]
      val machine = new AAM[Term, lattice.L, ClassicalAddress.A, ZeroCFA.T]
      val res = machine.eval(sem.parse(args(0)), sem, true, None)
      res.toDotFile("foo.dot")
    } else {
      println("Please provide input program as argument")
    }
  }
}
