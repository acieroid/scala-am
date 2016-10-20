import scala.util.parsing.input.Position
import scalaz.Scalaz._

class DotLanguage[Addr : Address] {
  val addr = implicitly[Address[Addr]]
  type Env = Environment[Addr]

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
    def lambda(v: Variable, body: Term, env: Env): L
    def obj(v: Variable, defs: Definition, env: Env): L
    def getClosures(x: L): Set[(Variable, Term, Env)]
    def getObjects(x: L): Set[(Variable, Definition, Env)]
  }

  object DotLatticeImpl {
    sealed trait Value
    case class Closure(v: Variable, body: Term, env: Env) extends Value {
      override def toString = s"#<λ ($v) $body>"
    }
    case class Obj(v: Variable, defs: Definition, env: Env) extends Value {
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
      def cardinality(x: L) = CardinalityNumber(x.elements.length)

      def lambda(v: Variable, body: Term, env: Env) =
        L(Set[Value](Closure(v, body, env)))
      def obj(v: Variable, defs: Definition, env: Env) =
        L(Set[Value](Obj(v, defs, env)))
      def getClosures(x: L) = {
        def getClo(x: Value): Option[(Variable, Term, Env)] = x match {
          case Closure(v, body, env) => Some((v, body, env))
          case _ => None
        }
        x.elements.flatMap(getClo)
      }
      def getObjects(x: L) = {
        def getObj(x: Value): Option[(Variable, Definition, Env)] = x match {
          case Obj(v, defs, env) => Some((v, defs, env))
          case _ => None
        }
        x.elements.flatMap(getObj)
      }
    }
  }

  class DotSemantics[Abs : DotLattice, Time : Timestamp]
      extends BaseSemantics[Term, Abs, Addr, Time] {
    def dabs = implicitly[DotLattice[Abs]]
    type Sto = Store[Addr, Abs]
    trait DotFrame extends Frame
    case class FrameLet(x: Variable, u: Term, env: Env) extends DotFrame

    private def evalVar(x: Variable, env: Env, store: Sto): MayFail[Abs] = env.lookup(x) match {
      case Some(a) => store.lookup(a) match {
        case Some(v) => v
        case None => UnboundAddress(a.toString)
      }
      case None => UnboundVariable(x)
    }

    private def findTermMember(defs: Definition, a: TermMember): Option[Term] = defs match {
      case Field(a2, t, _) if a2 == a => Some(t)
      case Field(_, _, _) => None
      case Aggregate(d1, d2, _) => findTermMember(d1, a) match {
        case Some(t) => Some(t)
        case None => findTermMember(d2, a)
      }
    }

    case class NoTermMember(member: String, obj: String, pos: Position) extends SemanticError
    def stepEval(t: Term, env: Env, store: Sto, time: Time) = t match {
      case Var(x, _) => for {
        v <- evalVar(x, env, store)
      } yield Action.value(v, store)
      case Lam(x, t, _) =>
        Action.value(dabs.lambda(x, t, env), store)
      case Obj(x, d, _) =>
        Action.value(dabs.obj(x, d, env), store)
      case App(x, y, _) => for {
        fun <- evalVar(x, env, store)
        arg <- evalVar(y, env, store)
      } yield dabs.getClosures(fun).map({
        case (x, t, env) =>
          val a = addr.variable(x, arg, time)
          Action.eval(t, env.extend(x, a), store.extend(a, arg))
      })
      case Let(x, t, u, _) =>
        Action.push(FrameLet(x, u, env), t, env, store)
      case Sel(x, a, _) => for {
        obj <- evalVar(x, env, store)
      } yield dabs.getObjects(obj).map({
        case (x, defs, env) =>
          findTermMember(defs, a) match {
            case Some(t) =>
              val ad = addr.variable(x, obj, time)
              Action.eval(t, env.extend(x, ad), store.extend(ad, obj))
            case None =>
              Action.error(NoTermMember(a, obj.toString, t.pos))
          }
      })
    }

    def stepKont(v: Abs, frame: Frame, store: Sto, time: Time) = frame match {
      case FrameLet(x, u, env) =>
        val a = addr.variable(x, v, time)
        Action.eval(u, env.extend(x, a), store.extend(a, v))
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
  def main(args: Array[String]) {
    if (args.length >= 1) {
      val dot = new DotLanguage[ClassicalAddress.A]
      import dot._
      val lattice = DotLatticeImpl
      implicit val isDotLattice = lattice.isDotLattice
      val sem = new DotSemantics[lattice.L, ZeroCFA.T]
      val machine = new AAM[Term, lattice.L, ClassicalAddress.A, ZeroCFA.T]
      val res = machine.eval(sem.parse(args(0)), sem, true, None)
      res.toDotFile("foo.dot")
    } else {
      println("Please provide input program as argument")
    }
  }
}
