import scalaz.{Plus => _, _}
import scalaz.Scalaz._
import SchemeOps._

class MakeSchemeLattice[
  S : StringLattice,
  B : BoolLattice,
  I : IntLattice,
  F : FloatLattice,
  C : CharLattice,
  Sym : SymbolLattice
](supportsCounting: Boolean) {
  sealed trait Value
  case object Bot extends Value {
    override def toString = "⊥"
  }
  case class Str(s: S) extends Value {
    override def toString = StringLattice[S].shows(s)
  }
  case class Bool(b: B) extends Value {
    override def toString = BoolLattice[B].shows(b)
  }
  case class Int(i: I) extends Value {
    override def toString = IntLattice[I].shows(i)
  }
  case class Float(f: F) extends Value {
    override def toString = FloatLattice[F].shows(f)
  }
  case class Char(c: C) extends Value {
    override def toString = CharLattice[C].shows(c)
  }
  case class Symbol(s: Sym) extends Value {
    override def toString = SymbolLattice[Sym].shows(s)
  }
  case class Prim[Addr : Address, Abs : JoinLattice](prim: Primitive[Addr, Abs]) extends Value {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Closure[Exp : Expression, Addr : Address](lambda: Exp, env: Environment[Addr]) extends Value {
    override def toString = "#<clo>"
  }
  case class Cons[Addr : Address](car: Addr, cdr: Addr) extends Value
  case object Nil extends Value {
    override def toString = "()"
  }
  case class Vec[Addr : Address](size: I, elements: Map[I, Addr], init: Addr) extends Value {
    override def toString = {
      val els = elements.toList.map({
        case (k, v) => s"${IntLattice[I].shows(k)}: $v"
      }).mkString(", ")
      s"Vec(${IntLattice[I].shows(size)}, {$els}, $init)"
    }
  }
  case class VectorAddress[Addr : Address](a: Addr) extends Value

  val True = Bool(BoolLattice[B].inject(true))
  val False = Bool(BoolLattice[B].inject(false))

  type L = Value

  /* TODO: don't use exceptions */
  case class CannotJoin[Abs](values: Set[Abs]) extends Exception {
    override def toString = "CannotJoin(" + values.mkString(", ") + ")"
  }

  val isSchemeLattice: IsSchemeLattice[L] = new IsSchemeLattice[L] {
    def bottom = Bot
    def join(x: L, y: L): L = if (x == y) { x } else {
      (x, y) match {
        case (Bot, _) => y
        case (_, Bot) => x
        case (Str(s1), Str(s2)) => Str(StringLattice[S].join(s1, s2))
        case (Bool(b1), Bool(b2)) => Bool(BoolLattice[B].join(b1, b2))
        case (Int(i1), Int(i2)) => Int(IntLattice[I].join(i1, i2))
        case (Float(f1), Float(f2)) => Float(FloatLattice[F].join(f1, f2))
        case (Char(c1), Char(c2)) => Char(CharLattice[C].join(c1, c2))
        case _ => throw new CannotJoin[L](Set(x, y))
      }
    }
    def subsumes(x: L, y: L): Boolean = if (x == y) { true } else {
      (x, y) match {
        case (_, Bot) => true
        case (Str(s1), Str(s2)) => StringLattice[S].subsumes(s1, s2)
        case (Bool(b1), Bool(b2)) => BoolLattice[B].subsumes(b1, b2)
        case (Int(i1), Int(i2)) => IntLattice[I].subsumes(i1, i2)
        case (Float(f1), Float(f2)) => FloatLattice[F].subsumes(f1, f2)
        case (Char(c1), Char(c2)) => CharLattice[C].subsumes(c1, c2)
        case _ => false
      }
    }
    val name = s"Lattice(${StringLattice[S].name}, ${BoolLattice[B].name}, ${IntLattice[I].name}, ${FloatLattice[F].name}, ${CharLattice[C].name}, ${SymbolLattice[Sym].name})"
    val counting = supportsCounting

    def isPrimitiveValue(x: L): Boolean = x match {
      case Bot | Str(_) | Bool(_) | Int(_) | Float(_) | Char(_) | Symbol(_) | Nil => true
      case Closure(_, _) | Prim(_) | Cons(_, _) | VectorAddress(_) | Vec(_, _, _) => false
    }

    def cardinality(x: L): Cardinality = x match {
      case Bot => CardinalityNumber(0)
      case Str(s) => StringLattice[S].cardinality(s)
      case Bool(b) => BoolLattice[B].cardinality(b)
      case Int(i) => IntLattice[I].cardinality(i)
      case Float(f) => FloatLattice[F].cardinality(f)
      case Char(c) => CharLattice[C].cardinality(c)
      case Symbol(s) => SymbolLattice[Sym].cardinality(s)
      case Nil => CardinalityNumber(1)
      case Closure(_, _) | Prim(_) | Cons(_, _) | VectorAddress(_) | Vec(_, _, _)  => CardinalityNumber(1)
    }

    def isTrue(x: L): Boolean = x match {
      case Bool(b) => BoolLattice[B].isTrue(b)
      case Bot => false
      case _ => true
    }
    def isFalse(x: L): Boolean = x match {
      case Bool(b) => BoolLattice[B].isFalse(b)
      case Bot => true
      case _ => false
    }

    import scala.language.implicitConversions
    implicit def mayFailSuccess(l: L): MayFail[L] = MayFailSuccess(l)
    implicit def mayFailError(err: SemanticError): MayFail[L] = MayFailError(List(err))
    def unaryOp(op: UnaryOperator)(x: L): MayFail[L] = if (x == Bot) { Bot } else { op match {
      case IsNull => x match {
        case Nil => True
        case _ => False
      }
      case IsCons => x match {
        case Cons(_, _) => True
        case _ => False
      }
      case IsChar => x match {
        case Char(_) => True
        case _ => False
      }
      case IsSymbol => x match {
        case Symbol(_) => True
        case _ => False
      }
      case IsString => x match {
        case Str(_) => True
        case _ => False
      }
      case IsInteger => x match {
        case Int(_) => True
        case _ => False
      }
      case IsFloat => x match {
        case Float(_) => True
        case _ => False
      }
      case IsBoolean => x match {
        case Bool(_) => True
        case _ => False
      }
      case IsVector => x match {
        case Vec(_, _, _) => True
        case VectorAddress(_) => True
        case _ => False
      }
      case Not => x match {
        case Bool(b) => Bool(BoolLattice[B].not(b))
        case _ => False /* any value is true */
      }
      case Ceiling => x match {
        case Int(n) => Int(n)
        case Float(n) => Float(FloatLattice[F].ceiling(n))
        case _ => OperatorNotApplicable("ceiling", List(x.toString))
      }
      case Log => x match {
        case Int(n) => Float(FloatLattice[F].log(IntLattice[I].toFloat(n)))
        case Float(n) => Float(FloatLattice[F].log(n))
        case _ => OperatorNotApplicable("log", List(x.toString))
      }
      case Random => x match {
        case Int(n) => Int(IntLattice[I].random(n))
        case Float(n) => Float(FloatLattice[F].random(n))
        case _ => OperatorNotApplicable("random", List(x.toString))
      }
      case VectorLength => x match {
        case Vec(size, _, _) => Int(size)
        case _ => OperatorNotApplicable("vector-length", List(x.toString))
      }
      case StringLength => x match {
        case Str(s) => Int(StringLattice[S].length(s))
        case _ => OperatorNotApplicable("string-length", List(x.toString))
      }
      case NumberToString => x match {
        case Int(n) => Str(IntLattice[I].toString(n))
        case Float(n) => Str(FloatLattice[F].toString(n))
        case _ => OperatorNotApplicable("number->string", List(x.toString))
      }
    }}

    def binaryOp(op: BinaryOperator)(x: L, y: L): MayFail[L] = if (x == Bot || y == Bot) { Bot } else {
      op match {
        case Plus => (x, y) match {
          case (Int(n1), Int(n2)) => Int(IntLattice[I].plus(n1, n2))
          case (Int(n1), Float(n2)) => Float(FloatLattice[F].plus(IntLattice[I].toFloat(n1), n2))
          case (Float(n1), Int(n2)) => Float(FloatLattice[F].plus(n1, IntLattice[I].toFloat(n2)))
          case (Float(n1), Float(n2)) => Float(FloatLattice[F].plus(n1, n2))
          case _ => OperatorNotApplicable("+", List(x.toString, y.toString))
        }
        case Minus => (x, y) match {
          case (Int(n1), Int(n2)) => Int(IntLattice[I].minus(n1, n2))
          case (Int(n1), Float(n2)) => Float(FloatLattice[F].minus(IntLattice[I].toFloat(n1), n2))
          case (Float(n1), Int(n2)) => Float(FloatLattice[F].minus(n1, IntLattice[I].toFloat(n2)))
          case (Float(n1), Float(n2)) => Float(FloatLattice[F].minus(n1, n2))
          case _ => OperatorNotApplicable("-", List(x.toString, y.toString))
        }
        case Times => (x, y) match {
          case (Int(n1), Int(n2)) => Int(IntLattice[I].times(n1, n2))
          case (Int(n1), Float(n2)) => Float(FloatLattice[F].times(IntLattice[I].toFloat(n1), n2))
          case (Float(n1), Int(n2)) => Float(FloatLattice[F].times(n1, IntLattice[I].toFloat(n2)))
          case (Float(n1), Float(n2)) => Float(FloatLattice[F].times(n1, n2))
          case _ => OperatorNotApplicable("*", List(x.toString, y.toString))
        }
        /* TODO: have a div for integer division (i.e., Scheme's quotient), and one for real division (/)). Also, handle division by zero. */
        case Div => (x, y) match {
          case (Int(n1), Int(n2)) => Int(IntLattice[I].div(n1, n2))
          case (Int(n1), Float(n2)) => Float(FloatLattice[F].div(IntLattice[I].toFloat(n1), n2))
          case (Float(n1), Int(n2)) => Float(FloatLattice[F].div(n1, IntLattice[I].toFloat(n2)))
          case (Float(n1), Float(n2)) => Float(FloatLattice[F].div(n1, n2))
          case _ => OperatorNotApplicable("/", List(x.toString, y.toString))
        }
        case Modulo => (x, y) match {
          case (Int(n1), Int(n2)) => Int(IntLattice[I].modulo(n1, n2))
          case _ => OperatorNotApplicable("modulo", List(x.toString, y.toString))
        }
        case Lt => (x, y) match {
          case (Int(n1), Int(n2)) => Bool(IntLattice[I].lt(n1, n2))
          case (Int(n1), Float(n2)) => Bool(FloatLattice[F].lt(IntLattice[I].toFloat(n1), n2))
          case (Float(n1), Int(n2)) => Bool(FloatLattice[F].lt(n1, IntLattice[I].toFloat(n2)))
          case (Float(n1), Float(n2)) => Bool(FloatLattice[F].lt(n1, n2))
          case _ => OperatorNotApplicable("<", List(x.toString, y.toString))
        }
        case NumEq => (x, y) match {
          case (Int(n1), Int(n2)) => Bool(IntLattice[I].eql(n1, n2))
          case (Int(n1), Float(n2)) => Bool(FloatLattice[F].eql(IntLattice[I].toFloat(n1), n2))
          case (Float(n1), Int(n2)) => Bool(FloatLattice[F].eql(n1, IntLattice[I].toFloat(n2)))
          case (Float(n1), Float(n2)) => Bool(FloatLattice[F].eql(n1, n2))
          case _ => OperatorNotApplicable("number=", List(x.toString, y.toString))
        }
        case Eq => (x, y) match {
          case (Str(s1), Str(s2)) => Bool(StringLattice[S].eql(s1, s2)) /* TODO: this isn't really physical equality for strings */
          case (Bool(b1), Bool(b2)) => Bool(BoolLattice[B].eql(b1, b2))
          case (Int(n1), Int(n2)) => Bool(IntLattice[I].eql(n1, n2))
          case (Float(n1), Float(n2)) => Bool(FloatLattice[F].eql(n1, n2))
          case (Char(c1), Char(c2)) => Bool(CharLattice[C].eql(c1, c2))
          case (Symbol(s1), Symbol(s2)) => Bool(SymbolLattice[Sym].eql(s1, s2))
          case (Nil, Nil) => True
          case (Prim(_), Prim(_)) => Bool(BoolLattice[B].inject(x == y))
          case (Closure(_, _), Closure(_, _)) => Bool(BoolLattice[B].inject(x == y))
          case (Cons(_, _), Cons(_, _)) => Bool(BoolLattice[B].inject(x == y))
          case (VectorAddress(_), VectorAddress(_)) => Bool(BoolLattice[B].inject(x == y))
          case _ => False
        }
        case StringAppend => (x, y) match {
          case (Str(s1), Str(s2)) => Str(StringLattice[S].append(s1, s2))
          case _ => OperatorNotApplicable("string-append", List(x.toString, y.toString))
        }
      }
    }

    def inject(x: scala.Int): L = Int(IntLattice[I].inject(x))
    def inject(x: scala.Float): L = Float(FloatLattice[F].inject(x))
    def inject(x: String): L = Str(StringLattice[S].inject(x))
    def inject(x: scala.Char): L = Char(CharLattice[C].inject(x))
    def inject(x: Boolean): L = Bool(BoolLattice[B].inject(x))
    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): L = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = Closure(x._1, x._2)
    def injectSymbol(x: String): L = Symbol(SymbolLattice[Sym].inject(x))
    def nil: L = Nil
    def cons[Addr : Address](car: Addr, cdr: Addr): L = Cons(car, cdr)

    def getClosures[Exp : Expression, Addr : Address](x: L) = x match {
      case Closure(lam: Exp @unchecked, env: Environment[Addr] @unchecked) => Set((lam, env))
      case _ => Set()
    }
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L) = x match {
      case Prim(p: Primitive[Addr, Abs] @unchecked) => Set(p)
      case _ => Set()
    }

    def car[Addr : Address](x: L): Set[Addr] = x match {
      case Cons(car: Addr @unchecked, cdr: Addr @unchecked) => Set(car)
      case _ => Set()
    }

    def cdr[Addr : Address](x: L): Set[Addr] = x match {
      case Cons(car: Addr @unchecked, cdr: Addr @unchecked) => Set(cdr)
      case _ => Set()
    }

    def vectorRef[Addr : Address](vector: L, index: L): MayFail[Set[Addr]] = (vector, index) match {
      case (Vec(size, content: Map[I, Addr] @unchecked, init: Addr @unchecked), Int(index)) => {
        val comp = IntLattice[I].lt(index, size)
        val t: Set[Addr] = if (BoolLattice[B].isTrue(comp)) {
          val vals = content.filterKeys(index2 => BoolLattice[B].isTrue(IntLattice[I].eql(index, index2))).values
          /* init doesn't have to be included if we know for sure that index is precise enough */
          vals.foldLeft(Set(init))((acc, v) => acc + v)
        } else { Set() }
        /* Don't perform bound checks here because we would get too many spurious flows */
        val f: Set[Addr] = Set()
        MayFailSuccess(t ++ f)
      }
      case (_: Vec[Addr] @unchecked, _) => MayFailError(List(OperatorNotApplicable("vector-ref", List(vector.toString, index.toString))))
      case _ => MayFailError(List(OperatorNotApplicable("vector-ref", List(vector.toString, index.toString))))
    }

    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])] = (vector, index) match {
      case (Vec(size, content: Map[I, Addr] @unchecked, init: Addr @unchecked), Int(index)) => {
        val comp = IntLattice[I].lt(index, size)
        val t: (L, Set[Addr]) = if (BoolLattice[B].isTrue(comp)) {
          content.get(index) match {
            case Some(a: Addr @unchecked) => (vector, Set(a))
            case None => (Vec(size, content + (index -> addr), init), Set(addr))
          }
        } else { (Bot, Set()) }
        val f: (L, Set[Addr]) = (Bot, Set())
        MayFailSuccess((join(t._1, f._1), t._2 ++ f._2))
      }
      case (_: Vec[Addr] @unchecked, _) => MayFailError(List(OperatorNotApplicable("vector-set!", List(vector.toString, index.toString, addr.toString))))
      case _ => MayFailError(List(OperatorNotApplicable("vector-set!", List(vector.toString, index.toString, addr.toString))))
    }

    def getVectors[Addr : Address](x: L) = x match {
      case VectorAddress(a: Addr @unchecked) => Set(a)
      case _ => Set()
    }

    def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)] = size match {
      case Int(size) => MayFailSuccess((VectorAddress(addr), Vec(size, Map[I, Addr](), init)))
      case _ => MayFailError(List(OperatorNotApplicable("vector", List(addr.toString, size.toString, init.toString))))
    }
  }

  sealed trait LSet
  case class Element(v: Value) extends LSet {
    override def toString = v.toString
  }
  case class Elements(vs: Set[Value]) extends LSet {
    override def toString = "{" + vs.mkString(",") + "}"
  }
  val boolOrMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x || y
    def zero: Boolean = false
  }
  val boolAndMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x && y
    def zero: Boolean = true
  }
  private def wrap(x: => Value): LSet = try { Element(x) } catch {
    case err: CannotJoin[Value] @unchecked => Elements(err.values)
  }
  implicit val lsetMonoid = new Monoid[LSet] {
    def append(x: LSet, y: => LSet): LSet = x match {
      case Element(Bot) => y
      case Element(a) => y match {
        case Element(Bot) => x
        case Element(b) => wrap(isSchemeLattice.join(a, b))
        case _: Elements => append(Elements(Set(a)), y)
      }
      case Elements(as) => y match {
        case Element(Bot) => x
        case Element(b) => append(x, Elements(Set(b)))
        case Elements(bs) =>
          /* every element in the other set has to be joined in this set */
          Elements(as.foldLeft(bs)((acc, x2) =>
            if (acc.exists(x1 => isSchemeLattice.subsumes(x1, x2))) {
              /* the set already contains an element that subsumes x2, don't add it to the set */
              acc
            } else {
              /* remove all elements subsumed by x2 and add x2 to the set */
              val subsumed = acc.filter(x1 => isSchemeLattice.subsumes(x2, x1))
              (acc -- subsumed) + x2
            }))
      }
    }
    def zero: LSet = Element(Bot)
  }
  implicit def mayFailMonoid[A](implicit monoid: Monoid[A]): Monoid[MayFail[A]] =
    new Monoid[MayFail[A]] {
      def append(x: MayFail[A], y: => MayFail[A]): MayFail[A] = (x, y) match {
        case (MayFailSuccess(x), MayFailSuccess(y)) => MayFailSuccess(monoid.append(x, y))
        case (MayFailSuccess(x), MayFailError(errs)) => MayFailBoth(x, errs)
        case (MayFailSuccess(x), MayFailBoth(y, errs)) => MayFailBoth(monoid.append(x, y), errs)
        case (MayFailError(errs), MayFailSuccess(x)) => MayFailBoth(x, errs)
        case (MayFailError(errs1), MayFailError(errs2)) => MayFailError(errs1 ++ errs2)
        case (MayFailError(errs1), MayFailBoth(x, errs2)) => MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs), MayFailSuccess(y)) => MayFailBoth(monoid.append(x, y), errs)
        case (MayFailBoth(x, errs1), MayFailError(errs2)) => MayFailBoth(x, errs1 ++ errs2)
        case (MayFailBoth(x, errs1), MayFailBoth(y, errs2)) => MayFailBoth(monoid.append(x, y), errs1 ++ errs2)
      }
      def zero: MayFail[A] = MayFailSuccess(monoid.zero)
    }
  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def append(x: Set[A], y: => Set[A]): Set[A] = x ++ y
    def zero: Set[A] = Set[A]()
  }
  private def foldMapLSet[B](x: LSet, f: L => B)(implicit b: Monoid[B]): B = x match {
    case Element(x) => f(x)
    case Elements(xs) => xs.foldMap(x => f(x))(b)
  }
  val isSchemeLatticeSet = new IsSchemeLattice[LSet] {
    val name = s"SetLattice(${StringLattice[S].name}, ${BoolLattice[B].name}, ${IntLattice[I].name}, ${FloatLattice[F].name}, ${CharLattice[C].name}, ${SymbolLattice[Sym].name})"
    val counting = supportsCounting

    def isTrue(x: LSet): Boolean = foldMapLSet(x, isSchemeLattice.isTrue(_))(boolOrMonoid)
    def isFalse(x: LSet): Boolean = foldMapLSet(x, isSchemeLattice.isFalse(_))(boolOrMonoid)
    def isPrimitiveValue(x: LSet): Boolean = foldMapLSet(x, isSchemeLattice.isPrimitiveValue(_))(boolAndMonoid)
    def cardinality(x: LSet): Cardinality = foldMapLSet(x, isSchemeLattice.cardinality(_))
    def unaryOp(op: UnaryOperator)(x: LSet): MayFail[LSet] = foldMapLSet(x, x => isSchemeLattice.unaryOp(op)(x).map(x => wrap(x)))
    def binaryOp(op: BinaryOperator)(x: LSet, y: LSet): MayFail[LSet] = foldMapLSet(x, x => foldMapLSet(y, y => isSchemeLattice.binaryOp(op)(x, y).map(x => wrap(x))))
    def join(x: LSet, y: LSet): LSet = Monoid[LSet].append(x, y)
    /* if we need to define meet at some point, a different representation might be
     * more practical. Using a product of all the domains used is probably thea
     * best, i.e., Value(int: I, bool: B, ..., prims: Set[Primitive]) */
    def meet(x: LSet, y: LSet): LSet = ???
    def subsumes(x: LSet, y: LSet): Boolean = foldMapLSet(y, y =>
      /* For every element in y, there exists an element of x that subsumes it */
      foldMapLSet(x, x => isSchemeLattice.subsumes(x, y))(boolOrMonoid))(boolAndMonoid)
    def car[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isSchemeLattice.car(x))
    def cdr[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isSchemeLattice.cdr(x))

    def vectorRef[Addr : Address](vector: LSet, index: LSet): MayFail[Set[Addr]] = foldMapLSet(vector, vector => foldMapLSet(index, index =>
      isSchemeLattice.vectorRef(vector, index)))
    def vectorSet[Addr : Address](vector: LSet, index: LSet, addr: Addr): MayFail[(LSet, Set[Addr])] = foldMapLSet(vector, vector => foldMapLSet(index, index =>
      isSchemeLattice.vectorSet(vector, index, addr).map({ case (v, addrs) => (wrap(v), addrs) })))

    def getClosures[Exp : Expression, Addr : Address](x: LSet): Set[(Exp, Environment[Addr])] = foldMapLSet(x, x => isSchemeLattice.getClosures(x))
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: LSet): Set[Primitive[Addr, Abs]] = foldMapLSet(x, x => isSchemeLattice.getPrimitives(x))
    def getVectors[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isSchemeLattice.getVectors(x))

    def bottom: LSet = Element(isSchemeLattice.bottom)
    def inject(x: scala.Int): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: scala.Float): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: String): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: scala.Char): LSet = Element(isSchemeLattice.inject(x))
    def inject(x: Boolean): LSet = Element(isSchemeLattice.inject(x))
    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): LSet = Element(isSchemeLattice.inject(x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): LSet = Element(isSchemeLattice.inject(x))
    def injectSymbol(x: String): LSet = Element(isSchemeLattice.injectSymbol(x))
    def cons[Addr : Address](car: Addr, cdr: Addr): LSet = Element(isSchemeLattice.cons(car, cdr))
    def vector[Addr : Address](addr: Addr, size: LSet, init: Addr): MayFail[(LSet, LSet)] = foldMapLSet(size, size =>
      isSchemeLattice.vector(addr, size, init).map({ case (a, v) => (Element(a), Element(v)) }))
    def nil: LSet = Element(isSchemeLattice.nil)
  }
}

class ConcreteLattice(counting: Boolean) extends SchemeLattice {
  import ConcreteString._
  import ConcreteBoolean._
  import ConcreteInteger._
  import ConcreteFloat._
  import ConcreteChar._
  import ConcreteSymbol._

  val lattice = new MakeSchemeLattice[S, B, I, F, C, Sym](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsSchemeLattice[L] = lattice.isSchemeLatticeSet
}

class TypeSetLattice(counting: Boolean) extends SchemeLattice {
  import Type._
  import ConcreteBoolean._
  val lattice = new MakeSchemeLattice[T, B, T, T, T, T](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsSchemeLattice[L] = lattice.isSchemeLatticeSet
}

class BoundedIntLattice(bound: Int, counting: Boolean) extends SchemeLattice {
  import Type._
  import ConcreteBoolean._
  val bounded = new BoundedInteger(bound)
  import bounded._
  val lattice = new MakeSchemeLattice[T, B, I, T, T, T](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsSchemeLattice[L] = lattice.isSchemeLatticeSet
}

class ConstantPropagationLattice(counting: Boolean) extends SchemeLattice {
  import StringConstantPropagation._
  import ConcreteBoolean._
  import IntegerConstantPropagation._
  import FloatConstantPropagation._
  import CharConstantPropagation._
  import SymbolConstantPropagation._

  val lattice = new MakeSchemeLattice[S, B, I, F, C, Sym](counting)
  type L = lattice.LSet
  implicit val isSchemeLattice: IsSchemeLattice[L] = lattice.isSchemeLatticeSet
}
