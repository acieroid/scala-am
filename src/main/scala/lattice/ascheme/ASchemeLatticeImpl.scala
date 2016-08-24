import scalaz.Scalaz._
import scalaz._
import SchemeOps._

class MakeASchemeLattice(val lattice: SchemeLattice) extends ASchemeLattice {
  val lat = lattice.isSchemeLattice

  /* TODO: factor this somewhere else */
  implicit def ofSet[A]: IsLatticeElement[Set[A]] = new IsLatticeElement[Set[A]] {
    def name = "OfSet"
    def bottom: Set[A] = Set.empty
    def top: Set[A] = throw new Error("OfSet lattice has no top value")
    def join(x: Set[A], y: => Set[A]): Set[A] = x ++ y
    def subsumes(x: Set[A], y: => Set[A]): Boolean = y.subsetOf(x)
    def eql[B](x: Set[A], y: Set[A])(implicit bool: IsBoolean[B]): B =
      if (x.size == 1 && y.size == 1 && x == y) { bool.inject(true) }
      else if (x.intersect(y).isEmpty) { bool.inject(false) }
      else { bool.top }
    def order(x: Set[A], y: Set[A]): Ordering = throw new Error("Cannot define an order since A is not required to be ordered")
  }

  type Pids = Set[Any]
  val pids: IsLatticeElement[Pids] = ofSet[Any]
  type Behs = Set[Any]
  val behs: IsLatticeElement[Behs] = ofSet[Any]

  case class Value(seq: lattice.L = lat.bottom, p: Pids = pids.bottom, b: Behs = behs.bottom)
  type L = Value

  /* TODO: most of this can be generalized so that we only have to redefine stuff like join, subsumes, binaryOp, and the new functions */
  val isASchemeLattice: IsASchemeLattice[L] = new IsASchemeLattice[L] {
    def bottom = Value()
    def join(x: L, y: L) = (x, y) match {
      case (Value(seq1, p1, b1), Value(seq2, p2, b2)) =>
        Value(lat.join(seq1, seq2), pids.join(p1, p2), behs.join(b1, b2))
    }
    def subsumes(x: L, y: L) = (x, y) match {
      case (Value(seq1, p1, b1), Value(seq2, p2, b2)) =>
        lat.subsumes(seq1, seq2) && pids.subsumes(p1, p2) && behs.subsumes(b1, b2)
    }
    val name = s"A(${lat.name})"
    val counting = lat.counting
    def isTrue(x: L) = lat.isTrue(x.seq) || x.p != pids.bottom || x.b != behs.bottom
    def isFalse(x: L) = lat.isFalse(x.seq) && x.p == pids.bottom && x.b == behs.bottom
    def unaryOp(op: UnaryOperator)(x: L): MayFail[L] = for { seq2 <- lat.unaryOp(op)(x.seq) } yield Value(seq = seq2)
    def binaryOp(op: BinaryOperator)(x: L, y: L): MayFail[L] = op match {
      case Eq => for { seq2 <- lat.binaryOp(op)(x.seq, y.seq) } yield {
        val nonseq = if (x == bottom) { lat.bottom } else {
          if (!x.p.intersect(y.p).isEmpty || x.b.intersect(y.b).isEmpty) {
            if (x.p.size + x.b.size == 1 && y.p.size + y.b.size == 1) {
              lat.inject(true)
            } else {
              lat.join(lat.inject(true), lat.inject(false))
            }
          } else { lat.inject(false) }
        }
        Value(seq = lat.join(nonseq, seq2))
      }
      case _ => for { seq2 <- lat.binaryOp(op)(x.seq, y.seq) } yield Value(seq = seq2)
    }
    def getClosures[Exp : Expression, Addr : Address](x: L) = lat.getClosures[Exp, Addr](x.seq)
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L) = lat.getPrimitives[Addr, Abs](x.seq)

    def inject(x: Int) = Value(seq = lat.inject(x))
    def inject(x: Float) = Value(seq = lat.inject(x))
    def inject(x: String) = Value(seq = lat.inject(x))
    def inject(x: Boolean) = Value(seq = lat.inject(x))
    def inject(x: Char) = Value(seq = lat.inject(x))
    def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]) = Value(seq = lat.inject[Addr, Abs](x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = Value(seq = lat.inject[Exp, Addr](x))
    def injectSymbol(x: String) = Value(seq = lat.injectSymbol(x))
    def cons[Addr : Address](car: Addr, cdr: Addr) = Value(seq = lat.cons[Addr](car, cdr))
    def nil = Value(seq = lat.nil)
    def car[Addr : Address](x: L) = lat.car[Addr](x.seq)
    def cdr[Addr : Address](x: L) = lat.cdr[Addr](x.seq)
    def vectorRef[Addr : Address](vector: L, index: L) = lat.vectorRef(vector.seq, index.seq)
    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])] = for {
      (seq, addrs) <- lat.vectorSet(vector.seq, index.seq, addr)
    } yield (Value(seq = seq), addrs)
    def getVectors[Addr : Address](x: L) = lat.getVectors[Addr](x.seq)
    def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)] = for {
      (v1, v2) <- lat.vector[Addr](addr, size.seq, init)
    } yield (Value(seq = v1), Value(seq = v2))

    def injectBehavior[Exp : Expression, Addr : Address](e: Exp, env: Environment[Addr]) = Value(b = Set((e, env)))
    def getBehaviors[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])] = x.b.collect({
      case beh: (Exp, Environment[Addr]) @unchecked => beh
    })
    def injectPid[PID : ThreadIdentifier](pid: PID) = Value(p = Set(pid))
    def getPids[PID : ThreadIdentifier](x: L): Set[PID] = x.p.collect({
      case p: PID @unchecked => p
    })
    def isPrimitiveValue(x: L) = lat.isPrimitiveValue(x.seq) && x.p.isEmpty && x.b.isEmpty
  }
}

class ASchemeConcreteLattice(counting: Boolean) extends MakeASchemeLattice(new ConcreteLattice(counting))
class ASchemeTypeSetLattice(counting: Boolean) extends MakeASchemeLattice(new TypeSetLattice(counting))
class ASchemeBoundedIntLattice(bound: Int, counting: Boolean) extends MakeASchemeLattice(new BoundedIntLattice(bound, counting))
class ASchemeConstantPropagationLattice(counting: Boolean) extends MakeASchemeLattice(new ConstantPropagationLattice(counting))
