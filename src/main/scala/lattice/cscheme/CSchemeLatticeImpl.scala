import scalaz.Scalaz._
import scalaz._
import SchemeOps._

class MakeCSchemeLattice(val lattice: SchemeLattice) extends CSchemeLattice {
  val lat = lattice.isSchemeLattice
  sealed trait Locked
  case object LockedBottom extends Locked
  case object LockedLocked extends Locked
  case object LockedUnlocked extends Locked
  case object LockedTop extends Locked

  implicit val locked: LatticeElement[Locked] = new LatticeElement[Locked] {
    def name = "Locked"
    def bottom = LockedBottom
    def top = LockedTop
    def join(x: Locked, y: => Locked) = (x, y) match {
      case (LockedBottom, _) => y
      case (_, LockedBottom) => x
      case (LockedTop, _) => x
      case (_, LockedTop) => y
      case (LockedLocked, LockedLocked) => x
      case (LockedUnlocked, LockedUnlocked) => x
      case _ => LockedTop
    }
    def subsumes(x: Locked, y: => Locked) = (x, y) match {
      case (LockedTop, _) => true
      case (LockedLocked, LockedLocked) => true
      case (LockedLocked, LockedBottom) => true
      case (LockedUnlocked, LockedUnlocked) => true
      case (LockedUnlocked, LockedBottom) => true
      case (LockedBottom, LockedBottom) => true
      case _ => false
    }
    def eql[B](x: Locked, y: Locked)(implicit bool: BoolLattice[B]): B = (x, y) match {
      case (LockedLocked, LockedLocked) => BoolLattice[B].inject(true)
      case (LockedUnlocked, LockedUnlocked) => BoolLattice[B].inject(true)
      case (LockedLocked, LockedUnlocked) => BoolLattice[B].inject(false)
      case (LockedUnlocked, LockedLocked) => BoolLattice[B].inject(false)
      case (LockedBottom, _) => BoolLattice[B].bottom
      case (_, LockedBottom) => BoolLattice[B].bottom
      case _ => BoolLattice[B].top
    }
    def order(x: Locked, y: Locked): Ordering = (x, y) match {
      case (LockedBottom, LockedBottom) => Ordering.EQ
      case (LockedBottom, _) => Ordering.LT
      case (_, LockedBottom) => Ordering.GT
      case (LockedLocked, LockedLocked) => Ordering.EQ
      case (LockedLocked, _) => Ordering.LT
      case (_, LockedLocked) => Ordering.GT
      case (LockedUnlocked, LockedUnlocked) => Ordering.EQ
      case (LockedUnlocked, _) => Ordering.LT
      case (_, LockedUnlocked) => Ordering.GT
      case (LockedTop, LockedTop) => Ordering.EQ
    }
    def cardinality(x: Locked): Cardinality = x match {
      case LockedBottom => CardinalityPrimitiveLikeNumber(0)
      case LockedLocked => CardinalityPrimitiveLikeNumber(1)
      case LockedUnlocked => CardinalityPrimitiveLikeNumber(1)
      case LockedTop => CardinalityPrimitiveLikeInf()
    }
  }

  type Tids = Set[Any] /* TODO: get rid of the any */
  type LockAddrs = Set[Any]

  val tids: LatticeElement[Tids] = LatticeElement.ofSet[Any]
  val lockaddrs: LatticeElement[LockAddrs] = LatticeElement.ofSet[Any]

  case class Value(seq: lattice.L = lat.bottom, t: Tids = tids.bottom, la: LockAddrs = lockaddrs.bottom, l: Locked = locked.bottom)
  type L = Value

  val isCSchemeLattice: IsCSchemeLattice[L] = new IsCSchemeLattice[L] {
    def bottom = Value()
    def join(x: L, y: L) = (x, y) match {
      case (Value(seq1, tids1, lockAddrs1, locked1), Value(seq2, tids2, lockAddrs2, locked2)) =>
        Value(lat.join(seq1, seq2), tids.join(tids1, tids2), lockaddrs.join(lockAddrs1, lockAddrs2), locked.join(locked1, locked2))
    }
    def subsumes(x: L, y: L) = (x, y) match {
      case (Value(seq1, tids1, lockAddrs1, locked1), Value(seq2, tids2, lockAddrs2, locked2)) =>
        lat.subsumes(seq1, seq2) && tids.subsumes(tids1, tids2) && lockaddrs.subsumes(lockAddrs1, lockAddrs2) && locked.subsumes(locked1, locked2)
    }
    val name = s"C(${lat.name})"
    val counting = lat.counting
    def isTrue(x: L) = lat.isTrue(x.seq) || x.t != tids.bottom || x.la != lockaddrs.bottom || x.l != locked.bottom
    def isFalse(x: L) = lat.isFalse(x.seq) && x.t == tids.bottom && x.la == lockaddrs.bottom && x.l == locked.bottom
    def unaryOp(op: UnaryOperator)(x: L): MayFail[L] = for { seq2 <- lat.unaryOp(op)(x.seq) } yield Value(seq = seq2)
    def binaryOp(op: BinaryOperator)(x: L, y: L): MayFail[L] = if (x == bottom || y == bottom) { bottom } else {
      op match {
        case Eq => for { seq2 <- lat.binaryOp(op)(x.seq, y.seq) } yield {
          val nonseq = if (x == bottom) { lat.bottom } else {
            if (!x.t.intersect(y.t).isEmpty || !x.la.intersect(y.la).isEmpty || locked.subsumes(x.l, y.l) || locked.subsumes(y.l, x.l)) {
              if ((x.l == locked.bottom && y.l == locked.bottom && x.t.size + x.la.size == 1 && y.t.size + y.la.size == 1)
                || (x.t.size + x.la.size == 0 && y.t.size + y.la.size == 0 && x.l == y.l && x.l != LockedTop)) {
                lat.inject(true) /* are certainly equal (only one element) */
              } else {
                lat.join(lat.inject(true), lat.inject(false)) /* can be equal */
              }
            } else { lat.inject(false) /* are certainly not equal */ }
          }
          Value(seq = lat.join(nonseq, seq2))
        }
        case _ => for { seq2 <- lat.binaryOp(op)(x.seq, y.seq) } yield Value(seq = seq2)
      }
    }
    def getClosures[Exp : Expression, Addr : Address](x: L) = lat.getClosures[Exp, Addr](x.seq)
    def getPrimitives[Addr : Address, Abs : JoinLattice](x: L) = lat.getPrimitives[Addr, Abs](x.seq)
    def isPrimitiveValue(x: L) = lat.isPrimitiveValue(x.seq) && x.t.isEmpty && x.la.isEmpty
    def cardinality(x: L) = ??? /* TODO */

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
    /* TODO: if (vector.t != tids.bottom || vector.la != lockaddrs.bottom || vector.l != locked.bottom) -> include error, same for index. Do the same for other operations as well? */
    def vectorRef[Addr : Address](vector: L, index: L) = lat.vectorRef(vector.seq, index.seq)
    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): MayFail[(L, Set[Addr])] = for {
      (seq, addrs) <- lat.vectorSet(vector.seq, index.seq, addr)
    } yield (Value(seq = seq), addrs) /* TODO: what happens to the other components? -> they result in an error, and are not returned */
    def getVectors[Addr : Address](x: L) = lat.getVectors[Addr](x.seq)
    def vector[Addr : Address](addr: Addr, size: L, init: Addr): MayFail[(L, L)] = for {
      (v1, v2) <- lat.vector[Addr](addr, size.seq, init)
    } yield (Value(seq = v1), Value(seq = v2))

    def getTids[TID : ThreadIdentifier](x: L): Set[TID] = x.t.collect({
      case tid: TID @unchecked => tid
    })
    def getLocks[Addr : Address](x: L): Set[Addr] = x.la.collect({
      case addr: Addr @unchecked => addr
    })
    def isLock(x: L) = Value(seq = lat.join(
      if (x.l != locked.bottom || x.la != lockaddrs.bottom) { lat.inject(true) } else { lat.bottom },
      if (x.seq != lat.bottom || x.t != tids.bottom) { lat.inject(false) } else { lat.bottom }))
    def isLocked(x: L) = x.l match {
      case LockedBottom => Value(seq = lat.bottom)
      case LockedLocked => Value(seq = lat.inject(true))
      case LockedUnlocked => Value(seq = lat.inject(false))
      case LockedTop => Value(seq = lat.join(lat.inject(true), lat.inject(false)))
    }
    def injectTid[TID : ThreadIdentifier](tid: TID) = Value(t = Set(tid))
    def lock[Addr : Address](addr: Addr) = Value(la = Set(addr))
    def lockedValue = Value(l = LockedLocked)
    def unlockedValue = Value(l = LockedUnlocked)
  }
}

class CSchemeConcreteLattice(counting: Boolean) extends MakeCSchemeLattice(new ConcreteLattice(counting))
class CSchemeTypeSetLattice(counting: Boolean) extends MakeCSchemeLattice(new TypeSetLattice(counting))
class CSchemeBoundedIntLattice(bound: Int, counting: Boolean) extends MakeCSchemeLattice(new BoundedIntLattice(bound, counting))
class CSchemeConstantPropagationLattice(counting: Boolean) extends MakeCSchemeLattice(new ConstantPropagationLattice(counting))
