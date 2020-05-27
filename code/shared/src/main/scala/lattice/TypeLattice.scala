package scalaam.lattice

import scalaam.core._

object Type {
  sealed trait T
  case object Top    extends T
  case object Bottom extends T

  abstract class BaseInstance(typeName: String) extends Lattice[T] {
    def show(x: T): String = x match {
      case Top    => typeName
      case Bottom => s"$typeName.⊥"
    }
    val bottom: T = Bottom
    val top: T    = Top
    def join(x: T, y: => T): T = x match {
      case Top    => Top
      case Bottom => y
    }
    def meet(x: T, y: => T): T = x match {
      case Bottom => Bottom
      case Top    => y
    }
    def subsumes(x: T, y: => T): Boolean = x match {
      case Top => true
      case Bottom =>
        y match {
          case Top    => false
          case Bottom => true
        }
    }
    def eql[B2: BoolLattice](n1: T, n2: T): B2 = (n1, n2) match {
      case (Top, Top) => BoolLattice[B2].top
      case _          => BoolLattice[B2].bottom
    }
    def split(v: T) = v match {
      case Bottom => Set.empty
      case Top    => Set(Top)
    }
  }

  type S   = T
  type B   = T
  type I   = T
  type R   = T
  type C   = T
  type Sym = T

  object T {
    import scalaam.lattice._

    implicit val typeIsString: StringLattice[S] = new BaseInstance("Str") with StringLattice[S] {
      def inject(x: String): T = Top
      def length[I2: IntLattice](s: T) = s match {
        case Top    => IntLattice[I2].top
        case Bottom => IntLattice[I2].bottom
      }
      def append(s1: T, s2: T) = (s1, s2) match {
        case (Bottom, _) | (_, Bottom) => Bottom
        case (Top, _) | (Top, _)       => Top
      }
      def ref[I2: IntLattice, C2: CharLattice](s: S, i: I2): C2 = s match {
        case Bottom                            => CharLattice[C2].bottom
        case Top if i == IntLattice[I2].bottom => CharLattice[C2].bottom
        case Top                               => CharLattice[C2].top
      }
      def lt[B2: BoolLattice](s1: T, s2: T) = (s1, s2) match {
        case (Bottom, _) | (_, Bottom) => BoolLattice[B2].bottom
        case (Top, _) | (Top, _)       => BoolLattice[B2].top
      }
      def toSymbol[Sym2: SymbolLattice](s: S) = s match {
        case Bottom => SymbolLattice[Sym2].bottom
        case Top    => SymbolLattice[Sym2].top
      }
      def toNumber[I2: IntLattice](s: S) = s match {
        case Bottom => IntLattice[I2].bottom
        case Top    => IntLattice[I2].top
      }
    }
    implicit val typeIsBoolean: BoolLattice[B] = new BaseInstance("Bool") with BoolLattice[B] {
      def inject(x: Boolean): T = Top
      def isTrue(b: T)          = b == Top
      def isFalse(b: T)         = b == Top
      def not(b: T)             = b
    }
    implicit val typeIsInteger: IntLattice[I] = new BaseInstance("Int") with IntLattice[I] {
      def inject(x: Int): T = Top
      def toReal[R2: RealLattice](n: T): R2 = n match {
        case Top    => RealLattice[R2].top
        case Bottom => RealLattice[R2].bottom
      }
      def random(n: T): T        = n
      def plus(n1: T, n2: T): T  = meet(n1, n2)
      def minus(n1: T, n2: T): T = meet(n1, n2)
      def times(n1: T, n2: T): T = meet(n1, n2)
      def div[R2: RealLattice](n1: T, n2: T): R2 = (n1, n2) match {
        case (Top, Top) => RealLattice[R2].top
        case _          => RealLattice[R2].bottom
      }
      def expt(n1: T, n2: T): T = meet(n1, n2)
      def quotient(n1: T, n2: T): T  = meet(n1, n2)
      def modulo(n1: T, n2: T): T    = meet(n1, n2)
      def remainder(n1: T, n2: T): T = meet(n1, n2)
      def lt[B2: BoolLattice](n1: T, n2: T): B2 = (n1, n2) match {
        case (Top, Top) => BoolLattice[B2].top
        case _          => BoolLattice[B2].bottom
      }
      def valuesBetween(n1: T, n2: T): Set[T] = Set(Top)
      def toString[S2: StringLattice](n: T): S2 = n match {
        case Top    => StringLattice[S2].top
        case Bottom => StringLattice[S2].bottom
      }
    }
    implicit val typeIsReal: RealLattice[R] = new BaseInstance("Real") with RealLattice[R] {
      def inject(x: Double): T = Top
      def toInt[I2: IntLattice](n: T): I2 = n match {
        case Top    => IntLattice[I2].top
        case Bottom => IntLattice[I2].bottom
      }
      def ceiling(n: T): T       = n
      def floor(n: T): T         = n
      def round(n: T): T         = n
      def log(n: T): T           = n
      def random(n: T): T        = n
      def sin(n: T): T           = n
      def asin(n: T): T          = n
      def cos(n: T): T           = n
      def acos(n: T): T          = n
      def tan(n: T): T           = n
      def atan(n: T): T          = n
      def sqrt(n: T): T          = n
      def plus(n1: T, n2: T): T  = meet(n1, n2)
      def minus(n1: T, n2: T): T = meet(n1, n2)
      def times(n1: T, n2: T): T = meet(n1, n2)
      def div(n1: T, n2: T): T   = meet(n1, n2)
      def expt(n1: T, n2: T): T  = meet(n1, n2)
      def lt[B2: BoolLattice](n1: T, n2: T): B2 = (n1, n2) match {
        case (Top, Top) => BoolLattice[B2].top
        case _          => BoolLattice[B2].bottom
      }
      def toString[S2: StringLattice](n: T): S2 = n match {
        case Top    => StringLattice[S2].top
        case Bottom => StringLattice[S2].bottom
      }
    }
    implicit val typeIsChar: CharLattice[C] = new BaseInstance("Char") with CharLattice[C] {
      def inject(c: Char): T = Top
      def toInt[I2: IntLattice](c: C) = c match {
        case Bottom => IntLattice[I2].bottom
        case Top    => IntLattice[I2].top
      }
    }
    implicit val typeIsSymbol: SymbolLattice[Sym] = new BaseInstance("Sym")
    with SymbolLattice[Sym] {
      def inject(sym: String): T                = Top
      def toString[S2: StringLattice](s: T): S2 = StringLattice[S2].top
    }
  }
}
