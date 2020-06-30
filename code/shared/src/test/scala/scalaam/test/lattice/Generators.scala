package scalaam.test.lattice

import org.scalacheck._
import scalaam.core.Lattice
import scalaam.lattice._

trait LatticeGenerator[L] {
  def any: Gen[L]
  def le(l: L): Gen[L]
  implicit val anyArb: Arbitrary[L] = Arbitrary(any)
  implicit val shrink: Shrink[L] = Shrink { v => Stream.empty }
}

object Generators {
  val str: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr, Gen.numStr))
  val int: Gen[Int] = Gen.choose(-1000, 1000)
  val double: Gen[Double] = Gen.choose(-1000.0, 1000.0)
  val char: Gen[Char] = Gen.choose(0.toChar, 255.toChar)
  val sym: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr))
}

class BooleanGenerator[B : BoolLattice] extends LatticeGenerator[B] {
  /** ConcreteBool is a finite scalaam.lattice with four elements */
  val bot = BoolLattice[B].bottom
  val t = BoolLattice[B].inject(true)
  val f = BoolLattice[B].inject(false)
  val top = BoolLattice[B].top

  def any = Gen.oneOf(bot, t, f, top)
  def le(l: B) =
    if (l == bot) { Gen.const(bot) }
    else if (l == top) { Gen.oneOf(bot, t, f) }
    else { Gen.oneOf(l, bot) }
}
object ConcreteBooleanGenerator extends BooleanGenerator[Concrete.B]


case class SetGen[A](g: Gen[A]) {
  /*implicit val buildable = new org.scalacheck.scalaam.util.Buildable[A, Set[A]] {
    def builder = new scala.collection.mutable.Builder[A, Set[A]] {
      var buff: Set[A] = Set.empty
      def clear() = { buff = Set.empty }
      def +=(x: A) = { buff = buff.union(Set(x)); this }
      def result = buff
    }
  }
  implicit val toTraversable = (s: Set[A]) => new Traversable[A] {
    def foreach[U](f: A => U): Unit = s.foreach({ x => f(x) })
  }*/
  val gen: Gen[Set[A]] = for {
    n <- Gen.choose(0,10)
    s <- Gen.buildableOfN[Set[A], A](n, g)
  } yield s
  def genSubset(set: Set[A]): Gen[Set[A]] = {
    val list = set.toList
    for { n <- Gen.choose(0, set.size) } yield scala.util.Random.shuffle(list).take(n).toSet
  }
}

class ConcreteGenerator[T](g: Gen[T])(implicit lat: Lattice[Concrete.L[T]]) extends LatticeGenerator[Concrete.L[T]] {
  val isetgen = SetGen[T](g)
  val topgen: Gen[Concrete.L[T]] = lat.top

  def any = Gen.oneOf(topgen, isetgen.gen.map(x => Concrete.Values(x)))
  def le(l: Concrete.L[T]) = l match {
    case Concrete.Top => any
    case Concrete.Values(content) => isetgen.genSubset(content).map(x => Concrete.Values(x))
  }
  override val shrink = Shrink { 
    case Concrete.Top => Stream.empty
    case Concrete.Values(vs) =>
      Shrink.shrinkContainer[Set,T].shrink(vs).map(Concrete.Values(_))
  }
}

object ConcreteStringGenerator extends ConcreteGenerator[String](Generators.str)(Concrete.L.stringConcrete)
object ConcreteIntGenerator extends ConcreteGenerator[Int](Generators.int)
object ConcreteRealGenerator extends ConcreteGenerator[Double](Generators.double)
object ConcreteCharGenerator extends ConcreteGenerator[Char](Generators.char)
object ConcreteSymbolGenerator extends ConcreteGenerator[String](Generators.sym)(Concrete.L.symConcrete)

object TypeGenerator extends LatticeGenerator[Type.T] {
  /** Type scalaam.lattice is a finite scalaam.lattice with two elements */
  def any = Gen.oneOf(Type.Top, Type.Bottom)
  def le(l: Type.T) = l match {
    case Type.Top => any
    case Type.Bottom => Gen.const(Type.Bottom)
  }
}

abstract class ConstantPropagationGenerator[X](gen: Gen[X])(implicit lat: Lattice[ConstantPropagation.L[X]]) extends LatticeGenerator[ConstantPropagation.L[X]] {
  def constgen: Gen[ConstantPropagation.L[X]] = for { x <- gen } yield ConstantPropagation.Constant(x)
  def botgen: Gen[ConstantPropagation.L[X]] = lat.bottom
  def topgen: Gen[ConstantPropagation.L[X]] = lat.top
  def any: Gen[ConstantPropagation.L[X]] = Gen.oneOf(constgen, botgen, topgen)
  def le(l: ConstantPropagation.L[X]) = if (l == lat.top) { any } else if (l == lat.bottom) { botgen } else { Gen.oneOf(l, lat.bottom) }
}


object ConstantPropagationStringGenerator extends ConstantPropagationGenerator[String](Generators.str)(ConstantPropagation.L.stringCP)
object ConstantPropagationIntGenerator extends ConstantPropagationGenerator[Int](Generators.int)
object ConstantPropagationRealGenerator extends ConstantPropagationGenerator[Double](Generators.double)
object ConstantPropagationCharGenerator extends ConstantPropagationGenerator[Char](Generators.char)
object ConstantPropagationSymbolGenerator extends ConstantPropagationGenerator[String](Generators.sym)(ConstantPropagation.L.symCP)
