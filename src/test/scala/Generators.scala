import org.scalacheck.{Arbitrary, Gen}

import scalaam.core.Lattice
import scalaam.lattice._

trait LatticeGenerator[L] {
  def any: Gen[L]
  def le(l: L): Gen[L]
  implicit val anyArb: Arbitrary[L] = Arbitrary(any)
}

object Generators {
  val str: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr, Gen.numStr))
  val int: Gen[Int] = Gen.choose(-1000, 1000)
  val double: Gen[Double] = Gen.choose(-1000.0, 1000.0)
  val char: Gen[Char] = Gen.choose(0.toChar, 255.toChar)
  val sym: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr))
}

class BooleanGenerator[B : BoolLattice] extends LatticeGenerator[B] {
  /** ConcreteBool is a finite lattice with four elements */
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
object ConcreteBooleanGenerator extends BooleanGenerator[concrete.B]


case class SetGen[A](g: Gen[A]) {
  /*implicit val buildable = new org.scalacheck.util.Buildable[A, Set[A]] {
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
  val gen: Gen[Set[A]] = Gen.buildableOfN[Set[A], A](10, g)
  def genSubset(set: Set[A]): Gen[Set[A]] = {
    val list = set.toList
    for { n <- Gen.choose(0, set.size) } yield scala.util.Random.shuffle(list).take(n).toSet
  }
}

class ConcreteGenerator[T](g: Gen[T])(implicit lat: Lattice[concrete.L[T]]) extends LatticeGenerator[concrete.L[T]] {
  val isetgen = SetGen[T](g)
  val topgen: Gen[concrete.L[T]] = lat.top

  def any = Gen.oneOf(topgen, isetgen.gen.map(x => concrete.Values(x)))
  def le(l: concrete.L[T]) = l match {
    case concrete.Top => any
    case concrete.Values(content) => isetgen.genSubset(content).map(x => concrete.Values(x))
  }
}
object ConcreteStringGenerator extends ConcreteGenerator[String](Generators.str)(concrete.L.stringConcrete)
object ConcreteIntGenerator extends ConcreteGenerator[Int](Generators.int)
object ConcreteRealGenerator extends ConcreteGenerator[Double](Generators.double)
object ConcreteCharGenerator extends ConcreteGenerator[Char](Generators.char)
object ConcreteSymbolGenerator extends ConcreteGenerator[String](Generators.sym)(concrete.L.symConcrete)

/*


object TypeGenerator extends LatticeGenerator[Type.T] {
  /** Type lattice is a finite lattice with two elements */
  def any = Gen.oneOf(Type.Top, Type.Bottom)
  def le(l: Type.T) = l match {
    case Type.Top => any
    case Type.Bottom => Gen.const(Type.Bottom)
  }
}

abstract class ConstantPropagationGenerator[X : Order](gen: Gen[X])(implicit lat: LatticeElement[ConstantPropagation.L[X]]) extends LatticeGenerator[ConstantPropagation.L[X]] {
  def constgen: Gen[ConstantPropagation.L[X]] = for { x <- gen } yield ConstantPropagation.Constant(x)
  def botgen: Gen[ConstantPropagation.L[X]] = lat.bottom
  def topgen: Gen[ConstantPropagation.L[X]] = lat.top
  def any: Gen[ConstantPropagation.L[X]] = Gen.oneOf(constgen, botgen, topgen)
  def le(l: ConstantPropagation.L[X]) = if (l == lat.top) { any } else if (l == lat.bottom) { botgen } else { Gen.oneOf(l, lat.bottom) }
}

object StringConstantPropagationGenerator extends ConstantPropagationGenerator[String](Generators.str)(Order[String], ConstantPropagation.L.stringCP)
object IntegerConstantPropagationGenerator extends ConstantPropagationGenerator[Int](Generators.int)
object DoubleConstantPropagationGenerator extends ConstantPropagationGenerator[Double](Generators.double)
object CharConstantPropagationGenerator extends ConstantPropagationGenerator[Char](Generators.char)
object SymbolConstantPropagationGenerator extends ConstantPropagationGenerator[String](Generators.sym)(Order[String], ConstantPropagation.L.symCP)*/
