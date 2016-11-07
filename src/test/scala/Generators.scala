import org.scalacheck.{Arbitrary, Gen}
import scalaz.Scalaz._
import scalaz._

case class ISetGen[A](g: Gen[A])(implicit val order: Order[A]) {
  implicit val buildable = new org.scalacheck.util.Buildable[A, ISet[A]] {
    def builder = new scala.collection.mutable.Builder[A, ISet[A]] {
      var buff: ISet[A] = ISet.empty
      def clear() { buff = ISet.empty }
      def +=(x: A) = { buff = buff.union(ISet.singleton(x)); this }
      def result = buff
    }
  }
  implicit val toTraversable = (s: ISet[A]) => new Traversable[A] {
    def foreach[U](f: A => U): Unit = s.map({ x => f(x); () })
  }
  val gen: Gen[ISet[A]] = Gen.buildableOfN[ISet[A], A](10, g)
  def genSubset(set: ISet[A]): Gen[ISet[A]] = {
    val list = set.toList
    for { n <- Gen.choose(0, set.size) } yield ISet.fromList(scala.util.Random.shuffle(list).take(n))
  }
}

trait LatticeGenerator[L] {
  def any: Gen[L]
  def le(l: L): Gen[L]
  implicit val anyArb: Arbitrary[L] = Arbitrary(any)
}

object Generators {
  val str: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr, Gen.numStr))
  val int: Gen[Int] = Gen.choose(-1000, 1000)
  val float: Gen[Float] = Gen.choose(-1000.toFloat, 1000.toFloat)
  val char: Gen[Char] = Gen.choose(0.toChar, 255.toChar)
  val sym: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr))
}

object ConcreteBooleanGenerator extends LatticeGenerator[ConcreteBoolean.B] {
  /** ConcreteBool is a finite lattice with four elements */
  val bool = ConcreteBoolean.isBoolean
  val bot = bool.bottom
  val t = bool.inject(true)
  val f = bool.inject(false)
  val top = bool.top

  def any = Gen.oneOf(bot, t, f, top)
  def le(l: ConcreteBoolean.B) =
    if (l == bot) { Gen.const(bot) }
    else if (l == top) { Gen.oneOf(bot, t, f) }
    else { Gen.oneOf(l, bot) }
}

object TypeGenerator extends LatticeGenerator[Type.T] {
  /** Type lattice is a finite lattice with two elements */
  def any = Gen.oneOf(Type.Top, Type.Bottom)
  def le(l: Type.T) = l match {
    case Type.Top => any
    case Type.Bottom => Gen.const(Type.Bottom)
  }
}

object ConcreteStringGenerator extends LatticeGenerator[ConcreteString.S] {
  val isetgen = ISetGen[String](Generators.str)
  def any = isetgen.gen
  def le(l: ConcreteString.S) = isetgen.genSubset(l)
}

object ConcreteIntegerGenerator extends LatticeGenerator[ConcreteInteger.I] {
  val isetgen = ISetGen[Int](Generators.int)
  def any = isetgen.gen
  def le(l: ConcreteInteger.I) = isetgen.genSubset(l)
}

object ConcreteFloatGenerator extends LatticeGenerator[ConcreteFloat.F] {
  val isetgen = ISetGen[Float](Generators.float)
  def any = isetgen.gen
  def le(l: ConcreteFloat.F) = isetgen.genSubset(l)
}

object ConcreteCharGenerator extends LatticeGenerator[ConcreteChar.C] {
  implicit val charOrder: Order[Char] = Order.fromScalaOrdering[Char]
  val isetgen = ISetGen[Char](Generators.char)
  def any = isetgen.gen
  def le(l: ConcreteChar.C) = isetgen.genSubset(l)
}

object ConcreteSymbolGenerator extends LatticeGenerator[ConcreteSymbol.Sym] {
  val isetgen = ISetGen[String](Generators.sym)
  def any = isetgen.gen
  def le(l: ConcreteSymbol.Sym) = isetgen.genSubset(l)
}

// TODO: bounded ints, constant propagation

abstract class ConstantPropagationGenerator[X, L](gen: Gen[X])(const: X => L, bot: L, top: L) extends LatticeGenerator[L] {
  def constgen: Gen[L] = for { x <- gen } yield const(x)
  def botgen: Gen[L] = bot
  def topgen: Gen[L] = top
  def any: Gen[L] = Gen.oneOf(constgen, botgen, topgen)
  def le(l: L) = if (l == top) { any } else if (l == bot) { bot } else { Gen.oneOf(l, bot) }
}

object StringConstantPropagationGenerator extends ConstantPropagationGenerator[String, StringConstantPropagation.S](Generators.str)(StringConstantPropagation.Constant, StringConstantPropagation.Bottom, StringConstantPropagation.Top)
object IntegerConstantPropagationGenerator extends ConstantPropagationGenerator[Int, IntegerConstantPropagation.I](Generators.int)(IntegerConstantPropagation.Constant, IntegerConstantPropagation.Bottom, IntegerConstantPropagation.Top)
object FloatConstantPropagationGenerator extends ConstantPropagationGenerator[Float, FloatConstantPropagation.F](Generators.float)(FloatConstantPropagation.Constant, FloatConstantPropagation.Bottom, FloatConstantPropagation.Top)
object CharConstantPropagationGenerator extends ConstantPropagationGenerator[Char, CharConstantPropagation.C](Generators.char)(CharConstantPropagation.Constant, CharConstantPropagation.Bottom, CharConstantPropagation.Top)
object SymbolConstantPropagationGenerator extends ConstantPropagationGenerator[String, SymbolConstantPropagation.Sym](Generators.sym)(SymbolConstantPropagation.Constant, SymbolConstantPropagation.Bottom, SymbolConstantPropagation.Top)
