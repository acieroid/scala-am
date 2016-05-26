import org.scalatest._
import org.scalatest.prop._
import org.scalatest.prop.TableDrivenPropertyChecks._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scalaz.{Plus => _, _}
import scalaz.Scalaz._

import SchemeOps._

abstract class LatticePropSpec(val lattice: Lattice)
    extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with TableDrivenPropertyChecks {
  type Abs = lattice.L
  val abs = lattice.isAbstractValue
  property("lattice should preserve boolean value and correctly implement not") {
    forAll { (b: Boolean) => {
      val v = abs.inject(b)
      if (b) assert(abs.isTrue(v)) else assert(abs.isFalse(v))
      if (b) assert(abs.isFalse(abs.unaryOp(Not)(v))) else assert(abs.isTrue(abs.unaryOp(Not)(v)))
    }}
  }
  property("lattice should correctly implement boolean operations") {
    forAll { (b1: Boolean, b2: Boolean) => {
      val v1 = abs.inject(b1)
      val v2 = abs.inject(b2)
      if (b1 && b2) assert(abs.isTrue(abs.and(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
      if (b1 || b2) assert(abs.isTrue(abs.or(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
    }}
  }
  property("lattice should correctly implement numerical comparisons") {
    forAll { (n1: Int, n2: Int) => {
        val v1 = abs.inject(n1)
        val v2 = abs.inject(n2)
        if (n1 < n2) assert(abs.isTrue(abs.binaryOp(Lt)(v1, v2))) else assert(abs.isFalse(abs.binaryOp(Lt)(v1, v2)))
        if (n1 == n2) assert(abs.isTrue(abs.binaryOp(NumEq)(v1, v2))) else assert(abs.isFalse(abs.binaryOp(NumEq)(v1, v2)))
    }}
  }
  property("lattice should report errors on invalid operations") {
    val v1 = abs.inject(1)
    val v2 = abs.inject(true)
    assert(abs.isError(abs.binaryOp(Plus)(v1, v2))); assert(abs.isError(abs.binaryOp(Plus)(v2, v1)))
    assert(abs.isError(abs.binaryOp(Minus)(v1, v2))); assert(abs.isError(abs.binaryOp(Minus)(v2, v1)))
    assert(abs.isError(abs.binaryOp(Times)(v1, v2))); assert(abs.isError(abs.binaryOp(Times)(v2, v1)))
    assert(abs.isError(abs.binaryOp(Div)(v1, v2))); assert(abs.isError(abs.binaryOp(Div)(v2, v1)))
    assert(abs.isError(abs.binaryOp(Modulo)(v1, v2))); assert(abs.isError(abs.binaryOp(Modulo)(v2, v1)))
    assert(abs.isError(abs.binaryOp(Lt)(v1, v2))); assert(abs.isError(abs.binaryOp(Lt)(v2, v1)))
    assert(abs.isError(abs.binaryOp(NumEq)(v1, v2))); assert(abs.isError(abs.binaryOp(NumEq)(v2, v1)))
  }
  property("bottom should be subsumed by any other value") {
    val values = Table(
      ("v"),
      (abs.inject(1)),
      (abs.inject(2)),
      (abs.inject(true)),
      (abs.inject(false)),
      (abs.inject("foo")),
      (abs.injectSymbol("foo")))
    forAll (values) { (v: Abs) =>
      assert(abs.subsumes(v, abs.bottom))
      assert(!abs.subsumes(abs.bottom, v))
    }
  }
  property("bottom should subsume itself") {
    assert(abs.subsumes(abs.bottom, abs.bottom))
  }
}

abstract class JoinLatticePropSpec(lattice: Lattice)
    extends LatticePropSpec(lattice) {
  property("lattice should join values correctly or raise a CannotJoin error") {
    val bot = abs.bottom
    val t = abs.inject(true)
    val f = abs.inject(false)
    val tf = abs.join(t, f)
    val tf2 = abs.join(tf, bot)
    assert(abs.isTrue(tf)); assert(abs.isFalse(tf))
    assert(abs.isTrue(tf2)); assert(abs.isFalse(tf2));
    assert(abs.subsumes(tf, tf2)); assert(abs.subsumes(tf2, tf)); assert(tf.equals(tf2))
  }
  property("{#t, #f} joined with {#f} should give {#t, #f}") {
    /* bug detected on commit 1a31d78 */
    val tf = abs.join(abs.inject(true), abs.inject(false))
    val f = abs.inject(false)
    val tff = abs.join(f, tf)
    assert(abs.isTrue(tff)); assert(abs.isFalse(tff))
  }
  property("{#t, Str, Int} should subsume Str") {
    /* bug detected on commit 7546a519 */
    val str = abs.inject("foo")
    val t = abs.inject(true)
    val int = abs.inject(1000)
    val joined = abs.join(int, abs.join(t, str))
    assert(abs.subsumes(joined, str))
  }
  property("isError is correct") {
    val err = abs.error(abs.inject("Error"))
    val noterr = abs.inject(true)
    val joined = abs.join(err, noterr)
    /* isError is true for values that contain errors only */
    assert(abs.isError(err))
    /* For values that contain errors and non-errors, isError is false */
    assert(!abs.isError(joined))
  }
}

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

abstract class LatticeElementSpecification[L : LatticeElement](gen: LatticeGenerator[L])
    extends PropSpec with GeneratorDrivenPropertyChecks {
  val abs = implicitly[LatticeElement[L]]
  val bot = abs.bottom
  implicit val arbL: Arbitrary[L] = Arbitrary(gen.any)

  type B = ConcreteBoolean.B
  implicit val bool = ConcreteBoolean.isBoolean

  // ∀ a: ⊥ ⊑ a
  property("Bottom is a lower bound") {
    forAll { (a: L) =>
      assert(abs.subsumes(a, bot))
    }
  }
  // ∀ a: a ⊑ ⊤
  property("Top is an upper bound (when defined)") {
    scala.util.Try(abs.top).toOption match {
      case None => ()
      case Some(top) => forAll { (a: L) =>
        assert(abs.subsumes(top, a))
      }
    }
  }
  // ∀ a, b: a ⊔ b = b ⊔ a
  property("Join is commutative") {
    forAll { (a: L, b: L) =>
      assert(abs.join(a, b) == abs.join(b, a))
    }
  }
  // ∀ a, b, c: (a ⊔ b) ⊔ c = a ⊔ (b ⊔ c)
  property("Join is associative") {
    forAll { (a: L, b: L, c: L) =>
      assert(abs.join(abs.join(a, b), c) == abs.join(a, abs.join(b, c)))
    }
  }
  // ∀ a: a ⊔ a = a
  property("Join is idempotent") {
    forAll { (a: L) =>
      assert(abs.join(a, a) == a)
    }
  }
  // ∀ a, b: a ⊑ b ⇒ a ⊔ b = b
  property("Join and subsumes are compatible") {
    forAll { (b: L) =>
      forAll(gen.le(b)) { (a: L) =>
        assert(abs.subsumes(b, a) && abs.join(a, b) == b)
      }
    }
  }
  // ∀ a: a = bot ∨ isTrue(eql(a, a))
  property("Equal elements are always eql if not bottom") {
    forAll { (a: L) =>
      assert(a == bot || bool.isTrue(abs.eql[B](a, a)))
    }
  }
}

abstract class BooleanSpecification[B : IsBoolean](gen: LatticeGenerator[B])
    extends PropSpec with GeneratorDrivenPropertyChecks {
  val bool: IsBoolean[B] = implicitly[IsBoolean[B]]
  implicit val arbB: Arbitrary[B] = Arbitrary(gen.any)
  // ∀ a: (a ⇒ isTrue(inject(a))) && (¬a => isFalse(inject(a)))
  property("Inject preserves truthiness") {
    forAll { (a: Boolean) =>
      if (a) { assert(bool.isTrue(bool.inject(a))) } else { assert(bool.isFalse(bool.inject(false))) }
    }
  }
  // isTrue(⊤) ∧ isFalse(⊤)
  property("Top is true and false (when defined)") {
    scala.util.Try(bool.top).toOption match {
      case None => ()
      case Some(top) => assert(bool.isTrue(top) && bool.isFalse(top))
    }
  }
  // ¬isTrue(⊥) ∧ ¬isFalse(⊥)
  property("Bottom is neither true nor false") {
    assert(!bool.isTrue(bool.bottom) && !bool.isFalse(bool.bottom))
  }
  // ∀ a: isTrue(a) ⇒ isFalse(not(a)) ∧ isFalse(a) ⇒ isTrue(not(a))
  property("Not reserves truthiness") {
    forAll { (a: B) =>
      if (bool.isTrue(a)) { assert(bool.isFalse(bool.not(a))) }
      if (bool.isFalse(a)) { assert(bool.isTrue(bool.not(a))) }
    }
  }
}

abstract class StringSpecification[S : IsString](gen: LatticeGenerator[S])
    extends PropSpec with GeneratorDrivenPropertyChecks {
  val str = implicitly[IsString[S]]
  implicit val arbS: Arbitrary[S] = Arbitrary(gen.any)

  val boundedInt = new BoundedInteger(1000)
  type I = boundedInt.I
  implicit val int = boundedInt.isInteger

  // length(⊥) = ⊥
  property("bot length is bot") {
    assert(str.length[I](str.bottom) == int.bottom)
  }

  // ∀ a: append(⊥, a) = ⊥
  property("bot appended with something is bot") {
    forAll { (a: S) =>
      assert(str.append(str.bottom, a) == str.bottom)
    }
  }
  // ∀ a: append(a, ⊥) = ⊥
  property("bot appended to something is bot") {
    forAll { (a: S) =>
      assert(str.append(a, str.bottom) == str.bottom)
    }
  }
  // ∀ a, b: a ⊑ b ⇒ length(a) ⊑ length(b)
  property("length is monotone") {
    forAll { (b: S) =>
      forAll(gen.le(b)) { (a: S) =>
        assert(str.subsumes(b, a) && int.subsumes(str.length[I](b), str.length[I](a)))
      }
    }
  }
  // ∀ a, b, c: b ⊑ c ⇒ append(a, b) ⊑ append(a, c)
  // ∀ a, b, c: b ⊑ c ⇒ append(b, a) ⊑ append(c, a)
  property("append is monotone") {
    forAll { (a: S, c: S) =>
      forAll(gen.le(c)) { (b: S) =>
        assert(str.subsumes(c, b) && str.subsumes(str.append(a, c), str.append(a, b)) && str.subsumes(str.append(c, a), str.append(b, a)))
      }
    }
  }

  // ∀ a: inject(length(a)) ⊑ length(inject(a))
  property("length is a sound over-approximation") {
    forAll { (a: String) =>
      assert(int.subsumes(str.length[I](str.inject(a)), int.inject(a.size)))
    }
  }
  // ∀ a, b: inject(append(a, b)) ⊑ append(inject(a), inject(b))
  property("append is a sound over-approximation") {
    forAll { (a: String, b: String) =>
      assert(str.subsumes(str.append(str.inject(a), str.inject(b)), str.inject(a ++ b)))
    }
  }
}

case class IntegerSpecification[I : IsInteger](gen: LatticeGenerator[I])
    extends PropSpec with GeneratorDrivenPropertyChecks {
  val int = implicitly[IsInteger[I]]
  implicit val abrI: Arbitrary[I] = Arbitrary(gen.any)

  type B = ConcreteBoolean.B
  implicit val bool = ConcreteBoolean.isBoolean
  type S = Type.T
  implicit val str = Type.typeIsString

  // TODO: toFloat not tested (toFloat(⊥) = ⊥; ∀ a b: a ⊑ b ⇒ toFloat(a) ⊑ toFloat(b); ∀ a: inject(toFloat(a)) ⊑ toFloat(inject(a)))
  // TODO: test div behavior with 0 ?
  // TODO: random is not tested

  // ceiling(⊥) = ⊥
  property("ceiling of bottom is bottom") {
    assert(int.ceiling(int.bottom) == int.bottom)
  }
  // random(⊥) = ⊥
  property("random of bottom is bottom") {
    assert(int.random(int.bottom) == int.bottom)
  }
  // toString(⊥) = ⊥
  property("toString of bottom is bottom") {
    assert(int.toString[S](int.bottom) == str.bottom)
  }
  // ∀ a, op2: op2(⊥, a) = ⊥ ∧ op2(a, ⊥) = ⊥
  property("binary operation on bottom is bottom") {
    forAll { (a: I) =>
      assert(int.plus(int.bottom, a) == int.bottom &&
        int.plus(a, int.bottom) == int.bottom &&
        int.minus(int.bottom, a) == int.bottom &&
        int.minus(a, int.bottom) == int.bottom &&
        int.times(int.bottom, a) == int.bottom &&
        int.times(a, int.bottom) == int.bottom &&
        int.div(int.bottom, a) == int.bottom &&
        int.div(a, int.bottom) == int.bottom &&
        int.modulo(int.bottom, a) == int.bottom &&
        int.modulo(a, int.bottom) == int.bottom &&
        int.lt(int.bottom, a) == bool.bottom &&
        int.lt(a, int.bottom) == bool.bottom)
    }
  }

  // ∀ a, b: a ⊑ b ⇒ ceiling(a) ⊑ ceiling(b)
  property("ceiling is monotone") {
    forAll { (b: I) =>
      forAll(gen.le(b)) { (a: I) =>
        assert(int.subsumes(b, a) && int.subsumes(int.ceiling(b), int.ceiling(a)))
      }
    }
  }
  // ∀ a, b, c: b ⊑ c ⇒ plus(a, b) ⊑ plus(a, c) ∧ plus(b, a) ⊑ plus(c, b)
  property("plus is monotone") {
    forAll { (a: I, c: I) =>
      forAll(gen.le(c)) { (b: I) =>
        assert(int.subsumes(c, b) && int.subsumes(int.plus(a, c), int.plus(a, b)) && int.subsumes(int.plus(c, a), int.plus(b, a)))
      }
    }
  }
  // ∀ a, b, c: b ⊑ c ⇒ minus(a, b) ⊑ minus(a, c) ∧ minus(b, a) ⊑ minus(c, b)
  property("minus is monotone") {
    forAll { (a: I, c: I) =>
      forAll(gen.le(c)) { (b: I) =>
        assert(int.subsumes(c, b) && int.subsumes(int.minus(a, c), int.minus(a, b)) && int.subsumes(int.minus(c, a), int.minus(b, a)))
      }
    }
  }
  // ∀ a, b, c: b ⊑ c ⇒ times(a, b) ⊑ times(a, c) ∧ times(b, a) ⊑ times(c, b)
  property("times is monotone") {
    forAll { (a: I, c: I) =>
      forAll(gen.le(c)) { (b: I) =>
        assert(int.subsumes(c, b) && int.subsumes(int.times(a, c), int.times(a, b)) && int.subsumes(int.times(c, a), int.times(b, a)))
      }
    }
  }
  // ∀ a, b, c: b ⊑ c ⇒ div(a, b) ⊑ div(a, c) ∧ div(b, a) ⊑ div(c, b)
  // TODO: div is a special beast
  //property("div is monotone") {
  //  forAll { (a: I, c: I) =>
  //    forAll(gen.le(c)) { (b: I) =>
  //      assert(int.subsumes(c, b) && int.subsumes(int.div(a, c), int.div(a, b)) && int.subsumes(int.div(c, a), int.div(b, a)))
  //    }
  //  }
  //}
  // ∀ a, b, c: b ⊑ c ⇒ modulo(a, b) ⊑ modulo(a, c) ∧ modulo(b, a) ⊑ modulo(c, b)
  // TODO: modulo is as special as div
  //property("modulo is monotone") {
  //  forAll { (a: I, c: I) =>
  //    forAll(gen.le(c)) { (b: I) =>
  //      assert(int.subsumes(c, b) && int.subsumes(int.modulo(a, c), int.modulo(a, b)) && int.subsumes(int.modulo(c, a), int.modulo(b, a)))
  //    }
  //  }
  //}
  // ∀ a, b, c: b ⊑ c ⇒ lt(a, b) ⊑ lt(a, c) ∧ lt(b, a) ⊑ lt(c, b)
  property("lt is monotone") {
    forAll { (a: I, c: I) =>
      forAll(gen.le(c)) { (b: I) =>
        assert(int.subsumes(c, b) && bool.subsumes(int.lt[B](a, c), int.lt[B](a, b)) && bool.subsumes(int.lt[B](c, a), int.lt[B](b, a)))
      }
    }
  }

  // ∀ a: inject(ceiling(a)) ⊑ ceiling(inject(a))
  property("ceiling is a sound over-approximation") {
    forAll { (a: Int) =>
      assert(int.subsumes(int.ceiling(int.inject(a)), int.inject(a))) // ceiling of an int is the same int
    }
  }
  // ∀ a b: inject(a + b) ⊑ plus(inject(a), inject(b))
  property("plus is a sound over-approximation") {
    forAll { (a: Int, b: Int) =>
      assert(int.subsumes(int.plus(int.inject(a), int.inject(b)), int.inject(a + b)))
    }
  }
  // ∀ a b: inject(a - b) ⊑ minus(inject(a), inject(b))
  property("minus is a sound over-approximation") {
    forAll { (a: Int, b: Int) =>
      assert(int.subsumes(int.minus(int.inject(a), int.inject(b)), int.inject(a - b)))
    }
  }
  // ∀ a b: inject(a * b) ⊑ times(inject(a), inject(b))
  property("times is a sound over-approximation") {
    forAll { (a: Int, b: Int) =>
      assert(int.subsumes(int.times(int.inject(a), int.inject(b)), int.inject(a * b)))
    }
  }

  // ∀ a b: inject(a / b) ⊑ div(inject(a), inject(b))
  property("div is a sound over-approximation") {
    forAll { (a: Int, b: Int) =>
      if (b != 0) { // TODO: exclude b from the generator?
        assert(int.subsumes(int.div(int.inject(a), int.inject(b)), int.inject(a / b)))
      }
    }
  }
  // ∀ a b: inject(modulo(a, b)) ⊑ modulo(inject(a), inject(b))
  property("modulo is a sound over-approximation") {
    forAll { (a: Int, b: Int) =>
      if (b != 0) {
        assert(int.subsumes(int.modulo(int.inject(a), int.inject(b)), int.inject(SchemeOps.modulo(a, b))))
      }
    }
  }
  // ∀ a b: inject(a < b) ⊑ lt(inject(a), inject(b))
  property("lt is a sound over-approximation") {
    forAll { (a: Int, b: Int) =>
      assert(bool.subsumes(int.lt[B](int.inject(a), int.inject(b)), bool.inject(a < b)))
    }
  }
}

case class FloatSpecification[F : IsFloat](gen: LatticeGenerator[F])
    extends PropSpec with GeneratorDrivenPropertyChecks {
  // No specification (yet). TODO
}

case class CharSpecification[C : IsChar](gen: LatticeGenerator[C])
    extends PropSpec with GeneratorDrivenPropertyChecks {
  // No specification
}

case class SymbolSpecification[Sym : IsSymbol](gen: LatticeGenerator[Sym])
    extends PropSpec with GeneratorDrivenPropertyChecks {
  // No specification
}

class TypeLatticeTest extends LatticeElementSpecification[Type.T](TypeGenerator)(Type.typeIsLatticeElement)
class TypeBooleanTest extends BooleanSpecification[Type.T](TypeGenerator)(Type.typeIsBoolean)
class TypeStringTest extends StringSpecification[Type.T](TypeGenerator)(Type.typeIsString)
class TypeIntegerTest extends IntegerSpecification[Type.T](TypeGenerator)(Type.typeIsInteger)
class TypeFloatTest extends FloatSpecification[Type.T](TypeGenerator)(Type.typeIsFloat)
class TypeCharTest extends CharSpecification[Type.T](TypeGenerator)(Type.typeIsChar)
class TypeSymbolTest extends SymbolSpecification[Type.T](TypeGenerator)(Type.typeIsSymbol)

class ConcreteBooleanLatticeTest extends LatticeElementSpecification[ConcreteBoolean.B](ConcreteBooleanGenerator)(ConcreteBoolean.isBoolean)
class ConcreteStringLatticeTest extends LatticeElementSpecification[ConcreteString.S](ConcreteStringGenerator)(ConcreteString.isString)
class ConcreteIntegerLatticeTest extends LatticeElementSpecification[ConcreteInteger.I](ConcreteIntegerGenerator)(ConcreteInteger.isInteger)
class ConcreteFloatLatticeTest extends LatticeElementSpecification[ConcreteFloat.F](ConcreteFloatGenerator)(ConcreteFloat.isFloat)
class ConcreteCharLatticeTest extends LatticeElementSpecification[ConcreteChar.C](ConcreteCharGenerator)(ConcreteChar.isChar)
class ConcreteSymbolLatticeTest extends LatticeElementSpecification[ConcreteSymbol.Sym](ConcreteSymbolGenerator)(ConcreteSymbol.isSymbol)

class ConcreteBooleanTest extends BooleanSpecification[ConcreteBoolean.B](ConcreteBooleanGenerator)(ConcreteBoolean.isBoolean)
class ConcreteStringTest extends StringSpecification[ConcreteString.S](ConcreteStringGenerator)(ConcreteString.isString)
class ConcreteIntegerTest extends IntegerSpecification[ConcreteInteger.I](ConcreteIntegerGenerator)(ConcreteInteger.isInteger)
class ConcreteFloatTest extends FloatSpecification[ConcreteFloat.F](ConcreteFloatGenerator)(ConcreteFloat.isFloat)
class ConcreteCharTest extends CharSpecification[ConcreteChar.C](ConcreteCharGenerator)(ConcreteChar.isChar)
class ConcreteSymbolTest extends SymbolSpecification[ConcreteSymbol.Sym](ConcreteSymbolGenerator)(ConcreteSymbol.isSymbol)

class StringConstantPropagationLatticeTest extends LatticeElementSpecification[StringConstantPropagation.S](StringConstantPropagationGenerator)(StringConstantPropagation.isString)
class IntegerConstantPropagationLatticeTest extends LatticeElementSpecification[IntegerConstantPropagation.I](IntegerConstantPropagationGenerator)(IntegerConstantPropagation.isInteger)
class FloatConstantPropagationLatticeTest extends LatticeElementSpecification[FloatConstantPropagation.F](FloatConstantPropagationGenerator)(FloatConstantPropagation.isFloat)
class CharConstantPropagationLatticeTest extends LatticeElementSpecification[CharConstantPropagation.C](CharConstantPropagationGenerator)(CharConstantPropagation.isChar)
class SymbolConstantPropagationLatticeTest extends LatticeElementSpecification[SymbolConstantPropagation.Sym](SymbolConstantPropagationGenerator)(SymbolConstantPropagation.isSymbol)


class ConcreteCountingTest extends LatticePropSpec(new ConcreteLattice(true))
class ConcreteNoCountingTest extends JoinLatticePropSpec(new ConcreteLattice(false))
class TypeSetCountingTest extends JoinLatticePropSpec(new TypeSetLattice(true))
class TypeSetNoCountingTest extends JoinLatticePropSpec(new TypeSetLattice(false))
class BoundedIntCountingTest extends JoinLatticePropSpec(new BoundedIntLattice(100, true))
class BoundedIntNoCountingTest extends JoinLatticePropSpec(new BoundedIntLattice(100, false))
class ConstantPropagationCountingTest extends JoinLatticePropSpec(new ConstantPropagationLattice(true))
class ConstantPropagationNoCountingTest extends JoinLatticePropSpec(new ConstantPropagationLattice(false))
