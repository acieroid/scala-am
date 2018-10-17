import org.scalacheck.{Prop, Properties}
import Prop.forAll

import scalaam.core.Lattice
import scalaam.lattice._

abstract class LatticeSpecification extends Properties("") {
  /* Taken from Scalaz's SpecLite class */
  override val name = this.getClass.getName.stripSuffix("$")
  def checkAll(props: Properties): Unit = {
    for ((name, prop) <- props.properties) {
      property(name) = prop
    }
  }
  def newProperties(name: String)(f: Properties => Properties): Properties = {
    val p = new Properties(name)
    f(p)
  }

  def conditional(p: Boolean, q: => Boolean): Boolean = !p || q
}

abstract class LatticeTest[L : Lattice](gen: LatticeGenerator[L]) extends LatticeSpecification {
  val laws: Properties = {
    newProperties("Lattice") { p =>
      implicit val arb = gen.anyArb
      val lat: Lattice[L] = implicitly
      import lat._

      /** The subsumption operation is reflexive */
      p.property("∀ a: a ⊑ a") = forAll((a: L) => subsumes(a, a))
      /** Bottom is the lower bound of all elements in the lattice */
      p.property("∀ a: ⊥ ⊑ a") = forAll((a: L) => subsumes(a, bottom))
      /** The join operation is commutative */
      p.property("∀ a, b: a ⊔ b = b ⊔ a") = forAll((a: L, b: L) => join(a, b) == join(b, a))
      /** The join operation is associative */
      p.property("∀ a, b, c: (a ⊔ b) ⊔ c = a ⊔ (b ⊔ c)") = forAll((a: L, b: L, c: L) => join(join(a, b), c) == join(a, join(b, c)))
      /** The join operation is idempotent */
      p.property("∀ a: a ⊔ a = a") = forAll((a: L) => join(a, a) == a)
      /** The join operation is compatible with subsumption */
      p.property("∀ a, b: a ⊑ b ⇒ a ⊔ b = b") = forAll { (b: L) =>
        forAll(gen.le(b)) { (a: L) =>
          conditional(subsumes(b, a), join(a, b) == b)
        }
      }
      p
    }
  }
  checkAll(laws)
}

abstract class BoolLatticeTest[B : BoolLattice](gen: LatticeGenerator[B]) extends LatticeTest[B](gen) {
  val boolLaws: Properties = {
    newProperties("Bool") { p =>
      newProperties("BoolLattice") { p =>
        implicit val arb = gen.anyArb
        val lat: BoolLattice[B] = implicitly
        import lat._

        /** Inject preserves truthiness */
        p.property("isTrue(inject(true)) ∧ isFalse(inject(false))") = isTrue(inject(true)) && isFalse(inject(false))
        /** Top is both true and false */
        p.property("isTrue(⊤) ∧ isFalse(⊤)") = isTrue(top) && isFalse(top)
        /** Bottom is neither true nor false */
        p.property("¬isTrue(⊥) ∧ ¬isFalse(⊥)") = !isTrue(bottom) && !isFalse(bottom)
        /** Not reverses truthiness */
        p.property("∀ a: isTrue(a) ⇒ isFalse(not(a)) ∧ isFalse(a) ⇒ isTrue(not(a))") = forAll { (a: B) =>
          conditional(isTrue(a), isFalse(not(a))) && conditional(isFalse(a), isTrue(not(a)))
        }
        /** Not is involutive */
        p.property("∀ a: not(not(a)) == a") = forAll((a: B) => not(not(a)) == a)
        p
      }
    }
  }
  checkAll(boolLaws)
}

abstract class StringLatticeTest[S : StringLattice, I : IntLattice](gen: LatticeGenerator[S]) extends LatticeTest[S](gen) {
  val stringLaws: Properties = {
    newProperties("String") { p =>
      implicit val arb = gen.anyArb
      val lat: StringLattice[S] = implicitly
      import lat._

      /** Length preserves bottom */
      p.property("length(⊥) = ⊥") = length[I](bottom) == IntLattice[I].bottom
      /** Length is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ length(a) ⊑ length(b)") = forAll { (b: S) =>
        forAll(gen.le(b)) { (a: S) =>
          conditional(subsumes(b, a), IntLattice[I].subsumes(length[I](b), length[I](a)))
        }
      }
      /** Length is sound */
      p.property("∀ a: inject(a.size) ⊑ length(inject(a))") = forAll((a: String) => IntLattice[I].subsumes(length[I](inject(a)), IntLattice[I].inject(a.size)))

      /** Append preservesf bottom */
      p.property("∀ a: append(a, ⊥) = ⊥ = append(⊥, a)") = forAll((a: S) => append(bottom, a) == bottom && append(a, bottom) == bottom)
      /** Append is monotone */
      p.property("∀ a, b, c: a ⊑ b ⇒ append(a, c) ⊑ append(b, c) ∧ append(c, a) ⊑ append(c, b)") = forAll { (b: S, c: S) =>
        forAll(gen.le(b)) { (a: S) =>
          conditional(subsumes(c, b),
                  subsumes(append(a, c), append(a, b)) && subsumes(append(c, a), append(b, a)))
        }
      }
      /** Append is sound */
      p.property("∀ a, b: append(inject(a), inject(b)) ⊑ inject(a ++ b)") = forAll((a: String, b: String) => subsumes(append(inject(a), inject(b)), inject(a ++ b)))
      /** Append is associative */
      p.property("∀ a, b, c: append(append(a, b), c) == append(a, append(b, c))") = forAll((a: S, b: S, c: S) => append(append(a, b), c) == append(a, append(b, c)))

      p
    }
  }
  checkAll(stringLaws)
}

abstract class IntLatticeTest[I : IntLattice, B : BoolLattice, R : RealLattice, S : StringLattice](gen: LatticeGenerator[I]) extends LatticeTest[I](gen) {
  val intLaws: Properties = {
    newProperties("Int") { p =>
      implicit val arb = gen.anyArb
      val lat: IntLattice[I] = implicitly
      import lat._

      /** Conversion to real preserves bottom */
      p.property("toReal(⊥) = ⊥") = toReal[R](bottom) == RealLattice[R].bottom
      /** Conversion to real is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ toReal(a) ⊑ toR(b)") = forAll { (b: I) =>
        forAll(gen.le(b)) { (a: I) =>
          conditional(subsumes(b, a), RealLattice[R].subsumes(toReal[R](b), toReal[R](a)))
        }
      }
      /** Conversion to real is sound */
      p.property("∀ a: inject(a.toDouble) ⊑ toReal(inject(a))") =
        forAll((a: Int) => RealLattice[R].subsumes(toReal[R](inject(a)), RealLattice[R].inject(a.toDouble)))

      /** Random preserves bottom */
      p.property("random(⊥) = ⊥") = random(bottom) == bottom
      /* Random should neither be monotone nor sound (at least in concrete) */

      /** Addition preserves bottom */
      p.property("plus(a, ⊥) = ⊥ = plus(⊥, a)") =
        forAll((a: I) => plus(a, bottom) == bottom && plus(bottom, a) == bottom)
      /** Addition is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ plus(a, b) ⊑ plus(a, c)") = forAll { (a: I, c: I) =>
        forAll(gen.le(c)) { (b: I) =>
          conditional(subsumes(c, b), subsumes(plus(a, c), plus(a, b)))
        }
      }
      /** Addition is sound */
      p.property("∀ a, b: inject(a + b) ⊑ plus(inject(a), inject(b))") =
        forAll((a: Int, b: Int) => subsumes(plus(inject(a), inject(b)), inject(a + b)))
      /** Addition is associative */
      p.property("∀ a, b, c: plus(a, plus(b, c)) == plus(plus(a, b), c)") =
        forAll((a: I, b: I, c: I) => plus(a, plus(b, c)) == plus(plus(a, b), c))
      /** Addition is commutative */
      p.property("∀ a, b: plus(a, b) == plus(b, a)") =
        forAll((a: I, b: I) => plus(a, b) == plus(b, a))

      /** Subtraction preserves bottom */
      p.property("minus(a, ⊥) = ⊥ = minus(⊥, a)") =
        forAll((a: I) => minus(a, bottom) == bottom && minus(bottom, a) == bottom)
      /** Subtraction is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ minus(a, b) ⊑ minus(a, c)") = forAll { (a: I, c: I) =>
        forAll(gen.le(c)) { (b: I) =>
          conditional(subsumes(c, b), subsumes(minus(a, c), minus(a, b)))
        }
      }
      /** Subtraction is sound */
      p.property("∀ a, b: inject(a - b) ⊑ minus(inject(a), inject(b))") =
        forAll((a: Int, b: Int) => subsumes(minus(inject(a), inject(b)), inject(a - b)))
      /** Subtraction is anticommutative */
      p.property("∀ a, b: minus(a, b) == minus(inject(0), minus(b, a))") =
        forAll((a: I, b: I) => minus(a, b) == minus(inject(0), minus(b, a)))

      /** Addition preserves bottom */
      p.property("times(a, ⊥) = ⊥ = times(⊥, a)") =
        forAll((a: I) => times(a, bottom) == bottom && times(bottom, a) == bottom)
      /** Addition is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ times(a, b) ⊑ times(a, c)") = forAll { (a: I, c: I) =>
        forAll(gen.le(c)) { (b: I) =>
          conditional(subsumes(c, b), subsumes(times(a, c), times(a, b)))
        }
      }
      /** Addition is sound */
      p.property("∀ a, b: inject(a + b) ⊑ times(inject(a), inject(b))") =
        forAll((a: Int, b: Int) => subsumes(times(inject(a), inject(b)), inject(a * b)))
      /** Addition is associative */
      p.property("∀ a, b, c: times(a, times(b, c)) == times(times(a, b), c)") =
        forAll((a: I, b: I, c: I) => times(a, times(b, c)) == times(times(a, b), c))
      /** Addition is commutative */
      p.property("∀ a, b: times(a, b) == times(b, a)") =
        forAll((a: I, b: I) => times(a, b) == times(b, a))

      /** Quotient preserves bottom */
      p.property("div(a, ⊥) = ⊥ = div(⊥, a)") =
        forAll((a: I) =>
          quotient(a, bottom) == bottom && conditional(!subsumes(a, inject(0)),
            quotient(bottom, a) == bottom))
      /** Quotient is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ div(a, b) ⊑ div(a, c)") = forAll { (a: I, c: I) =>
        forAll(gen.le(c)) { (b: I) =>
          conditional(subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(c, inject(0)),
            subsumes(quotient(a, c), quotient(a, b)))
        }
      }
      /** Quotient is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ div(inject(a), inject(b))") =
        forAll((a: Int, b: Int) => conditional(b != 0, subsumes(quotient(inject(a), inject(b)), inject(a / b))))

      /** Modulo preserves bottom */
      p.property("modulo(a, ⊥) = ⊥ = modulo(⊥, a)") =
        forAll((a: I) => modulo(a, bottom) == bottom && conditional(!subsumes(a, inject(0)),
          modulo(bottom, a) == bottom))
      /** Modulo is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ modulo(a, b) ⊑ modulo(a, c)") = forAll { (a: I, c: I) =>
        forAll(gen.le(c)) { (b: I) =>
          conditional(subsumes(c, b) && !subsumes(c, inject(0)) && !subsumes(b, inject(0)),
            subsumes(modulo(a, c), modulo(a, b)))
        }
      }
      /** Modulo is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ modulo(inject(a), inject(b))") =
        forAll((a: Int, b: Int) => conditional(b != 0, subsumes(modulo(inject(a), inject(b)), inject(MathOps.modulo(a, b)))))

      /** Remainder preserves bottom */
      p.property("rem(a, ⊥) = ⊥ = rem(⊥, a) (if a ≠ 0)") = forAll((a: I) =>
        remainder(a, bottom) == bottom && conditional(!subsumes(a, inject(0)),
          remainder(bottom, a) == bottom))
      /** Remainder is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ rem(a, b) ⊑ rem(a, c)") = forAll { (a: I, c: I) =>
        forAll(gen.le(c)) { (b: I) =>
          conditional(subsumes(c, b) && !subsumes(c, inject(0)) && !subsumes(b, inject(0)),
            subsumes(remainder(a, c), remainder(a, b)))
        }
      }
      /** Remainder is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ rem(inject(a), inject(b))") =
        forAll((a: Int, b: Int) => conditional(b != 0, subsumes(remainder(inject(a), inject(b)), inject(MathOps.remainder(a, b)))))

      /** Less-than operation preserves bottom */
      p.property("lt(a, ⊥) = ⊥ = lt(⊥, a)") =
        forAll((a: I) => lt[B](a, bottom) == BoolLattice[B].bottom && lt[B](bottom, a) == BoolLattice[B].bottom)
      /** Less-than operation is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ lt(a, b) ⊑ lt(a, c)") = forAll { (a: I, c: I) =>
        forAll(gen.le(c)) { (b: I) =>
          conditional(subsumes(b, c), BoolLattice[B].subsumes(lt[B](a, c), lt[B](a, b)))
        }
      }
      /** Less-than operation is sound */
      p.property("∀ a, b ≠ 0: inject(a < b) ⊑ lt(inject(a), inject(b))") =
        forAll((a: Int, b: Int) => BoolLattice[B].subsumes(lt[B](inject(a), inject(b)), BoolLattice[B].inject(a < b)))

      /** To-string operation preserves bottom */
      p.property("toString(⊥) = ⊥") = lat.toString[S](bottom) == StringLattice[S].bottom
      /** To-string operation is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ toString(a) ⊑ toString(b)") = forAll { (b: I) =>
        forAll(gen.le(b)) { (a: I) =>
          conditional(subsumes(b, a),
            StringLattice[S].subsumes(lat.toString[S](b), lat.toString[S](a)))
        }
      }
      /** To-string operation is sound */
      p.property("∀ a, b: inject(toString(a)) ⊑ toString(inject(a))") =
        forAll((a: Int) => StringLattice[S].subsumes(lat.toString[S](inject(a)), StringLattice[S].inject(a.toString)))

      p
    }
  }
  checkAll(intLaws)
}

abstract class RealLatticeTest[R : RealLattice, B : BoolLattice, I : IntLattice, S : StringLattice](gen: LatticeGenerator[R]) extends LatticeTest[R](gen) {
  val realLaws: Properties = {
    newProperties("Real") { p =>
      implicit val arb = gen.anyArb
      val lat: RealLattice[R] = implicitly
      import lat._

      /** Integer conversion preserves bottom */
      p.property("toInt(⊥) = ⊥") = toInt[I](bottom) == IntLattice[I].bottom
      /** Integer conversion is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ toInt(a) ⊑ toInt(b)") = forAll { (b: R) =>
        forAll(gen.le(b)) { (a: R) =>
          conditional(subsumes(b, a), IntLattice[I].subsumes(toInt[I](b), toInt[I](a)))
        }
      }
      /** Integer conversion is sound */
      p.property("∀ a: a.toInt ⊑ toInt(inject(a))") =
        forAll((a: Double) => IntLattice[I].subsumes(toInt[I](inject(a)), IntLattice[I].inject(a.toInt)))

      /** Ceiling preserves bottom */
      p.property("ceiling(⊥) = ⊥") = ceiling(bottom) == bottom
      /** Ceiling is monotone */
      p.property("∀ a, b: a ⊑ b ⇒ ceiling(a) ⊑ ceiling(b)") = forAll { (b: R) =>
        forAll(gen.le(b)) { (a: R) =>
          conditional(subsumes(b, a), subsumes(ceiling(b), ceiling(a)))
        }
      }
      /** Ceiling is sound */
      p.property("∀ a: inject(a.ceil) ⊑ ceiling(inject(a))") =
        forAll((a: Double) => subsumes(ceiling(inject(a)), inject(MathOps.ceil(a))))

      /** Log preserves bottom */
      p.property("log(⊥) = ⊥") = log(bottom) == bottom
      /** Log is monotone */
      /* TODO[easy] failing test
       p.property("∀ a, b: a ⊑ b ⇒ log(a) ⊑ log(b)") = forAll { (b: R) =>
          forAll(gen.le(b)) { (a: R) =>
            conditional(subsumes(b, a), subsumes(log(b), log(a)))
          }
        }
       */
      /** Log is sound */
      p.property("∀ a: inject(a.log) ⊑ log(inject(a))") =
        forAll((a: Double) => conditional(a > 0, subsumes(log(inject(a)), inject(scala.math.log(a)))))

      /** Random preserves bottom */
      p.property("random(⊥) = ⊥") = random(bottom) == bottom
      /* Random should neither be monotone nor sound (at least in concrete) */

      /** Addition preserves bottom */
      p.property("plus(a, ⊥) = ⊥ = plus(⊥, a)") =
        forAll((a: R) => plus(a, bottom) == bottom && plus(bottom, a) == bottom)
      /** Addition is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ plus(a, b) ⊑ plus(a, c)") = forAll { (a: R, c: R) =>
        forAll(gen.le(c)) { (b: R) =>
          conditional(subsumes(c, b), subsumes(plus(a, c), plus(a, b)))
        }
      }
      /** Addition is sound */
      p.property("∀ a, b: inject(a + b) ⊑ plus(inject(a), inject(b))") =
        forAll((a: Double, b: Double) => subsumes(plus(inject(a), inject(b)), inject(a + b)))
      /* Plus isn't required to be associative or commutative on reals */


      /** Subtraction preserves bottom */
      p.property("minus(a, ⊥) = ⊥ = minus(⊥, a)") =
        forAll((a: R) => minus(a, bottom) == bottom && minus(bottom, a) == bottom)
      /** Subtraction is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ minus(a, b) ⊑ minus(a, c)") = forAll { (a: R, c: R) =>
        forAll(gen.le(c)) { (b: R) =>
          conditional(subsumes(c, b), subsumes(minus(a, c), minus(a, b)))
        }
      }
      /** Subtraction is sound */
      p.property("∀ a, b: inject(a - b) ⊑ minus(inject(a), inject(b))") =
        forAll((a: Double, b: Double) => subsumes(minus(inject(a), inject(b)), inject(a - b)))
      /* Minus isn't required to be anticommutative on reals */

      /** Multiplication preserves bottom */
      p.property("times(a, ⊥) = ⊥ = times(⊥, a)") =
        forAll((a: R) => times(a, bottom) == bottom && times(bottom, a) == bottom)
      /** Multiplication is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ times(a, b) ⊑ times(a, c)") = forAll { (a: R, c: R) =>
        forAll(gen.le(c)) { (b: R) =>
          conditional(subsumes(c, b), subsumes(times(a, c), times(a, b)))
        }
      }
      /** Multiplication is sound */
      p.property("∀ a, b: inject(a + b) ⊑ times(inject(a), inject(b))") =
        forAll((a: Double, b: Double) => subsumes(times(inject(a), inject(b)), inject(a * b)))
      /* Multiplication isn't required to be associative and commutative on reals */

      /** Division preserves bottom */
      p.property("div(a, ⊥) = ⊥ = div(⊥, a)") =
        forAll((a: R) => div(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), div(bottom, a) == bottom))
      /** Division is monotone */
      p.property("∀ a, b, c: b ⊑ c ⇒ div(a, b) ⊑ div(a, c)") = forAll { (a: R, c: R) =>
        forAll(gen.le(c)) { (b: R) =>
          conditional(subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(c, inject(0)),
            subsumes(div(a, c), div(a, b)))
        }
      }
      /** Division is sound */
      p.property("∀ a, b ≠ 0: inject(a / b) ⊑ div(inject(a), inject(b))") =
        forAll((a: Double, b: Double) => conditional(b != 0, subsumes(div(inject(a), inject(b)), inject(a / b))))

      /** Less-than operation preserves bottom */
        p.property("lt(a, ⊥) = ⊥ = lt(⊥, a)") =
          forAll((a: R) => lt[B](a, bottom) == BoolLattice[B].bottom && lt[B](bottom, a) == BoolLattice[B].bottom)
      /** Less-than operation is monotone */
        p.property("∀ a, b, c: b ⊑ c ⇒ lt(a, b) ⊑ lt(a, c)") = forAll { (a: R, c: R) =>
          forAll(gen.le(c)) { (b: R) =>
            conditional(subsumes(b, c), BoolLattice[B].subsumes(lt[B](a, c), lt[B](a, b)))
          }
        }
      /** Less-than operation is sound */
        p.property("∀ a, b ≠ 0: inject(a < b) ⊑ lt(inject(a), inject(b))") =
          forAll((a: Double, b: Double) => BoolLattice[B].subsumes(lt[B](inject(a), inject(b)), BoolLattice[B].inject(a < b)))
      /** To-string operation preserves bottom */
        p.property("toString(⊥) = ⊥") = lat.toString[S](bottom) == StringLattice[S].bottom
      /** To-string operation is monotone */
        p.property("∀ a, b: a ⊑ b ⇒ toString(a) ⊑ toString(b)") = forAll { (b: R) =>
          forAll(gen.le(b)) { (a: R) =>
            conditional(subsumes(b, a),
                  StringLattice[S].subsumes(lat.toString[S](b), lat.toString[S](a)))
          }
        }
      /** To-string operation is sound */
        p.property("∀ a: inject(a.toString) ⊑ toString(inject(a))") =
          forAll((a: Double) => StringLattice[S].subsumes(lat.toString[S](inject(a)), StringLattice[S].inject(a.toString)))
      p
    }
  }
  checkAll(realLaws)
}

abstract class CharLatticeTest[C : CharLattice](gen: LatticeGenerator[C]) extends LatticeTest[C](gen) {
  val charLaws: Properties = {
    newProperties("Char") { p => p }
  }
  checkAll(charLaws)
}

abstract class SymbolLatticeTest[Sym : SymbolLattice](gen: LatticeGenerator[Sym]) extends LatticeTest[Sym](gen) {
  val symbolLaws: Properties = {
    newProperties("Symbol") { p => p }
  }
  checkAll(symbolLaws)
}

class ConcreteBoolTest extends BoolLatticeTest[Concrete.B](ConcreteBooleanGenerator)
class ConcreteStringTest extends StringLatticeTest[Concrete.S, Concrete.I](ConcreteStringGenerator)
class ConcreteIntTest extends IntLatticeTest[Concrete.I, Concrete.B, Concrete.R, Concrete.S](ConcreteIntGenerator)
class ConcreteRealTest extends RealLatticeTest[Concrete.R, Concrete.B, Concrete.I, Concrete.S](ConcreteRealGenerator)
class ConcreteCharTest extends CharLatticeTest[Concrete.C](ConcreteCharGenerator)
class ConcreteSymbolTest extends SymbolLatticeTest[Concrete.Sym](ConcreteSymbolGenerator)

class TypeBoolTest extends BoolLatticeTest[Type.B](TypeGenerator)
class TypeStringTest extends StringLatticeTest[Type.S, Type.I](TypeGenerator)
class TypeIntTest extends IntLatticeTest[Type.I, Type.B, Type.R, Type.S](TypeGenerator)
class TypeRealTest extends RealLatticeTest[Type.R, Type.B, Type.I, Type.S](TypeGenerator)
class TypeCharTest extends CharLatticeTest[Type.C](TypeGenerator)
class TypeSymbolTest extends SymbolLatticeTest[Type.Sym](TypeGenerator)

/* No bool test for constant propagation, as it is equivalent to concrete booleans */
class ConstantPropagationStringTest extends StringLatticeTest[ConstantPropagation.S, ConstantPropagation.I](ConstantPropagationStringGenerator)
class ConstantPropagationIntTest extends IntLatticeTest[ConstantPropagation.I, Concrete.B, ConstantPropagation.R, ConstantPropagation.S](ConstantPropagationIntGenerator)
class ConstantPropagationRealTest extends RealLatticeTest[ConstantPropagation.R, Concrete.B, ConstantPropagation.I, ConstantPropagation.S](ConstantPropagationRealGenerator)
class ConstantPropagationCharTest extends CharLatticeTest[ConstantPropagation.C](ConstantPropagationCharGenerator)
class ConstantPropagationSymbolTest extends SymbolLatticeTest[ConstantPropagation.Sym](ConstantPropagationSymbolGenerator)

