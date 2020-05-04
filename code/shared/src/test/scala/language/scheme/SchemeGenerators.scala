package scalaam.test.lattice.scheme

import org.scalacheck.{Gen, Shrink, Arbitrary}

import scalaam.test.lattice._

import scalaam.core._
import scalaam.lattice._
import scalaam.language.sexp._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._

trait SchemeLatticeGenerator[L] extends LatticeGenerator[L] {
    /* unary ops */
    val anyUnaryOp: Gen[SchemeOps.UnaryOperator] = Gen.oneOf(SchemeOps.UnaryOperator.values)
    implicit val arbUnop: Arbitrary[SchemeOps.UnaryOperator] = Arbitrary(anyUnaryOp)
    /* binary ops */
    val anyBinaryOp: Gen[SchemeOps.BinaryOperator] = Gen.oneOf(SchemeOps.BinaryOperator.values)
    implicit val arbBinop: Arbitrary[SchemeOps.BinaryOperator] = Arbitrary(anyBinaryOp)
}


abstract class ModularSchemeLatticeGenerator[
    S: StringLattice,
    B: BoolLattice,
    I: IntLattice,
    R: RealLattice,
    C: CharLattice,
    Sym: SymbolLattice
](strGen: LatticeGenerator[S],
  blnGen: LatticeGenerator[B],
  intGen: LatticeGenerator[I],
  reaGen: LatticeGenerator[R],
  chrGen: LatticeGenerator[C],
  symGen: LatticeGenerator[Sym]) {

    // useful shorthands
    val intLat = implicitly[IntLattice[I]]
    val blnLat = implicitly[BoolLattice[B]]
    val valLat = modularLattice.schemeLattice

    // useful helpers (TODO: some of these probably already exist somewhere in ScalaCheck)
    def genTuple[A,B](genA: Gen[A], genB: Gen[B]): Gen[(A,B)] = 
        for { a <- genA ; b <- genB } yield (a,b)
    def pickAtMost[X](max: Int, gen: Gen[X]): Gen[Set[X]] = 
        for { n <- Gen.choose(0,max) ; lst <- Gen.listOfN(n,gen) } yield (lst.toSet)

    // for the purposes of generating arbitrary Scheme values, we can just represent addresses and environments with integers
    case class SimpleAddr(addr: Int) extends Address { def printable = true }
    case class SimpleEnv(id: Int)
    // we'll use the "real" primitives
    def primitives = new SchemeLatticePrimitives[modularLattice.L, SimpleAddr]
    // the modular lattice that is used
    lazy val modularLattice: ModularSchemeLattice[SimpleAddr,SimpleEnv,S,B,I,R,C,Sym] = new ModularSchemeLattice
    
    type L = modularLattice.L
    type V = modularLattice.Value

    // helper to generate the correct Element/Elements value from a given set of Values
    private def buildL(values: Set[V]): L = 
        if (values.isEmpty) {
            valLat.bottom
        } else if (values.size == 1) {
            modularLattice.Element(values.head)
        } else {
            modularLattice.Elements(values)
        }
    // useful to reduce the size of counterexample instances
    implicit val shrinkL: Shrink[L] = Shrink[L] {
        case modularLattice.Element(v) => 
            Shrink.shrink(v).map(modularLattice.Element)
        case modularLattice.Elements(vs) => 
            Shrink.shrinkContainer[Set,V].shrink(vs).map(buildL)
    }
    implicit val shrinkV: Shrink[V] = Shrink[V] {
        case modularLattice.Vec(siz,els,ini) => for {
            iniShrinked <- Shrink.shrink(ini)
            elsShrinked <- Shrink.shrinkContainer[Set,(I,L)].shrink(els.toSet)
        } yield modularLattice.Vec(siz,elsShrinked.toMap,iniShrinked)
        // TODO: Shrink other values (useful for e.g. concrete lattice)
        case _ => Stream.empty
    }
    implicit val shrinkBinding: Shrink[(I,L)] = Shrink[(I,L)] { case (idx,vlu) => 
        for { vluShrinked <- Shrink.shrink(vlu) } yield (idx,vluShrinked)
    }

    object SchemeValueLatticeGenerator extends SchemeLatticeGenerator[L] {
        /* Generate any Scheme value */
        def any: Gen[L] = Gen.oneOf(elementGen, elementsGen)
        private def elementGen: Gen[L] = 
            SchemeVLatticeGenerator.any.map(modularLattice.Element)
        private def elementsGen: Gen[L] = for {
            nil <- pickAtMost(1, SchemeVLatticeGenerator.anyNilV)
            str <- pickAtMost(1, SchemeVLatticeGenerator.anyStrV)
            bln <- pickAtMost(1, SchemeVLatticeGenerator.anyBlnV)
            int <- pickAtMost(1, SchemeVLatticeGenerator.anyIntV)
            rea <- pickAtMost(1, SchemeVLatticeGenerator.anyReaV)
            chr <- pickAtMost(1, SchemeVLatticeGenerator.anyChrV)
            sym <- pickAtMost(1, SchemeVLatticeGenerator.anySymV)
            prm <- pickAtMost(3, SchemeVLatticeGenerator.anyPrmV)
            ptr <- pickAtMost(3, SchemeVLatticeGenerator.anyPtrV)
            pai <- pickAtMost(3, SchemeVLatticeGenerator.anyPaiV)
            vec <- pickAtMost(1, SchemeVLatticeGenerator.anyVecV)
            clo <- pickAtMost(3, SchemeVLatticeGenerator.anyCloV)
        } yield buildL(nil ++ str ++ bln ++ int ++ rea ++ chr ++ sym ++ prm ++ ptr ++ pai ++ vec ++ clo)
        /* Generate a Scheme value subsumed by a given Scheme value */
        def le(l: L): Gen[L] = l match {
            case modularLattice.Element(v) => 
                SchemeVLatticeGenerator.le(v).map(modularLattice.Element)
            case modularLattice.Elements(vs) => for {
                subset <- Gen.someOf(vs)
                subsumed <- Gen.sequence[Set[V],V](subset.map(SchemeVLatticeGenerator.le))
            } yield buildL(subsumed)
        }
        override val shrink = shrinkL 
    }

    object SchemeVLatticeGenerator extends LatticeGenerator[V] {
        // any address
        val anyAddr: Gen[SimpleAddr] = Gen.choose(0,100).map(SimpleAddr(_)) // addresses are faked in 100 different variations
                private def vectorContent(siz: I, ini: L): Gen[Map[I,L]] = for {
            maxSize <- Gen.choose(0,5)
            bindings <- Gen.mapOfN(maxSize, genTuple(intGen.any, SchemeValueLatticeGenerator.any)) 
        } yield vectorNormalize(bindings, siz, ini)
        // a generator for each type of value
        val anyBotV: Gen[V] = Gen.const(modularLattice.Bot)
        val anyNilV: Gen[V] = Gen.const(modularLattice.Nil)
        val anyIntV: Gen[V] = intGen.any.map(modularLattice.Int)
        val anyStrV: Gen[V] = strGen.any.map(modularLattice.Str)
        val anyBlnV: Gen[V] = blnGen.any.map(modularLattice.Bool)
        val anyReaV: Gen[V] = reaGen.any.map(modularLattice.Real)
        val anyChrV: Gen[V] = chrGen.any.map(modularLattice.Char)
        val anySymV: Gen[V] = symGen.any.map(modularLattice.Symbol)
        val anyPrmV: Gen[V] = Gen.oneOf(primitives.allPrimitives).map(modularLattice.Prim)
        val anyPtrV: Gen[V] = anyAddr.map(modularLattice.Pointer)
        val anyCloV: Gen[V] = for {
            nm1 <- Gen.choose(0,100)                // lambdas are faked in 100 different variations
            lam = SchemeLambda(List(),List(SchemeValue(ValueInteger(nm1), Identity.none)),Identity.none)
            nm2 <- Gen.choose(0,100)                // environments are faked in 100 different variations
            env = SimpleEnv(nm2)
        } yield modularLattice.Clo(lam,env,None)
        val anyPaiV: Gen[V] = for {
            carAddr <- anyAddr
            cdrAddr <- anyAddr
        } yield modularLattice.Cons(carAddr,cdrAddr)
        val anyVecV: Gen[V] = for {
            siz <- intGen.any.suchThat(i => blnLat.isTrue(intLat.lt(intLat.inject(-1),i)))
            ini <- SchemeValueLatticeGenerator.any
            con <- vectorContent(siz,ini) 
        } yield modularLattice.Vec(siz,con,ini)
        // any value
        def any: Gen[modularLattice.Value] = 
            Gen.oneOf(anyBotV, anyNilV, anyStrV, anyBlnV, anyIntV, anyReaV, anyChrV, 
                      anySymV, anyPrmV, anyPtrV, anyCloV, anyVecV, anyPaiV)
        // any value subsumed by a given value
        def le(l: V): Gen[V] =
            Gen.oneOf(Gen.const(modularLattice.Bot), l match {
                case modularLattice.Str(s) => strGen.le(s).map(modularLattice.Str)
                case modularLattice.Bool(b) => blnGen.le(b).map(modularLattice.Bool)
                case modularLattice.Int(i) => intGen.le(i).map(modularLattice.Int)
                case modularLattice.Real(r) => reaGen.le(r).map(modularLattice.Real)
                case modularLattice.Char(c) => chrGen.le(c).map(modularLattice.Char)
                case modularLattice.Symbol(s) => symGen.le(s).map(modularLattice.Symbol)
                case modularLattice.Vec(s,c,i) => for {
                    sle <- intGen.le(s).suchThat(i => i == s || i != intLat.bottom)
                    ile <- SchemeValueLatticeGenerator.le(i)
                    cle <- vectorContentLe(c,sle,i,ile)
                } yield modularLattice.Vec(sle,cle,ile)
                case _ => Gen.const(l)  
            })
        // with the current representation, vectors are tricky to handle
        private def vectorContentLe(bds: Map[I,L], siz: I, i1: L, i2: L): Gen[Map[I,L]] = for {
            removed <- Gen.someOf(bds)
            subsumed = removed.map(b => Gen.choose(1,3).flatMap(Gen.listOfN(_, genTuple(intGen.le(b._1), SchemeValueLatticeGenerator.le(b._2)))))
            values <- Gen.sequence[Set[List[(I,L)]],List[(I,L)]](subsumed).map(_.flatten)
            augmented <- Gen.choose(0,5).flatMap(Gen.mapOfN(_, genTuple(intGen.any, SchemeValueLatticeGenerator.le(i1))))
        } yield vectorNormalize((values ++ augmented).toMap,siz,i2)
        private def vectorNormalize(bds: Map[I,L], siz: I, ini: L): Map[I,L] = {
            val bds1 = bds.filter { case (idx,vlu) =>
                blnLat.isTrue(intLat.lt(idx,siz)) && 
                blnLat.isTrue(intLat.lt(intLat.inject(-1),idx)) &&
                !valLat.subsumes(ini,vlu)
            }
            val idxs = bds1.map(_._1)
            bds1.filter(bnd => !idxs.exists(idx => idx != bnd._1 && intLat.subsumes(idx,bnd._1)))
        }
        override val shrink = shrinkV
    }
}

object ConstantModularSchemeLattice  extends ModularSchemeLatticeGenerator(ConstantPropagationStringGenerator, ConcreteBooleanGenerator, ConstantPropagationIntGenerator,ConstantPropagationRealGenerator,ConstantPropagationCharGenerator,ConstantPropagationSymbolGenerator)
object ConcreteModularSchemeLattice  extends ModularSchemeLatticeGenerator(ConcreteStringGenerator, ConcreteBooleanGenerator, ConcreteIntGenerator, ConcreteRealGenerator, ConcreteCharGenerator, ConcreteSymbolGenerator)
object TypeModularSchemeLattice      extends ModularSchemeLatticeGenerator(TypeGenerator, TypeGenerator, TypeGenerator, TypeGenerator, TypeGenerator, TypeGenerator)