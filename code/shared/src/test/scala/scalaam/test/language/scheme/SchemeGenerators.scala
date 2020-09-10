package scalaam.test.language.scheme

import org.scalacheck._
import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.scheme.lattices.{ModularSchemeLattice, SchemeOps}
import scalaam.language.scheme.primitives._
import scalaam.language.sexp._
import scalaam.lattice._
import scalaam.test.lattice._

trait SchemeLatticeGenerator[L] extends LatticeGenerator[L] {
    /* unary ops */
    val anyUnaryOp: Gen[SchemeOps.UnaryOperator] = Gen.oneOf(SchemeOps.UnaryOperator.values)
    implicit val arbUnop: Arbitrary[SchemeOps.UnaryOperator] = Arbitrary(anyUnaryOp)
    /* binary ops */
    val anyBinaryOp: Gen[SchemeOps.BinaryOperator] = Gen.oneOf(SchemeOps.BinaryOperator.values)
    implicit val arbBinop: Arbitrary[SchemeOps.BinaryOperator] = Arbitrary(anyBinaryOp)
    /* generators for specific values */
    def anyPai: Gen[L]
    def anyVec: Gen[L]
    def anyInt: Gen[L]
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

    val emptyEnv = Environment[SimpleAddr](Iterable.empty)

    // useful shorthands
    lazy val intLat = implicitly[IntLattice[I]]
    lazy val blnLat = implicitly[BoolLattice[B]]
    implicit lazy val valLat = modularLattice.schemeLattice

    // useful helpers (TODO: some of these probably already exist somewhere in ScalaCheck)
    def genTuple[X,Y](genX: Gen[X], genY: Gen[Y]): Gen[(X,Y)] = 
        for { x <- genX ; y <- genY } yield (x,y)
    def pickAtMost[X](max: Int, gen: Gen[X]): Gen[List[X]] = 
        for { n <- Gen.choose(0,max) ; lst <- Gen.listOfN(n,gen) } yield lst

    // for the purposes of generating arbitrary Scheme values, we can just represent addresses with integers
    case class SimpleAddr(addr: Int) extends Address { 
        def printable = true
        def idn = Identity.none
    }
    // we'll use the "real" primitives
    def primitives = new SchemeLatticePrimitives[modularLattice.L, SimpleAddr]
    // the scalaam.modular scalaam.lattice that is used
    lazy val modularLattice: ModularSchemeLattice[SimpleAddr,Nothing,S,B,I,R,C,Sym] = new ModularSchemeLattice
    
    type L = modularLattice.L
    type V = modularLattice.Value

    // useful to reduce the size of counterexample instances
    implicit val shrinkL: Shrink[L] = Shrink[L] {
        case modularLattice.Elements(vs) => 
            Shrink.shrinkContainer[List,V].shrink(vs).map(modularLattice.Elements)
    }
    implicit val shrinkV: Shrink[V] = Shrink[V] {
        case modularLattice.Vec(siz,els) => for {
            elsShrinked <- Shrink.shrinkContainer[Set,(I,L)].shrink(els.toSet)
        } yield modularLattice.Vec(siz,elsShrinked.toMap)
        // TODO: Shrink other values (useful for e.g. concrete scalaam.lattice)
        case _ => Stream.empty
    }
    implicit val shrinkBinding: Shrink[(I,L)] = Shrink[(I,L)] { case (idx,vlu) => 
        for { vluShrinked <- Shrink.shrink(vlu) } yield (idx,vluShrinked)
    }

    object SchemeValueLatticeGenerator extends SchemeLatticeGenerator[L] {
        /* Generate any Scheme value */
        lazy val any: Gen[L] = for {
            nil <- pickAtMost(1, SchemeVLatticeGenerator.anyNilV)
            str <- pickAtMost(1, SchemeVLatticeGenerator.anyStrV)
            bln <- pickAtMost(1, SchemeVLatticeGenerator.anyBlnV)
            int <- pickAtMost(1, SchemeVLatticeGenerator.anyIntV)
            rea <- pickAtMost(1, SchemeVLatticeGenerator.anyReaV)
            chr <- pickAtMost(1, SchemeVLatticeGenerator.anyChrV)
            sym <- pickAtMost(1, SchemeVLatticeGenerator.anySymV)
            prm <- pickAtMost(1, SchemeVLatticeGenerator.anyPrmV)
            ptr <- pickAtMost(1, SchemeVLatticeGenerator.anyPtrV)
            clo <- pickAtMost(1, SchemeVLatticeGenerator.anyCloV)
            lst = nil ++ str ++ bln ++ int ++ rea ++ chr ++ sym ++ prm ++ ptr ++ clo
        } yield modularLattice.Elements(lst.sortBy(_.ord))
        /* Generate any cons-cell */
        def anyPai: Gen[L] = SchemeVLatticeGenerator.anyPaiV.map(modularLattice.Element(_))
        /* Generate any vector */
        def anyVec: Gen[L] = SchemeVLatticeGenerator.anyVecV.map(modularLattice.Element(_))
        /* Generate a Scheme value subsumed by a given Scheme value */
        def le(l: L): Gen[L] = l match {
            case modularLattice.Elements(vs) => for {
                subset <- Gen.someOf(vs)
                subsumed <- Gen.sequence[List[V],V](subset.map(SchemeVLatticeGenerator.le))
            } yield modularLattice.Elements(subsumed.sortBy(_.ord))
        }
        /* Generating specific values */
        def anyInt = SchemeVLatticeGenerator.anyIntV.map(modularLattice.Element(_))
        /* Shrinking values */
        override val shrink = shrinkL 
    }

    object SchemeVLatticeGenerator  {
        // helpers
        val anyAddr: Gen[SimpleAddr] = Gen.choose(0,100).map(SimpleAddr(_)) // addresses are faked in 100 different variations
        val anyClosure: Gen[(valLat.Closure,Option[String])] = for {
            nm1 <- Gen.choose(0,100)                // lambdas are faked in 100 different variations
            lam = SchemeLambda(List(),List(SchemeValue(ValueInteger(nm1), Identity.none)),Identity.none)
            nm2 <- Gen.choose(0,100)                // environments are faked in 100 different variations
        } yield ((lam,emptyEnv),None)
        // a generator for each type of value
        val anyNilV: Gen[V] = Gen.const(modularLattice.Nil)
        val anyIntV: Gen[V] = intGen.any.retryUntil(_ != IntLattice[I].bottom).map(modularLattice.Int)
        val anyStrV: Gen[V] = strGen.any.retryUntil(_ != StringLattice[S].bottom).map(modularLattice.Str)
        val anyBlnV: Gen[V] = blnGen.any.retryUntil(_ != BoolLattice[B].bottom).map(modularLattice.Bool)
        val anyReaV: Gen[V] = reaGen.any.retryUntil(_ != RealLattice[R].bottom).map(modularLattice.Real)
        val anyChrV: Gen[V] = chrGen.any.retryUntil(_ != CharLattice[C].bottom).map(modularLattice.Char)
        val anySymV: Gen[V] = symGen.any.retryUntil(_ != SymbolLattice[Sym].bottom).map(modularLattice.Symbol)
        val anyPrmV: Gen[V] = pickAtMost(3, Gen.oneOf(primitives.allPrimitives)).retryUntil(_.nonEmpty).map(ps => modularLattice.Prim(ps.toSet))
        val anyPtrV: Gen[V] = pickAtMost(3, anyAddr).retryUntil(_.nonEmpty).map(ps => modularLattice.Pointer(ps.toSet))
        val anyCloV: Gen[V] = pickAtMost(3, anyClosure).retryUntil(_.nonEmpty).map(cs => modularLattice.Clo(cs.toSet))
        lazy val anyPaiV: Gen[V] = for {
            car <- SchemeValueLatticeGenerator.any
            cdr <- SchemeValueLatticeGenerator.any
        } yield modularLattice.Cons(car,cdr)
        lazy val anyVecV: Gen[V] = for {
            siz <- intGen.any.suchThat(i => blnLat.isTrue(intLat.lt(intLat.inject(-1),i)))//Gen.oneOf(Gen.posNum[Int].map(intLat.inject), Gen.const(intLat.top))
            con <- vectorContent(siz) 
        } yield modularLattice.Vec(siz,con)
        // any value
        // any value subsumed by a given value
        def le(l: V): Gen[V] = l match {
            case modularLattice.Str(s)      => strGen.le(s).retryUntil(_ != StringLattice[S].bottom).map(modularLattice.Str)
            case modularLattice.Bool(b)     => blnGen.le(b).retryUntil(_ != BoolLattice[B].bottom).map(modularLattice.Bool)
            case modularLattice.Int(i)      => intGen.le(i).retryUntil(_ != IntLattice[I].bottom).map(modularLattice.Int)
            case modularLattice.Real(r)     => reaGen.le(r).retryUntil(_ != RealLattice[R].bottom).map(modularLattice.Real)
            case modularLattice.Char(c)     => chrGen.le(c).retryUntil(_ != CharLattice[C].bottom).map(modularLattice.Char)
            case modularLattice.Symbol(s)   => symGen.le(s).retryUntil(_ != SymbolLattice[Sym].bottom).map(modularLattice.Symbol)
            case modularLattice.Prim(ps)    => Gen.someOf(ps).retryUntil(_.nonEmpty).map(_.toSet).map(modularLattice.Prim)
            case modularLattice.Clo(cs)     => Gen.someOf(cs).retryUntil(_.nonEmpty).map(_.toSet).map(modularLattice.Clo)
            case modularLattice.Pointer(ps) => Gen.someOf(ps).retryUntil(_.nonEmpty).map(_.toSet).map(modularLattice.Pointer)
            case modularLattice.Cons(a,d)   => for {
                ale <- SchemeValueLatticeGenerator.le(a)
                dle <- SchemeValueLatticeGenerator.le(d)
            } yield modularLattice.Cons(ale,dle)
            case modularLattice.Vec(s,c) => for {
                sle <- intGen.le(s).suchThat(i => i == s || i != intLat.bottom)
                cle <- vectorContentLe(c,sle)
            } yield modularLattice.Vec(sle,cle)
            case _ => Gen.const(l)  
        }
        // with the current representation, vectors are tricky to handle
        private def vectorContent(siz: I): Gen[Map[I,L]] = for {
            maxSize <- Gen.choose(0,5)
            bindings <- Gen.mapOfN(maxSize, genTuple(intGen.any, SchemeValueLatticeGenerator.any)) 
        } yield vectorNormalize(bindings, siz)
        private def vectorContentLe(bds: Map[I,L], siz: I): Gen[Map[I,L]] = for {
            removed <- Gen.someOf(bds)
            subsumed = removed.map(b => Gen.choose(1,3).flatMap(Gen.listOfN(_, genTuple(intGen.le(b._1), SchemeValueLatticeGenerator.le(b._2)))))
            values <- Gen.sequence[Set[List[(I,L)]],List[(I,L)]](subsumed).map(_.flatten)
            augmented <- Gen.choose(0,5).flatMap(Gen.mapOfN(_, genTuple(intGen.any, SchemeValueLatticeGenerator.any)))
        } yield vectorNormalize((values ++ augmented).toMap,siz)
        private def vectorNormalize(bds: Map[I,L], siz: I): Map[I,L] = {
            val bds1 = bds.filter { case (idx,_) => blnLat.isTrue(intLat.lt(idx,siz)) }
            val idxs = bds1.map(_._1)
            bds1.filter(bnd => !idxs.exists(idx => idx != bnd._1 && intLat.subsumes(idx,bnd._1)))
        }
    }
}

object ConstantModularSchemeLattice  extends ModularSchemeLatticeGenerator(ConstantPropagationStringGenerator, ConcreteBooleanGenerator, ConstantPropagationIntGenerator,ConstantPropagationRealGenerator,ConstantPropagationCharGenerator,ConstantPropagationSymbolGenerator)
object ConcreteModularSchemeLattice  extends ModularSchemeLatticeGenerator(ConcreteStringGenerator, ConcreteBooleanGenerator, ConcreteIntGenerator, ConcreteRealGenerator, ConcreteCharGenerator, ConcreteSymbolGenerator)
object TypeModularSchemeLattice      extends ModularSchemeLatticeGenerator(TypeGenerator, TypeGenerator, TypeGenerator, TypeGenerator, TypeGenerator, TypeGenerator)