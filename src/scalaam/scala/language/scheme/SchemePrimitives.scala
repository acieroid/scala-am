package scalaam.language.scheme

import scalaam.core._

trait SchemePrimitives[A <: Address, V, T, C] extends Semantics[SchemeExp, A, V, T, C] {
  implicit val timestamp: Timestamp[T, C]
  implicit val schemeLattice: SchemeLattice[V, SchemeExp, A]

  case class PrimitiveArityError(name: String, expected: Int, got: Int) extends Error
  trait Primitive {
    def name: String
    def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T): Set[Action.A] =
      callMF(fexp, args, store, t).mapSet[Action.A](vstore => Action.Value(vstore._1, vstore._2))(err => Action.Err(err))
    def callMF(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T): MayFail[(V, Store[A, V]), Error]
  }
  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def allPrimitives: List[Primitive] = {
    import PrimitiveDefs._
    List(
      Plus,
      )
  }

  object PrimitiveDefs {
      /** Helper for defining operations that do not modify the store */
    abstract class NoStoreOperation(val name: String, val nargs: Option[Int] = None) extends Primitive {
      def callMF(args: List[V]): MayFail[V, Error] = MayFail.failure(PrimitiveArityError(name, nargs.getOrElse(-1), args.length))
      def callMF(arg1: V, arg2: V): MayFail[V, Error] = callMF(List(arg1, arg2))
      def callMF(arg1: (SchemeExp, V), arg2: (SchemeExp, V)): MayFail[V, Error] = callMF(arg1._2, arg2._2)
      def callMF(fexp: SchemeExp, arg1: (SchemeExp, V), arg2: (SchemeExp, V)): MayFail[V, Error] = callMF(arg1, arg2)
      def callMF(arg: V): MayFail[V, Error] = callMF(List(arg))
      def callMF(arg: (SchemeExp, V)): MayFail[V, Error] = callMF(arg._2)
      def callMF(fexp: SchemeExp, arg: (SchemeExp, V)): MayFail[V, Error] = callMF(arg)
      def callMF(): MayFail[V, Error] = callMF(List())
      def callMF(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T): MayFail[(V, Store[A, V]), Error] = (args match {
        case Nil => callMF()
        case x :: Nil => callMF(fexp, x)
        case x :: y :: Nil => callMF(fexp, x, y)
        case _ => callMF(args.map({ case (_, v) => v }))
      }).map(v => (v, store))
    }

    import schemeLattice._
    /* Simpler names for lattice operations */
    def isNull = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsNull) _
    def isCons = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsCons) _
    def isChar = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsChar) _
    def isSymbol = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsSymbol) _
    def isString = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsString) _
    def isInteger = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsInteger) _
    def isReal = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsReal) _
    def isBoolean = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsBoolean) _
    def isVector = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsVector) _
    def ceiling = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Ceiling) _
    def floor = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Floor) _
    def round = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Round) _
    def log = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Log) _
    def not = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Not) _
    def random = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Random) _
    def sin = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Sin) _
    def asin = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ASin) _
    def cos = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Cos) _
    def acos = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ACos) _
    def tan = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Tan) _
    def atan = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ATan) _
    def sqrt = schemeLattice.unaryOp(SchemeOps.UnaryOperator.Sqrt) _
    def vectorLength = schemeLattice.unaryOp(SchemeOps.UnaryOperator.VectorLength) _
    def stringLength = schemeLattice.unaryOp(SchemeOps.UnaryOperator.StringLength) _
    def numberToString = schemeLattice.unaryOp(SchemeOps.UnaryOperator.NumberToString) _
    def symbolToString = schemeLattice.unaryOp(SchemeOps.UnaryOperator.SymbolToString) _
    def stringToSymbol = schemeLattice.unaryOp(SchemeOps.UnaryOperator.StringToSymbol) _
    def inexactToExact = schemeLattice.unaryOp(SchemeOps.UnaryOperator.InexactToExact) _
    def exactToInexact = schemeLattice.unaryOp(SchemeOps.UnaryOperator.ExactToInexact) _

    def plus = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Plus) _
    def minus = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Minus) _
    def times = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Times) _
    def div = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Div) _
    def quotient = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Quotient) _
    def modulo = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Modulo) _
    def remainder = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Remainder) _
    def lt = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Lt) _
    def numEq = schemeLattice.binaryOp(SchemeOps.BinaryOperator.NumEq) _
    def eqq = schemeLattice.binaryOp(SchemeOps.BinaryOperator.Eq) _
    def stringAppend = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringAppend) _
    def stringLt = schemeLattice.binaryOp(SchemeOps.BinaryOperator.StringLt) _

    object Plus extends NoStoreOperation("+") {
      override def callMF(args: List[V]) = args match {
        case Nil => number(0)
        case x :: rest => callMF(rest).flatMap(y => plus(x, y))
      }
    }
  }
}


