package scalaam.primitiveCompilation

import scalaam.primitiveCompilation.PrimSource._
import scalaam.primitiveCompilation.PrimTarget._
import scalaam.primitiveCompilation.ANFCompiler._
import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.sexp._

object Test extends App {
  val program1 = "(define (length l) (let ((n (null? l))) (if n 0 (let ((c (cdr l))) (let ((len (length c))) (+ 1 len))))))"
  val program2 = "(define (length l) (if (null? l) 0 (+ 1 (length (cdr l)))))"
  val program3 = "(define (gcd a b)(let ((nul (= b 0))) (if nul a (let ((mod (modulo a b))) (gcd b mod)))))"
  println(PrimCompiler.compile(toANF(SchemeParser.parse(program1))))
  println(PrimCompiler.compile(toANF(SchemeParser.parse(program2))))

}

object PrimCompiler {

  type SE = PrimSource.Exp
  type TE = PrimTarget.Exp

  type SA = PrimSource.AExp
  type TA = PrimTarget.AExp

  trait CompilerException extends Exception

  case class  IllegalExpressionException(e: Expression) extends Exception // For expressions that should not occur in the language compiled in the current phase.
  case class ShouldNotEncounterException(e: Expression) extends Exception // For expressions that should not be encountered by the compiler in the current phase.

  case class PrimInfo(name: String, args: List[Identifier], recursive: Boolean, storeUsage: Boolean)
  type PI = PrimInfo

  def compile(exp: SchemeExp): String = toScala(toTarget(toSource(exp)))

  /////////////////////////////
  // FIRST COMPILATION PHASE //
  /////////////////////////////

  def toSource(exp: SchemeExp): (SE, PI) = {

    def valueToSource(v: Value): SE = v match {
      case ValueBoolean(bool) => AE(PrimSource.Boo(bool))
      case ValueInteger(int) => AE(PrimSource.Num(int))
    }

    var rec: Boolean = false
    var sto: Boolean = false
    var prm: String = ""

    // Mark a primitive as recursive if a function with the same name is called within the body (note: we do not take into account bindings to the same name -> TODO).
    // TODO write this in a functional way.
    def bodyToSource(exp: Expression): SE = exp match {
      case fc@SchemeFuncall(f, args, _) =>
        val argn = args.map(arg => (bodyToSource(arg), arg.idn.pos))
        if (!argn.forall(_._1.isInstanceOf[PrimSource.AE])) throw IllegalExpressionException(fc)
        val argv: PrimSource.Args = argn.map{case (AE(ae), pos) => (ae, pos)}.toArray
        bodyToSource(f) match { // TODO maybe check arity?
          case prim@AE(PrimSource.Var(Id(name))) if prm == name =>
            rec = true
            PrimSource.PrimCall(prim, argv, true, fc.idn.pos)
          case AE(PrimSource.Var(Id(name))) if PrimitiveOperations.opNams.contains(name) =>
            if (PrimitiveOperations.stoNams.contains(name)) sto = true // TODO is this list sufficient?
            PrimSource.OpCall(PrimitiveOperations.ops.find(_.name == name).get, argv, fc.idn.pos)
          case prim => PrimSource.PrimCall(prim, argv, false, fc.idn.pos)
        }
      case SchemeIf(cond, cons, alt, _) => bodyToSource(cond) match {
        case AE(ae) => If(ae, bodyToSource(cons), bodyToSource(alt))
        case _ => throw IllegalExpressionException(cond) // TODO: support?
      }
      // For now, let behaves as a let* TODO?
      // Restrain a body to a single expression...
      // Important: use foldRight!
      case SchemeLet(bnd, body :: Nil, _) => bnd.foldRight(bodyToSource(body)){ case ((id, bnd), acc) => Let(PrimSource.Var(Id(id.name)), bodyToSource(bnd), acc) }
      case SchemeLetStar(bnd, body :: Nil, _) => bnd.foldRight(bodyToSource(body)){ case ((id, bnd), acc) => Let(PrimSource.Var(Id(id.name)), bodyToSource(bnd), acc) }
      // Dysfunctional begin now. TODO?
      case SchemeBegin(e :: Nil, _) => bodyToSource(e)
      case SchemeDefineVariable(id, exp, _) => Let(PrimSource.Var(Id(id.name)), bodyToSource(exp), ???) // TODO
      case SchemeVar(id) => AE(PrimSource.Var(Id(id.name)))
      case SchemeValue(v, _) => valueToSource(v)
      case id@Identifier(_, _) => throw ShouldNotEncounterException(id)
      case e => throw IllegalExpressionException(e)
    }

    exp match {
      // A primitive function is expected to be a function definition.
      case SchemeDefineFunction(nam, args, body :: Nil, _) =>
        prm = nam.name
        val src = bodyToSource(body)
        (src, PrimInfo(prm, args, rec, sto))
      case e => throw IllegalExpressionException(e)
    }
  }

  //////////////////////////////
  // SECOND COMPILATION PHASE //
  //////////////////////////////

  def toTarget(src: (SE, PI)): (TE, PI) = {

    val sto = src._2.storeUsage

    def AExpToTarget(ae: SA): TA = ae match {
      case PrimSource.Boo(b) => PrimTarget.Boo(b)
      case PrimSource.Num(n) => PrimTarget.Num(n)
      case PrimSource.Var(v) => PrimTarget.Var(v)
    }

    def varToTarget(v: PrimSource.Var): PrimTarget.Var = PrimTarget.Var(v.v)

    def toTarget(exp: SE): TE = exp match {
      case AE(ae) => Lat(Inj(AExpToTarget(ae)))
      case If(cond, cons, alt) => Cond(Inj(AExpToTarget(cond)), toTarget(cons), toTarget(alt))
      /*
      case If(cond, cons, alt) =>
        val v0 = PrimTarget.Var()
        val v1 = PrimTarget.Var()
        val v2 = PrimTarget.Var()
        Bind(v0, Lat(Inj(AExpToTarget(cond))), // BindC
          Bind(v1, IfTrue(Inj(v0), toTarget(cons)), // BindC
            Bind(v2, IfFalse(Inj(v0), toTarget(alt)), // BindC
              Lat(Join(Inj(v1),
                Inj(v2))))))
      */
      case Let(v, init, body) => Bind(varToTarget(v), toTarget(init), toTarget(body))
      case PrimSource.PrimCall(prim, args, rec, pos) =>
        PrimTarget.PrimCall(toTarget(prim), Args(args.map({ case (ae, pos) => (AExpToTarget(ae), pos) })), rec, sto, pos)
      case PrimSource.OpCall(op, args, pos) =>
        PrimTarget.OpCall(op, Args(args.map({ case (ae, pos) => (AExpToTarget(ae), pos) })), pos)
    }

    (toTarget(src._1), src._2)
  }

  /////////////////////////////
  // THIRD COMPILATION PHASE //
  /////////////////////////////

  def sanitize(name: String): String = name.replaceAll("[^a-zA-Z0-9]+","_")
  def sanitize(name: String, text: String): String = text.replaceAll(name, sanitize(name))

  // TODO: name cleaning (e.g. '-' in list-ref,...) but also have to adapt body then (in case of recursion).
  def toScala(tar: (TE, PI)): String = {
    val PrimInfo(name, args, rec, sto) = tar._2

    def nonRecursive: String =
s"""object ${sanitize(name).capitalize} extends NoStoreOperation("$name", Some(${args.length})) {
  private def appl(args: List[V]): MayFail[V, Error] = {
${if (args.nonEmpty) s"    val ${args.mkString(" :: ")} :: Nil = args\n${tar._1.print(4)}" else tar._1.print(4)}
  }
  override def call(args: List[V]): MayFail[V, Error] = if (args.length == ${args.length}) appl(args) else MayFail.failure(PrimitiveArityError("$name", ${args.length}, args.length))
}"""
    def recursive: String =
s"""object ${sanitize(name).capitalize} extends SimpleFixpointPrimitive("$name", Some(${args.length})) {
  private def appl(args: Args, $name: Args => MayFail[V, Error]): MayFail[V, Error] = {
${if (args.nonEmpty) s"    val ${args.mkString(" :: ")} :: Nil = args\n${tar._1.print(4)}" else tar._1.print(4)}
  }
  def callWithArgs(args: Args, $name: Args => MayFail[V, Error]): MayFail[V, Error] = if (args.length == ${args.length}) appl(args, $name) else MayFail.failure(PrimitiveArityError("$name", ${args.length}, args.length))
}"""
    def recursiveWithStore: String =
s"""object ${sanitize(name).capitalize} extends SimpleFixpointPrimitiveUsingStore("$name", Some(${args.length})) {
  private def appl(fpos: Identity.Position, args: Args, store: Store[A,V], ${sanitize(name)}: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[V, Error] = {
${if (args.nonEmpty) s"    val ${args.mkString(" :: ")} :: Nil = args\n${sanitize(name,tar._1.print(4))}" else sanitize(name, tar._1.print(4))}
  }
  def callWithArgs(fpos: Identity.Position, args: Args, store: Store[A,V], ${sanitize(name)}: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[V, Error] =
    if (args.length == ${args.length}) appl(fpos, args, store, ${sanitize(name)}, alloc) else MayFail.failure(PrimitiveArityError("$name", ${args.length}, args.length))
}"""
    (rec, sto) match {
      case (true, false)  => recursive
      case (false, false) => nonRecursive
      case (_, true)   => recursiveWithStore
      //case (false, true)  => recursiveWithStore // TODO: are there primitives of this kind? (Note: If enabled, also enable this in PrimTarget.scala).
    }
  }

}
