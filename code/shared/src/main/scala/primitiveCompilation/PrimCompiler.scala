package scalaam.primitiveCompilation

import java.util.regex.Matcher

import scalaam.primitiveCompilation.PrimSource._
import scalaam.primitiveCompilation.PrimTarget._
import scalaam.primitiveCompilation.ANFCompiler._
import scalaam.core._
import scalaam.language.scheme._
import scalaam.language.sexp._
import scalaam.modular._
import scalaam.modular.scheme._

object GeneratePrimitives extends App {
  import java.io._
  val file = new File("CompiledPrimitives.scala")
  val bw = new BufferedWriter(new FileWriter(file))
  for {
    name <- List("<=", ">=", ">",
      "zero?", "positive?", "negative?", "odd?", "even?",
      "max", "min", "abs", "gcd", "lcm",
      "not",
      "newline", "display",
      "caar", "cadr", "cddr", "cdar", "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr", "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr", "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
      "equal?", "list?", "list-ref", "member", "memq", "assoc", "assq", "list-tail", "length", "append", "reverse")
  } {
    bw.write(PrimCompiler.compile(toANF(SchemeParser.parse(Primitives.primitives(name)))))
    bw.write("\n\n")
  }
  bw.close
}

object Benchmark extends App {
  def run(file: String) = {
    println(s"Running on $file")
    val program = Primitives.parseWithoutPrelude(file)
    println(s"Successfully parsed")
    val analysis = new ModAnalysis(program) with BigStepSemantics with ConstantPropagationDomain with NoSensitivity with StandardSchemeModFSemantics
    val t0 = System.nanoTime()
    analysis.analyze()
    val t1 = System.nanoTime()
    analysis.debug()
//    println(s"Store: ${analysis.store}")
  }
/*  run("test/mceval.scm")
  run("test/scp1/9.12.scm") */
  run("test/gabriel/browse.scm")
/*  run("test/scp1/8.15.scm") */
//  run("test/gambit/mazefun.scm")
/*  run("test/gabriel/diviter.scm")
  run("test/gabriel/divrec.scm")
  run("test/gambit/matrix.scm")
  run("test/scp1/9.18.scm")
  run("test/scp1/5.14.3.scm")
  run("test/scp1/7.16.scm")
  run("test/scp1/9.16.scm")
  run("test/gambit/destruc.scm")
  run("test/gabriel/destruc.scm")
  run("test/gabriel/dderiv.scm")
  run("test/scp1/7.11.scm")
  run("test/scp1/7.13.scm")
  run("test/scp1/5.22.scm")
  run("test/scp1/7.14.scm")
  run("test/WeiChenRompf2019/kcfa-worst-case-256.scm")
  run("test/scp1/7.4.scm")
  run("test/scp1/7.17.scm")
  run("test/scp1/9.14.scm")
  run("test/scp1/7.9.scm")
//  run("test/sigscheme/mem.scm")
  run("test/scp1/7.15.scm")
  run("test/sat.scm")
  run("test/gabriel/deriv.scm")
  run("test/sigscheme/takr.scm")
  run("test/scp1/7.12.scm")
  run("test/regex.scm")
  run("test/grid.scm")
  run("test/gabriel/puzzle.scm")
  run("test/scp1/5.20.4.scm")
  run("test/scp1/5.19.scm")
  run("test/scp1/9.15.scm")
 */

/*
//  run("test/gabriel/browse.scm")
//  run("test/gabriel/cpstak.scm")
//  run("test/gabriel/dderiv.scm")
//  run("test/gabriel/deriv.scm")
//  run("test/gabriel/destruc.scm")
//  run("test/gabriel/diviter.scm")
//  run("test/gabriel/divrec.scm")
//  run("test/gabriel/puzzle.scm")
//  run("test/gabriel/takl.scm")
  run("test/sat.scm")
  run("test/bound-precision.scm")
  run("test/collatz.scm")
  run("test/fib.scm")
  run("test/my-list.scm")
  run("test/sq.scm")
  run("test/scp1/2.1.scm")
  run("test/scp1/9.14.scm")
  run("test/scp1/7.11.scm")
  run("test/scp1/7.4.scm")
  run("test/scp1/8.6.scm")
  run("test/scp1/7.13.scm")
//  run("test/scp1/8.5.scm") // relies on apply (unsupported)
  run("test/scp1/8.12.scm")
  run("test/scp1/7.5.scm")
  run("test/scp1/3.8.scm")
  run("test/scp1/5.22.scm")
//  run("test/scp1/7.18.scm") // TODO: reverse, map
  run("test/scp1/5.20.4.scm")
  run("test/scp1/9.16.scm")
  run("test/scp1/9.3.scm")
  run("test/scp1/3.1.scm")
  run("test/scp1/8.15.scm")
  run("test/scp1/7.12.scm")
  run("test/scp1/9.9.scm")
  run("test/scp1/7.17.scm")
  run("test/scp1/5.7.scm")
  run("test/scp1/9.13.scm")
  run("test/scp1/3.6.scm")
//  run("test/scp1/9.8.scm") // right-rotate
  run("test/scp1/5.19.scm")
  run("test/scp1/8.14.scm")
  run("test/scp1/8.10.scm")
  run("test/scp1/3.2.1.scm")
  run("test/scp1/9.15.scm")
  run("test/scp1/7.6.scm")
  run("test/scp1/3.2.scm")
  run("test/scp1/9.5.scm")
  run("test/scp1/9.12.scm")
  run("test/scp1/7.9.scm")
  run("test/scp1/9.17.scm")
  run("test/scp1/9.2.scm")
  run("test/scp1/5.6.scm")
  run("test/scp1/7.2.scm")
  run("test/scp1/3.9.scm")
  run("test/scp1/7.14.scm")
  run("test/scp1/5.14.3.scm")
  run("test/scp1/8.1.3.scm")
  run("test/scp1/9.7.scm")
  run("test/scp1/7.15.scm")
//  run("test/scp1/8.16.scm") // TODO: for-each
  run("test/scp1/8.13.scm")
//  run("test/scp1/8.11.scm") // TODO: map
  run("test/scp1/7.3.scm")
  run("test/scp1/3.4.scm")
  run("test/scp1/9.6.scm")
  run("test/scp1/3.3.scm")
//  run("test/scp1/4.8.scm") // sqrt
  run("test/scp1/8.1.1.scm")
  run("test/scp1/7.16.scm")
  run("test/scp1/9.18.scm")
  run("test/scp1/4.1.scm")
  run("test/scp1/5.21.scm")
  run("test/scp1/2.4.scm")
  run("test/letrec-begin.scm")
  run("test/church-6.scm")
  run("test/work.scm")
  run("test/blur.scm")
//  run("test/quasiquoting.scm") // unsupported
  run("test/rosetta/easter.scm")
  run("test/rosetta/quadratic.scm")
  run("test/mut-rec.scm")
  run("test/regex.scm")
  run("test/sym.scm")
  run("test/widen.scm")
  run("test/mceval.scm")
  run("test/WeiChenRompf2019/solovay-strassen.scm")
  run("test/WeiChenRompf2019/kcfa-worst-case-256.scm")
  run("test/WeiChenRompf2019/omega.scm")
  run("test/WeiChenRompf2019/kcfa-worst-case-16.scm")
  run("test/WeiChenRompf2019/kcfa-worst-case-64.scm")
  run("test/WeiChenRompf2019/kcfa3.scm")
//  run("test/WeiChenRompf2019/regex-derivative.scm") // cannot parse
  run("test/WeiChenRompf2019/kcfa-worst-case-32.scm")
//  run("test/WeiChenRompf2019/meta-circ.scm") // TODO: eqv?
  run("test/WeiChenRompf2019/the-little-schemer/ch2.scm")
  run("test/WeiChenRompf2019/the-little-schemer/ch10.scm")
  run("test/WeiChenRompf2019/the-little-schemer/ch1.scm")
  run("test/WeiChenRompf2019/the-little-schemer/ch3.scm")
//  run("test/WeiChenRompf2019/the-little-schemer/ch7.scm") // atom?
//  run("test/WeiChenRompf2019/the-little-schemer/ch9.scm") // will-stop?
  run("test/WeiChenRompf2019/the-little-schemer/ch4.scm")
//  run("test/WeiChenRompf2019/the-little-schemer/ch5.scm") // rember*
//  run("test/WeiChenRompf2019/the-little-schemer/ch8.scm") // does not parse
//  run("test/WeiChenRompf2019/the-little-schemer/ch6.scm") // does not parse
  run("test/WeiChenRompf2019/fermat.scm")
  run("test/WeiChenRompf2019/rsa.scm")
//  run("test/WeiChenRompf2019/toplas98/lattice-processed.scm") // does not parse
//  run("test/WeiChenRompf2019/toplas98/splay.scm") // does not parse
//  run("test/WeiChenRompf2019/toplas98/lattice.scm") // does not parse
//  run("test/WeiChenRompf2019/toplas98/handle.scm") // does not parse
//  run("test/WeiChenRompf2019/scheme2java.scm") // char-alphabetic?
  run("test/grid.scm")
  run("test/church-2-num.scm")
//  run("test/SICP-compiler.scm") // TODO: map
  run("test/infinite-3.scm")
  run("test/nested-defines.scm")
  run("test/fact.scm")
  run("test/loop2.scm")
  run("test/kcfa3.scm")
  //  run("test/sigscheme/mem.scm") // TODO: slow with compiled
  run("test/sigscheme/takr.scm")
  run("test/sigscheme/rec.scm")
  run("test/sigscheme/arithint.scm")
  run("test/sigscheme/let-loop.scm")
  run("test/sigscheme/loop.scm")
  run("test/sigscheme/case.scm")
  run("test/gabriel/diviter.scm")
  run("test/gabriel/deriv.scm")
  run("test/gabriel/triangl.scm")
//  run("test/gabriel/boyer.scm") // TODO: slow
  run("test/gabriel/puzzle.scm")
  run("test/gabriel/takl.scm")
  run("test/gabriel/cpstak.scm")
  run("test/gabriel/divrec.scm")
  run("test/gabriel/dderiv.scm")
  run("test/gabriel/destruc.scm")
  run("test/gabriel/browse.scm")
  run("test/inc.scm")
  run("test/foo.scm")
  run("test/rotate.scm")
  run("test/quasiquoting-simple.scm")
  run("test/kcfa2.scm")
  run("test/kernighanvanwyk/ack.scm")
  run("test/primtest.scm")
  run("test/infinite-2.scm")
  run("test/church.scm")
//  run("test/gambit/sboyer.scm") // TODO: map
//  run("test/gambit/compiler.scm") // does not parse
  run("test/gambit/diviter.scm")
  run("test/gambit/tak.scm")
//  run("test/gambit/trav1.scm") // todo 
//  run("test/gambit/wc.scm") // does not parse
//  run("test/gambit/earley.scm") // list->vector
//  run("test/gambit/slatex.scm") // does not parse
  run("test/gambit/deriv.scm")
//  run("test/gambit/peval.scm") // TODO: slow
//  run("test/gambit/triangl.scm") // list->vector
//  run("test/gambit/nboyer.scm") // TODO: map
//  run("test/gambit/tail.scm") // read-char
  run("test/gambit/mazefun.scm")
//  run("test/gambit/scheme.scm") // does not parse
//  run("test/gambit/string.scm") // substring
  run("test/gambit/matrix.scm") 
//  run("test/gambit/ctak.scm") // call/cc
  run("test/gambit/primes.scm")
//  run("test/gambit/lattice.scm") // apply
  run("test/gambit/sum.scm")
//  run("test/gambit/puzzle.scm") // call/cc
  run("test/gambit/sumloop.scm")
  run("test/gambit/paraffins.scm")
//  run("test/gambit/fibc.scm") // call/cc
  run("test/gambit/nqueens.scm")
//  run("test/gambit/graphs.scm") // TODO: really slow
  run("test/gambit/destruc.scm")
  run("test/gambit/perm9.scm")
  run("test/gambit/array1.scm")
//  run("test/gambit/cat.scm") // read-char
//  run("test/gambit/browse.scm") // TODO: slow
//  run("test/scm2c.scm") // TODO: eqv
  run("test/mj09.scm")
  run("test/infinite-1.scm")
//  run("test/Streams.scm") // force
  run("test/ad/stack.scm")
  run("test/ad/queue.scm")
  run("test/ad/linear.scm")
//  run("test/ad/bfirst.scm") // create-graph
  run("test/ad/btree.scm")
//  run("test/ad/qsort.scm") // TODO: slow with compiled
  run("test/ad/mesort.scm")
  run("test/ad/RBtreeADT.scm")
  run("test/ad/bubsort.scm")
  run("test/ad/list.scm")
  run("test/ad/inssort.scm")
  run("test/ad/dict.scm")
//  run("test/ad/selsort.scm") // does not parse
  run("test/ad/abstrct.scm")
  run("test/ad/qstand.scm")
  run("test/ad/prioq.scm")
  run("test/ad/stspaceCODE.scm") // TODO: reverse
  run("test/ad/bst.scm")
//  run("test/ad/heap.scm") // does not parse
  run("test/ad/quick.scm")
  run("test/eta.scm")
  run("test/gcipd.scm")
  run("test/rsa.scm")
//  run("test/scm2java.scm") // list->string
  run("test/test.scm")
  run("test/count.scm")
*/
}
object Test extends App {
  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (cadr x) (car (cdr x)))"""))))
//  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (cddr x) (cdr (cdr x)))"""))))
//  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (caddr x) (car (cdr (cdr x))))"""))))
//  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (cadr x) (car (cdr x)))"""))))
//  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (assq k l)
//        (if (null? l)
//          #f
//         (if (equal? (caar l) k)
//           (car l)
//           (assq k (cdr l)))))"""))))
//  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (newline) #f)"""))))
//  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (display x) x)"""))))
//println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (append l1 l2)
//          (if (null? l1)
//              l2
//              (cons (car l1)
//                    (append (cdr l1) l2))))"""))))

  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (equal? a b)
          (or (eq? a b)
            (and (null? a) (null? b))
            (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))"""))))

  println(PrimCompiler.compile(toANF(SchemeParser.parse("""(define (member e l)
          (if (null? l)
            #f
            (if (equal? (car l) e)
              l
              (member e (cdr l)))))"""))))

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
        val argn = args.map(arg => (bodyToSource(arg), (- arg.idn.pos._1, - arg.idn.pos._2) )) // we set primitive positions to negative values so that it does not clash with user code
        if (!argn.forall(_._1.isInstanceOf[PrimSource.AE])) throw IllegalExpressionException(fc)
        val argv: PrimSource.Args = argn.map{case (AE(ae), pos) => (ae, pos)}.toArray
        bodyToSource(f) match { // TODO maybe check arity?
          case prim@AE(PrimSource.Var(Id(name))) if prm == name =>
            rec = true
            PrimSource.PrimCall(prim, argv, true, (- fc.idn.pos._1, - fc.idn.pos._2)) // negative position
            /*
          case AE(PrimSource.Var(Id(name))) if LatticeOperations.opNams.contains(name) =>
            if (LatticeOperations.stoNams.contains(name)) sto = true // TODO is this list sufficient?
            PrimSource.OpCall(LatticeOperations.ops.find(_.name == name).get, argv, fc.idn.pos) */
          case prim => PrimSource.PrimCall(prim, argv, false, (- fc.idn.pos._1, - fc.idn.pos._2)) // negative position
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
      case e =>
        println(s"Illegal exp: $e")
        throw IllegalExpressionException(e)
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
      //case PrimSource.OpCall(op, args, pos) =>
        //PrimTarget.LatticeOp(op, Args(args.map({ case (ae, pos) => (AExpToTarget(ae), pos) })), pos)
    }

    (toTarget(src._1), src._2)
  }

  /////////////////////////////
  // THIRD COMPILATION PHASE //
  /////////////////////////////

  def toScala(tar: (TE, PI)): String = {
    val PrimInfo(name, args, rec, sto) = tar._2
    def bodyStr: String = if (args.nonEmpty) {
      args.zipWithIndex.map({ case (a, i) =>
        s"val `${a}_pos` = args($i)._1\nval `$a` = args($i)._2"
      }).mkString("\n") ++ "\n" ++ tar._1.print(4)
    } else {
      tar._1.print(4)
    }

    def nonRecursive: String =
s"""object ${PrimTarget.scalaNameOf(name)} extends StoreOperation("$name", Some(${args.length})) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == ${args.length}) {
      { $bodyStr }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("$name", ${args.length}, args.length))
}"""
    def recursive: String =
s"""object ${PrimTarget.scalaNameOf(name)} extends SimpleFixpointPrimitive("$name", Some(${args.length})) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A, V], alloc: SchemeAllocator[A], $name: Args => MayFail[V, Error]): MayFail[V, Error] =
    if (args.length == ${args.length}) {
      $bodyStr
    } else MayFail.failure(PrimitiveArityError("$name", ${args.length}, args.length))
}"""
    def recursiveWithStore: String =
s"""object ${PrimTarget.scalaNameOf(name)} extends SimpleFixpointPrimitiveUsingStore("$name", Some(${args.length})) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == ${args.length}) {
      { $bodyStr }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("$name", ${args.length}, args.length))
}"""
    (rec, sto) match {
      case (true, false)  => recursiveWithStore // TODO: identification of store prims is not complete: member uses store primitives but is not detected as so. For now let's just either use nonRecursive, or recursiveWithStore
      case (false, false) => nonRecursive
      case (_, true)   => recursiveWithStore
      //case (false, true)  => recursiveWithStore // TODO: are there primitives of this kind? (Note: If enabled, also enable this in PrimTarget.scala).
    }
  }

}
