package scalaam.modular.scheme

import scalaam.language.CScheme._
import scalaam.core._
import scalaam.language.CScheme._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.language.sexp
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.modular._
import scalaam.util.Annotations.mutable
import scalaam.util.SmartHash
import scalaam.util.benchmarks.Timeout

trait SmallStepModConcSemantics extends ModAnalysis[SchemeExp]
                                   with DedicatedGlobalStore[SchemeExp]
                                   with ReturnValue[SchemeExp]
                                   with ContextSensitiveComponents[SchemeExp] {
  
  type Exp  = SchemeExp
  type Exps = List[Exp]

  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    CSchemeUndefiner.undefine(List(preludedProgram))
  }

  //XXXXXXXXXXX//
  // ADDRESSES //
  //XXXXXXXXXXX//

  // TODO: incorporate another addressing scheme... (add context).

  // Local addresses are simply made out of lexical information.
  sealed trait LocalAddr extends Address {
    def idn(): Identity
    override def toString: String = this match {
      case VarAddr(id)  => s"var ($id)"
      case PtrAddr(exp) => s"ptr (${exp.idn})"
      case PrmAddr(nam) => s"prm ($nam)"
    }
  }
  case class VarAddr(id: Identifier)  extends LocalAddr { def printable = true;  def idn(): Identity =  id.idn }
  case class PtrAddr(exp: SchemeExp)  extends LocalAddr { def printable = false; def idn(): Identity =  exp.idn }
  case class PrmAddr(nam: String)     extends LocalAddr { def printable = true;  def idn(): Identity = Identity.none }

  //override def intraAnalysis(component: Component): IntraAnalysis = new SmallStepIntra(component)

  //XXXXXXXXXXXXXXXXX//
  // ABSTRACT VALUES //
  //XXXXXXXXXXXXXXXXX//

  // Abstract values come from a Scala-AM Scheme lattice (a type lattice).
  type Prim = SchemePrimitive[Value, Addr]
  implicit val lattice: SchemeLattice[Value, Addr, Prim, Env]
  lazy val primitives: SchemePrimitives[Value, Addr] = new SchemeLatticePrimitives()

  case class Env(data: Map[String, Addr]) {
    override def toString: String = s"ENV{${data.keySet.toList.filterNot(emptyEnv.data.keySet.contains).sorted.mkString(" ")} <prims>}"
  }

  // The empty environment. Binds all primitives in the store upon initialisation (hence why this is a val and not put in the intra-analysis).
  val emptyEnv: Env = {
    var data = Map[String, Addr]()
    // Set up initial environment and install the primitives in the global store.
    primitives.allPrimitives.foreach { p =>
      val addr = GlobalAddr(PrmAddr(p.name))
      store += (addr -> lattice.primitive(p))
      data = data + (p.name -> addr)
    }
    Env(data)
  }

  //XXXXXXXXXXXX//
  // COMPONENTS //
  //XXXXXXXXXXXX//

  //implicit def view(c: Component): SchemeComponent
  trait SchemeComponent extends SmartHash with TID { def body: SchemeExp; def env: Env } // Scheme components now are thread identifiers.

  //def newComponent(body: Exp, ctx: ComponentContext): SchemeComponent

  type Component = SchemeComponent
  implicit def view(c: Component): SchemeComponent = c

  // The main process of the program.
  case object MainComponent extends SchemeComponent {
    def body: Exp = program
    def  env: Env = emptyEnv
    override def toString: String = "Main"
  }

  // The context of a component. TODO
  type ComponentContext = Unit

  // A process created by the program.
  case class ThreadComponent(body: Exp, env: Env, ctx: ComponentContext) extends SchemeComponent

  lazy val initialComponent: SchemeComponent = MainComponent
  def newComponent(body: Exp, env: Env, ctx: ComponentContext): SchemeComponent = ThreadComponent(body, env, ctx)

  // Other required definitions.

  type ComponentContent = Option[(Exp, Env)]
  def content(cmp: Component): ComponentContent = view(cmp) match {
    case MainComponent => None
    case p: ThreadComponent => Some((p.body, p.env))
  }
  def context(cmp: Component): Option[ComponentContext] = view(cmp) match {
    case MainComponent => None
    case p: ThreadComponent => Some(p.ctx)
  }

  //XXXXXXXXXXXXXXXXXXXXXXXXXX//
  // INTRA-COMPONENT ANALYSIS //
  //XXXXXXXXXXXXXXXXXXXXXXXXXX//

  trait SmallStepIntra extends IntraAnalysis with GlobalStoreIntra with ReturnResultIntra  {

    //----------//
    // ANALYSIS //
    //----------//

    def analyze(timeout: Timeout.T = Timeout.none): Unit = {
      val initialState = Eval(component.body, component.env, KEmpty)
      var work: WorkList[State] = LIFOWorkList[State](initialState)
      var visited = Set[State]()
      var result  = lattice.bottom
      while(work.nonEmpty && !timeout.reached) {
        val state = work.head
        work = work.tail
        state match {
          case Kont(vl, KEmpty) =>
            result = lattice.join(result,vl)
          case _ if !visited.contains(state) =>
            val successors = step(state)
            if (storeChanged || kstoreChanged) {
              visited = Set()
              storeChanged = false
              kstoreChanged = false
            }
            work = work.addAll(successors)
            visited += state
          case _ => ()
        }
      }
      writeResult(result)
    }

    //-------------//
    // ENVIRONMENT //
    //-------------//

    def extendEnv(id: Identifier, addr: LocalAddr, env: Env): Env =
      Env(env.data + (id.name -> allocAddr(addr)))

    def lookupEnv(id: Identifier, env: Env): Addr = 
      env.data.getOrElse(id.name, throw new NoSuchElementException(s"$id in $env"))

    //-------//
    // STORE //
    //-------//

    // Tracks changes to the global store.
    @mutable private var storeChanged: Boolean = false

    private def bind(variable: Identifier, vl: Value, env: Env): Env = {
      val addr = VarAddr(variable)
      if (writeAddr(addr, vl)) storeChanged = true
      extendEnv(variable, addr, env)
    }

    // TODO: better call this assign or something ...
    private def rebind(variable: Identifier, vl: Value, env: Env): Value = {
      if (writeAddr(lookupEnv(variable, env), vl)) storeChanged = true
      lattice.bottom
    }

    //--------//
    // KSTORE //
    //--------//

    // TODO: improve this and abstract better.

    // Continuation addresses.
    sealed trait KA extends SmartHash
    case class KAddr(stack: List[Exp]) extends KA
    case object KEmpty extends KA

    // Continuation store.
    private case class K(frame: Frame, cc: KA)
    private type KStore = Map[KA, Set[K]]

    @mutable private var ks: KStore = Map() // KStore private to this component!
    @mutable private var kstoreChanged: Boolean = false // Tracks changes to the continuation store.

    // Operations on continuation store.
    private def lookupKStore(cc: KA): Set[K] = ks.getOrElse(cc, Set())
    private def extendKStore(e: Exp, frame: Frame, cc: KA): KA = {
      val kaddr = allocateKAddr(e, cc)
      val knt = K(frame, cc)
      val old = lookupKStore(kaddr)
      if (!old.contains(knt)) kstoreChanged = true
      ks += kaddr -> (old + knt)
      kaddr
    }

    //-------//
    // STACK //
    //-------//

    sealed trait Frame
    type Stack = KA

    case class SequenceFrame(exps: Exps, env: Env)                                                                 extends Frame
    case class       IfFrame(cons: Exp, alt: Exp, env: Env)                                                        extends Frame
    case class      AndFrame(exps: Exps, env: Env)                                                                 extends Frame
    case class       OrFrame(exps: Exps, env: Env)                                                                 extends Frame
    case class  PairCarFrame(cdr: SchemeExp, env: Env, pair: Exp)                                                  extends Frame
    case class  PairCdrFrame(car: Value, pair: Exp)                                                                extends Frame
    case class      SetFrame(variable: Identifier, env: Env)                                                       extends Frame
    case class OperatorFrame(args: Exps, env: Env, fexp: SchemeFuncall)                                            extends Frame
    case class OperandsFrame(todo: Exps, done: List[(Exp, Value)], env: Env, f: Value, fexp: SchemeFuncall)        extends Frame // "todo" also contains the expression currently evaluated.
    case class      LetFrame(id: Identifier, todo: List[(Identifier, Exp)], done: List[(Identifier, Value)], body: Exps, env: Env) extends Frame
    case class  LetStarFrame(id: Identifier, todo: List[(Identifier, Exp)], body: Exps, env: Env)                                  extends Frame
    case class   LetRecFrame(id: Identifier, todo: List[(Identifier, Exp)], body: Exps, env: Env)                                  extends Frame
    case object    JoinFrame                                                                                       extends Frame

    //-----------//
    // SEMANTICS //
    //-----------//

    sealed trait State

    case class Eval(expr: Exp, env: Env, stack: Stack) extends State { override def toString: String = s"Eval $expr" }
    case class           Kont(vl: Value, stack: Stack) extends State { override def toString: String = s"Kont $vl"   }

    // Computes the successor state(s) of a given state.
    private def step(state: State): Set[State] = state match {
      case Eval(exp, env, stack) => evaluate(exp, env, stack)
      case Kont(_, KEmpty) => throw new Exception("Cannot step a continuation state with an empty stack.")
      case Kont(vl, cc) => lookupKStore(cc).flatMap(k => continue(vl, k.frame, k.cc))
    }

    // Evaluates an expression (in the abstract).
    private def evaluate(exp: Exp, env: Env, stack: Stack): Set[State] = exp match {
      // Single-step evaluation.
      case l@SchemeLambda(_, _, _)                 => Set(Kont(lattice.closure((l, env), None), stack))
      case l@SchemeVarArgLambda(_, _, _, _)        => Set(Kont(lattice.closure((l, env), None), stack))
      case SchemeValue(value, _)                   => Set(Kont(evalLiteralValue(value), stack))
      case SchemeVar(id)                           => Set(Kont(readAddr(lookupEnv(id, env)), stack))

      // Multi-step evaluation.
      case c@SchemeFuncall(f, args, _)             => Set(Eval(f, env, extendKStore(f, OperatorFrame(args, env, c), stack)))
      case e@SchemePair(car, cdr, _)               => Set(Eval(car, env, extendKStore(car, PairCarFrame(cdr, env, e), stack)))
      case SchemeSet(variable, value, _)           => Set(Eval(value, env, extendKStore(value, SetFrame(variable, env), stack)))
      case SchemeAnd(Nil, _)                       => Set(Kont(lattice.bool(true), stack))
      case SchemeAnd(e :: es, _)                   => evalAnd(e,es,env,stack)
      case SchemeBegin(exps, _)                    => evalSequence(exps, env, stack)
      case SchemeIf(cond, cons, alt, _)            => evalIf(cond, cons, alt, env, stack)
      case SchemeLet(bindings, body, _)            => evalLet(bindings, List(), body, env, stack)
      case SchemeLetrec(bindings, body, _)         => evalLetRec(bindings, body, env, stack)
      case SchemeLetStar(bindings, body, _)        => evalLetStar(bindings, body, env, stack)
      case SchemeNamedLet(name, bindings, body, _) => evalNamedLet(name, bindings, body, env, stack)
      case SchemeOr(exps, _)                       => evalOr(exps, env, stack)
      case SchemeSplicedPair(_, _, _)              => throw new Exception("Splicing not supported.")

      // Multithreading.
      case CSchemeFork(body, _)                    => evalFork(body, env, stack)
      case CSchemeJoin(body, _)                    => Set(Eval(body, env, extendKStore(body, JoinFrame, stack)))

      // Unexpected cases.
      case e                                       => throw new Exception(s"evaluate: unexpected expression type: ${e.label}.")
    }

    private def evalSequence(exps: Exps, env: Env, stack: Stack): Set[State] = exps match {
      case e ::  Nil => Set(Eval(e, env, stack))
      case e :: exps => Set(Eval(e, env, extendKStore(e, SequenceFrame(exps, env), stack)))
      case Nil       => throw new Exception("Empty body should have been disallowed by compiler.")
    }

    private def evalIf(cond: Exp, cons: Exp, alt: Exp, env: Env, stack: Stack): Set[State] =
      Set(Eval(cond, env, extendKStore(cond, IfFrame(cons, alt, env), stack)))

    private def evalAnd(first: SchemeExp, rest: List[SchemeExp], env: Env, stack: Stack): Set[State] =
      if (rest.isEmpty) {
        Set(Eval(first, env, stack))
      } else {
        val frm = AndFrame(rest,env)
        Set(Eval(first, env, extendKStore(first, frm, stack)))
      }

    private def evalOr(exps: Exps, env: Env, stack: Stack): Set[State] = exps match {
      case Nil       => Set(Kont(lattice.bool(false), stack))
      case e :: exps => Set(Eval(e, env, extendKStore(e, OrFrame(exps, env), stack)))
    }

    private def evalArgs(todo: Exps, fexp: SchemeFuncall, f: Value, done: List[(Exp, Value)], env: Env, stack: Stack): Set[State] = todo match {
      case Nil             => apply(fexp, f, done.reverse, stack) // Function application.
      case args@(arg :: _) => Set(Eval(arg, env, extendKStore(arg, OperandsFrame(args, done, env, f, fexp), stack)))
    }

    // Let: bindings are made after all expressions are evaluated.
    private def evalLet(todo: List[(Identifier, Exp)], done: List[(Identifier, Value)], body: Exps, env: Env, stack: Stack): Set[State] = todo match {
      case Nil                 =>
        val env2 = done.reverse.foldLeft(env)((env, bnd) => bind(bnd._1, bnd._2, env))
        evalSequence(body, env2, stack)
      case (id, exp) :: rest    => Set(Eval(exp, env, extendKStore(exp, LetFrame(id, rest, done, body, env), stack)))
    }

    // Let*: bindings are made immediately (in continue).
    private def evalLetStar(todo: List[(Identifier, Exp)], body: Exps, env: Env, stack: Stack): Set[State] = todo match {
      case Nil                  => evalSequence(body, env, stack)
      case (id, exp) :: rest    => Set(Eval(exp, env, extendKStore(exp, LetStarFrame(id, rest, body, env), stack)))
    }

    // Letrec: bindings are made upfront and gradually updated (in continue). TODO is this correct in the abstract? => it's letrec* semantics here, but that's ok (we also do that in ModF)
    private def evalLetRec(bindings: List[(Identifier, Exp)], body: Exps, env: Env, stack: Stack): Set[State] = bindings match {
      case Nil                 => evalSequence(body, env, stack)
      case (id, exp) :: rest =>
        val env2 = bindings.map(_._1).foldLeft(env)((env, id) => bind(id, lattice.bottom, env))
        Set(Eval(exp, env2, extendKStore(exp, LetRecFrame(id, rest, body, env2), stack)))
    }

    private def evalNamedLet(name: Identifier, bindings: List[(Identifier, Exp)], body: Exps, env: Env, stack: Stack): Set[State] = {
      val (form, actu) = bindings.unzip
      val lambda = SchemeLambda(form, body, name.idn)
      val env2 = bind(name, lattice.bottom, env)
      val clo = lattice.closure((lambda, env2), Some(name.name))
      rebind(name, clo, env2)
      val call = SchemeFuncall(lambda, actu, name.idn)
      evalArgs(actu, call, clo, Nil, env, stack)
    }

    private def evalFork(body: Exp, env: Env, stack: Stack): Set[State] = {
      val component = newComponent(body, env, ())
      spawn(component)
      Set(Kont(lattice.thread(component), stack)) // Returns the TID of the newly created thread.
    }

    // Continues with a value (in the abstract).
    private def continue(vl: Value, frame: Frame, stack: Stack): Set[State] = frame match {
      case SequenceFrame(exps, env)                => evalSequence(exps, env, stack)
      case IfFrame(cons, alt, env)                 => conditional(vl, Eval(cons, env, stack), Eval(alt,  env, stack))
      case AndFrame(exps, env)                     => conditional(vl, evalAnd(exps.head, exps.tail, env, stack), Set[State](Kont(lattice.bool(false), stack)))
      case OrFrame(exps, env)                      => conditional(vl, Set[State](Kont(vl, stack)), evalOr(exps, env, stack))
      case PairCarFrame(cdr, env, pair)            => Set(Eval(cdr, env, extendKStore(cdr, PairCdrFrame(vl, pair), stack)))
      case PairCdrFrame(carv, pair)                => Set(Kont(allocateCons(pair)(carv, vl), stack))
      case SetFrame(variable, env)                 => Set(Kont(rebind(variable, vl, env), stack)) // Returns bottom.
      case OperatorFrame(args, env, fexp)          => evalArgs(args, fexp, vl, List(), env, stack)
      case OperandsFrame(todo, done, env, f, fexp) => evalArgs(todo.tail, fexp, f, (todo.head, vl) :: done, env, stack)
      case LetFrame(id, todo, done, body, env)     => evalLet(todo, (id, vl) :: done, body, env, stack)
      case LetStarFrame(id, todo, body, env)       => evalLetStar(todo, body, bind(id, vl, env), stack)
      case LetRecFrame(id, todo, body, env)        => rebind(id, vl, env); continueLetRec(todo, body, env, stack)
      case JoinFrame                               => lattice.getThreads(vl).map(tid => Kont(readResult(tid.asInstanceOf[Component]), stack)) //TODO: parameterize ModularLattice with type of TID to avoid asInstanceOf here
    }

    private def conditional(value: Value, t: State, f: State): Set[State] = conditional(value, Set(t), Set(f))
    private def conditional(value: Value, t: Set[State], f: Set[State]): Set[State] = {
      val tr = if (lattice.isTrue(value)) t else Set[State]()
      if (lattice.isFalse(value)) tr ++ f else tr
    }

    private def continueLetRec(todo: List[(Identifier, Exp)], body: Exps, env: Env, stack: Stack): Set[State] = todo match {
      case Nil                => evalSequence(body, env, stack)
      case (id, exp) :: rest  => Set(Eval(exp, env, extendKStore(exp, LetRecFrame(id, rest, body, env), stack)))
    }

    //====================//
    // EVALUATION HELPERS //
    //====================//

    // primitives glue code
    // TODO[maybe]: while this should be sound, it might be more precise to not immediately write every value update to the global store ...
    private case object StoreAdapter extends Store[Addr,Value] {
      def lookup(a: Addr): Option[Value] = Some(readAddr(a))
      def extend(a: Addr, v: Value): Store[Addr, Value] = { writeAddr(a,v) ; this }
      // all the other operations should not be used by the primitives ...
      def content                               = throw new Exception("Operation not allowed!")
      def forall(p: ((Addr, Value)) => Boolean) = throw new Exception("Operation not allowed!")
      def join(that: Store[Addr, Value])        = throw new Exception("Operation not allowed!")
      def keys                                  = throw new Exception("Operation not allowed!")
      def restrictTo(a: Set[Addr])              = throw new Exception("Operation not allowed!")
      def subsumes(that: Store[Addr, Value])    = throw new Exception("Operation not allowed!")
    }

    // Evaluate literals by in injecting them in the lattice.
    private def evalLiteralValue(literal: sexp.Value): Value = literal match {
      case sexp.ValueBoolean(b)   => lattice.bool(b)
      case sexp.ValueCharacter(c) => lattice.char(c)
      case sexp.ValueInteger(n)   => lattice.number(n)
      case sexp.ValueNil          => lattice.nil
      case sexp.ValueReal(r)      => lattice.real(r)
      case sexp.ValueString(s)    => lattice.string(s)
      case sexp.ValueSymbol(s)    => lattice.symbol(s)
      case _ => throw new Exception(s"Unsupported Scheme literal: $literal")
    }

    private def applyPrimitives(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)], stack: Stack): Set[State] = {
      lattice.getPrimitives(fval).map(prm => Kont(
        prm.call(fexp, args, StoreAdapter, allocator) match {
          case MayFailSuccess((vlu,_))  => vlu
          case MayFailBoth((vlu,_),_)   => vlu
          case MayFailError(_)          => lattice.bottom
        },
        stack)
      ).toSet
    }

    private def applyClosures(fun: Value, args: List[(SchemeExp,Value)], stack: Stack): Set[State] = {
      val arity = args.length
      lattice.getClosures(fun).flatMap({
        case ((SchemeLambda(prs,body,_),env), _) if prs.length == arity =>
          val env2 = prs.zip(args.map(_._2)).foldLeft(env)({case (env, (f, a)) => bind(f, a, env)})
          evalSequence(body, env2, stack)
        case ((SchemeVarArgLambda(prs,vararg,body,_),env), _) if prs.length <= arity =>
          val (fixedArgs, varArgs) = args.splitAt(prs.length)
          val fixedArgVals = fixedArgs.map(_._2)
          val varArgVal = allocateList(varArgs)
          val env2 = bind(vararg, varArgVal, prs.zip(fixedArgVals).foldLeft(env)({case (env, (f, a)) => bind(f, a, env)}))
          evalSequence(body, env2, stack)
        case _ => Set()
      })
    }

    // Function application.
    private def apply(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)], stack: Stack): Set[State] =
      if(args.forall(_._2 != lattice.bottom))
        applyClosures(fval,args, stack) ++ applyPrimitives(fexp, fval, args, stack)
      else
        Set(Kont(lattice.bottom, stack))

    //====================//
    // ALLOCATION HELPERS //
    //====================//

    protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): Value = {
      val addr = allocAddr(PtrAddr(pairExp))
      val pair = lattice.cons(car,cdr)
      writeAddr(addr,pair)
      lattice.pointer(addr)
    }

    protected def allocateList(elms: List[(SchemeExp,Value)]): Value = elms match {
      case Nil                => lattice.nil
      case (exp,vlu) :: rest  => allocateCons(exp)(vlu,allocateList(rest))
    }

    val allocator: SchemeAllocator[Addr] = new SchemeAllocator[Addr] {
      def pointer(exp: SchemeExp): Addr = allocAddr(PtrAddr(exp))
    }

    def allocateKAddr(e: Exp, cc: KA): KAddr
  }
}

trait KAExpressionContext extends SmallStepModConcSemantics {

  override def intraAnalysis(component: Component): IntraAnalysis = new AllocIntra(component)

  class AllocIntra(cmp: Component) extends IntraAnalysis(cmp) with SmallStepIntra {

    def allocateKAddr(e: Exp, cc: KA): KAddr = cc match {
      case KEmpty   => KAddr(List(e))
      case KAddr(l) => KAddr((e :: l).take(1))
    }

  }
}