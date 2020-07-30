package scalaam.modular.scheme.ssmodconc

import scalaam.core._
import scalaam.language.CScheme._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.language.sexp
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.util.Annotations.mutable
import scalaam.util.SmartHash
import scalaam.util.benchmarks.Timeout

/**
 * Definition of Scheme components. In ModConc, Scheme components also double as thread identifiers.
 */
sealed trait SmallStepModConcComponent extends SmartHash with TID

// The main thread of the program.
case object MainComponent extends SmallStepModConcComponent {
  override def toString: String = "Main"
}

// A thread created by the program.
case class ThreadComponent[Ctx](exp: SchemeExp, env: Environment[Address], ctx: Ctx) extends SmallStepModConcComponent {
  override def toString: String = s"ThreadComponent($exp, $env, $ctx)"
}

/**
 * Provides a small-step ModConc semantics for a concurrent Scheme with threads.
 * Additionally supported primitives (upon R5RS): fork, join.
 */
trait SmallStepModConcSemantics extends ModAnalysis[SchemeExp]
                                   with GlobalStore[SchemeExp]
                                   with ReturnValue[SchemeExp]
                                   with SchemeSetup
                                   with ContextSensitiveComponents[SchemeExp] {

  type Env = Environment[Addr]

  // all allocations are context-insensitive
  def allocVar(id: Identifier, cmp: Component) = VarAddr(id,()) 
  def allocPtr(ptr: SchemeExp, cmp: Component) = PtrAddr(ptr,())

  //XXXXXXXXX//
  // PROGRAM //
  //XXXXXXXXX//
  
  type Exp  = SchemeExp
  type Exps = List[Exp]

  //XXXXXXXXXXXX//
  // COMPONENTS //
  //XXXXXXXXXXXX//

  type Component = SmallStepModConcComponent
  implicit def view(c: Component): SmallStepModConcComponent = c
  def expr(cmp: Component) = view(cmp) match {
    case MainComponent => program
    case ThreadComponent(exp, _, _) => exp
  }

  // The context of a ThreadComponent. TODO
  type ComponentContext = Unit

  lazy val initialComponent: SmallStepModConcComponent = MainComponent
  def newComponent(body: Exp, env: Env, ctx: ComponentContext): SmallStepModConcComponent = ThreadComponent(body, env, ctx)

  // Other required definitions.

  type ComponentContent = Option[(Exp, Env)]
  def content(cmp: Component): ComponentContent = view(cmp) match {
    case MainComponent => None
    case ThreadComponent(exp, env, _) => Some((exp, env))
  }
  def context(cmp: Component): Option[ComponentContext] = view(cmp) match {
    case MainComponent => None
    case ThreadComponent(_, _, ctx) => Some(ctx)
  }


  //XXXXXXXXXXXXXXXXXXXXXXXXXX//
  // INTRA-COMPONENT ANALYSIS //
  //XXXXXXXXXXXXXXXXXXXXXXXXXX//

  override def intraAnalysis(cmp: Component): SmallStepIntra
  trait SmallStepIntra extends IntraAnalysis with ReturnResultIntra  {

    //----------//
    // ANALYSIS //
    //----------//


    def analyze(timeout: Timeout.T = Timeout.none): Unit = {
      // Create an initial state based on the component's expression and environment, together with an empty continuation stack.
      val initialState = component match {
        case MainComponent => Eval(program, initialEnv, KEmpty)
        case ThreadComponent(exp, env, _) => Eval(exp, env, KEmpty)
      }

      @mutable var work: WorkList[State] = LIFOWorkList[State](initialState)
      @mutable var visited = Set[State]()
      @mutable var result  = lattice.bottom

      // Step states until a fixpoint/timeout is reached.
      while(work.nonEmpty && !timeout.reached) {
        val state = work.head
        work = work.tail
        state match {
          case Kont(vl, KEmpty) =>
            result = lattice.join(result,vl)
          case _ if !visited.contains(state) =>
            val successors = step(state)
            work = work.addAll(successors)
            // Clear the visited set when one of the global stores has changed.
            if (storeChanged || kstoreChanged) {
              visited = Set()
               storeChanged = false
              kstoreChanged = false
            }
            visited += state
          case _ => ()
        }
      }

      // Write the result value to the store.
      writeResult(result)
    }


    //-----------------------//
    // ENVIRONMENT AND STORE //
    //-----------------------//


    // Should not be used directly.
    def extendEnv(id: Identifier, addr: Addr, env: Env): Env =
      env.extend(id.name, addr)

    // Should not be used directly.
    def lookupEnv(id: Identifier, env: Env): Addr =
      env.lookup(id.name).getOrElse(throw new NoSuchElementException(s"$id in $env"))

    // Tracks changes to the global store.
    @mutable private var storeChanged: Boolean = false

    /** Defines a new variable: extends the store and environment. */
    private def define(variable: Identifier, vl: Value, env: Env): Env = {
      val addr = allocVar(variable, component)
      if (writeAddr(addr, vl)) storeChanged = true
      extendEnv(variable, addr, env)
    }

    /** Assigns a variable: updates the store at the corresponding address in environment. */
    private def assign(variable: Identifier, vl: Value, env: Env): Value = {
      if (writeAddr(lookupEnv(variable, env), vl)) storeChanged = true
      lattice.void
    }

    /** Looks up a variable in the store, given the current environment. */
    private def lookup(variable: Identifier, env: Env): Value =
      readAddr(lookupEnv(variable, env))


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


    //--------------//
    // STACK FRAMES //
    //--------------//


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
    case class      LetFrame(id: Identifier, todo: List[(Identifier, Exp)], done: List[(Identifier, Value)],
                                                                            body: Exps, env: Env)                  extends Frame
    case class  LetStarFrame(id: Identifier, todo: List[(Identifier, Exp)], body: Exps, env: Env)                  extends Frame
    case class   LetRecFrame(id: Identifier, todo: List[(Identifier, Exp)], body: Exps, env: Env)                  extends Frame
    case object    JoinFrame                                                                                       extends Frame             

    //-----------//
    // SEMANTICS //
    //-----------//


    sealed trait State

    case class Eval(expr: Exp, env: Env, stack: Stack) extends State { override def toString: String = s"Eval $expr" }
    case class           Kont(vl: Value, stack: Stack) extends State { override def toString: String = s"Kont $vl"   }

    // Computes the successor state(s) of a given state.
    private def step(state: State): Set[State] = state match {
      case Eval(exp, env, stack) => this.evaluate(exp, env, stack)
      case Kont(_, KEmpty) => throw new Exception("Cannot step a continuation state with an empty stack.")
      case Kont(vl, cc) => lookupKStore(cc).flatMap(k => continue(vl, k.frame, k.cc))
    }

    // Evaluates an expression (in the abstract).
    protected def evaluate(exp: Exp, env: Env, stack: Stack): Set[State] = exp match {
      // Single-step evaluation.
      case l@SchemeLambda(_, _, _)                 => Set(Kont(lattice.closure((l, env), None), stack))
      case l@SchemeVarArgLambda(_, _, _, _)        => Set(Kont(lattice.closure((l, env), None), stack))
      case SchemeValue(value, _)                   => Set(Kont(evalLiteralValue(value), stack))
      case SchemeVar(id)                           => Set(Kont(lookup(id, env), stack))

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
      case Nil               =>
        val env2 = done.reverse.foldLeft(env)((env, bnd) => define(bnd._1, bnd._2, env))
        evalSequence(body, env2, stack)
      case (id, exp) :: rest => Set(Eval(exp, env, extendKStore(exp, LetFrame(id, rest, done, body, env), stack)))
    }

    // Let*: bindings are made immediately (in continue).
    private def evalLetStar(todo: List[(Identifier, Exp)], body: Exps, env: Env, stack: Stack): Set[State] = todo match {
      case Nil               => evalSequence(body, env, stack)
      case (id, exp) :: rest => Set(Eval(exp, env, extendKStore(exp, LetStarFrame(id, rest, body, env), stack)))
    }

    // Letrec: bindings are made upfront and gradually updated (in continue).
    private def evalLetRec(bindings: List[(Identifier, Exp)], body: Exps, env: Env, stack: Stack): Set[State] = bindings match {
      case Nil               => evalSequence(body, env, stack)
      case (id, exp) :: rest =>
        val env2 = bindings.map(_._1).foldLeft(env)((env, id) => define(id, lattice.bottom, env))
        Set(Eval(exp, env2, extendKStore(exp, LetRecFrame(id, rest, body, env2), stack)))
    }

    private def evalNamedLet(name: Identifier, bindings: List[(Identifier, Exp)], body: Exps, env: Env, stack: Stack): Set[State] = {
      val (form, actu) = bindings.unzip
      val lambda = SchemeLambda(form, body, name.idn)
      val env2 = define(name, lattice.bottom, env)
      val clo = lattice.closure((lambda, env2), Some(name.name))
      assign(name, clo, env2)
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
      case SetFrame(variable, env)                 => Set(Kont(assign(variable, vl, env), stack)) // Returns bottom.
      case OperatorFrame(args, env, fexp)          => evalArgs(args, fexp, vl, List(), env, stack)
      case OperandsFrame(todo, done, env, f, fexp) => evalArgs(todo.tail, fexp, f, (todo.head, vl) :: done, env, stack)
      case LetFrame(id, todo, done, body, env)     => evalLet(todo, (id, vl) :: done, body, env, stack)
      case LetStarFrame(id, todo, body, env)       => evalLetStar(todo, body, define(id, vl, env), stack)
      case LetRecFrame(id, todo, body, env)        => assign(id, vl, env); continueLetRec(todo, body, env, stack)
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


    //--------------------//
    // EVALUATION HELPERS //
    //--------------------//

    // Evaluate literals by in injecting them in the scalaam.lattice.
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

    // Function application.
    private def apply(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp, Value)], stack: Stack): Set[State] = {
      // Application of primitives.
      def applyPrimitives(): Set[State] = {
        lattice.getPrimitives(fval).map(prm => Kont(
          prm.call(fexp, args, StoreAdapter, interpreterBridge) match {
            case MayFailSuccess((vlu,_))  => vlu
            case MayFailBoth((vlu,_),_)   => vlu
            case MayFailError(_)          => lattice.bottom
          },
          stack)
        ).toSet
      }

      // Application of closures.
      def applyClosures(): Set[State] = {
        lattice.getClosures(fval).flatMap({
          case ((SchemeLambda(prs,body,_),env), _) if prs.length == args.length =>
            val env2 = prs.zip(args.map(_._2)).foldLeft(env)({case (env, (f, a)) => define(f, a, env)})
            evalSequence(body, env2, stack)
          case ((SchemeVarArgLambda(prs,vararg,body,_),env), _) if prs.length <= args.length =>
            val (fixedArgs, varArgs) = args.splitAt(prs.length)
            val fixedArgVals = fixedArgs.map(_._2)
            val varArgVal = allocateList(varArgs)
            val env2 = define(vararg, varArgVal, prs.zip(fixedArgVals).foldLeft(env)({case (env, (f, a)) => define(f, a, env)}))
            evalSequence(body, env2, stack)
          case _ => Set()
        })
      }

      // Function application.
      if(args.forall(_._2 != lattice.bottom))
        applyClosures() ++ applyPrimitives()
      else
        Set(Kont(lattice.bottom, stack))
    }

    //--------------------//
    // ALLOCATION HELPERS //
    //--------------------//

    protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): Value = {
      val addr = allocPtr(pairExp, component)
      val pair = lattice.cons(car,cdr)
      writeAddr(addr,pair)
      lattice.pointer(addr)
    }

    protected def allocateList(elms: List[(SchemeExp,Value)]): Value = elms match {
      case Nil                => lattice.nil
      case (exp,vlu) :: rest  => allocateCons(exp)(vlu,allocateList(rest))
    }

    protected val interpreterBridge: SchemeInterpreterBridge[Addr] = new SchemeInterpreterBridge[Addr] {
      def pointer(exp: SchemeExp): Addr = allocPtr(exp, component)
      def currentThread = component
    }

    def allocateKAddr(e: Exp, cc: KA): KAddr
  }
}

trait KKallocModConc extends SmallStepModConcSemantics {

  val k: Int

  override def intraAnalysis(cmp: Component): KCFAIntra
  trait KCFAIntra extends IntraAnalysis with SmallStepIntra {
    def allocateKAddr(e: Exp, cc: KA): KAddr = cc match {
      case KEmpty   => KAddr( List(e).take(k))
      case KAddr(l) => KAddr((e :: l).take(k))
    }
  }
}