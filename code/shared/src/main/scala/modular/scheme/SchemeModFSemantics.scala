package scalaam.modular.scheme

import java.io.{BufferedWriter, File, FileWriter}

import scalaam.language.scheme.primitives.{SchemeAllocator, SchemePrimitive, SchemePrimitives}
import scalaam.core.Identity.Position
import scalaam.core._
import scalaam.modular._
import scalaam.language.scheme._
import scalaam.language.sexp
import scalaam.util._

/**
 * Base definitions for a Scheme MODF analysis.
 */
trait SchemeModFSemantics extends ModAnalysis[SchemeExp]
                            with GlobalStore[SchemeExp]
                            with ReturnValue[SchemeExp]
                            with ContextSensitiveComponents[SchemeExp]
//                            with InterceptCall[SchemeExp]
{

  def debug(): Unit = {
    println("Dependencies")
    println("--------------------")
    for { (dep, comp) <- deps } {
      println(s"$dep -> $comp")
    }
    println("Store")
    println("--------------------")
    for { (addr, v) <- store } {
      if (addr.isInstanceOf[ComponentAddr] && !addr.asInstanceOf[ComponentAddr].addr.isInstanceOf[PrmAddr])
        println(s"$addr -> $v")
    }
  }

  //XXXXXXXXXXXXXXXXXXXX//
  // LEXICAL ADDRESSING //
  //XXXXXXXXXXXXXXXXXXXX//

  // Ensure that the program is translated to use lexical addresses first!
  override lazy val program = {
    val originalProgram = super.program

    // Set up initial environment and install the primitives in the global store.
    primitives.allPrimitives.foreach { p =>
      val addr = ComponentAddr(PrmAddr(p.name))
      store += (addr -> lattice.primitive(p))
    }

    val initialBindings = primitives.allPrimitives.map(_.name).toSet
    SchemeLexicalAddresser.translateProgram(originalProgram, initialBindings)
  }

  // Local addresses are simply made out of lexical information.
  trait LocalAddr extends Address { def idn(): Identity;  def dropContext: Address = this }
  case class VarAddr(cmp: Component, id: Identifier) extends LocalAddr { def printable = true;  def idn(): Identity =  id.idn }
  case class PtrAddr[C](idn: Identity, c: C) extends LocalAddr { def printable = false; }
  case class CarAddr[C](idn: Identity, c: C) extends LocalAddr { def printable = false  }
  case class CdrAddr[C](idn: Identity, c: C) extends LocalAddr { def printable = false  }
  case class PrmAddr(nam: String)   extends LocalAddr { def printable = true;  def idn(): Identity = Identity.none }

  //XXXXXXXXXXXXXXXXX//
  // ABSTRACT VALUES //
  //XXXXXXXXXXXXXXXXX//

  // Abstract values come from a Scala-AM Scheme lattice (a type lattice).
  type Prim = SchemePrimitive[Value, Addr]
  implicit val lattice: SchemeLattice[Value, Addr, Prim, Component]
  val primitives: SchemePrimitives[Value, Addr]


  //XXXXXXXXXXXXXXXXXXXXXXXXX//
  // COMPONENTS AND CONTEXTS //
  //XXXXXXXXXXXXXXXXXXXXXXXXX//

  // In ModF, components are function calls in some context.

  // This abstract class is parameterised by the choice of two types of components:
  // * A MainComponent type representing the main function of the program.
  // * A CallComponent type representing function calls. CallComponents must have a parent pointer and lambda expression, contain a context and may contain a name.
  // The MainComponent should be unique and can hence be an object. CallComponents can be created using the `newCallComponent` function.
  // All components used together with this Scheme MODF analysis should be viewable as SchemeComponents.
  implicit def view(c: Component): SchemeComponent
  trait SchemeComponent { def body: SchemeExp; def name: Option[String] }
  trait MainComponent extends SchemeComponent {
    def body: SchemeExp = program
    override def toString: String = "main"
    def name = None
  }
  trait CallComponent extends SchemeComponent {
    // Requires a closure and a context and may contain a name.
    def nam: Option[String]
    def clo: lattice.Closure
    def ctx: ComponentContext
    // convenience accessors
    lazy val (lambda, parent) = clo
    lazy val body: SchemeExp = SchemeBody(lambda.body)
    override def toString: String = nam match {
      case None => s"λ@${lambda.idn} ($parent) [${ctx.toString}]"
      case Some(name) => s"$name ($parent) [${ctx.toString}]"
    }
    def name = nam
  }

  type ComponentContent = Option[lattice.Closure]
  def content(cmp: Component) = view(cmp) match {
    case _ : MainComponent => None
    case call: CallComponent => Some(call.clo)
  }
  def context(cmp: Component) = view(cmp) match {
    case _ : MainComponent => None
    case call: CallComponent => Some(call.ctx)
  }

  /* Return some context for pointer allocation */
  def getPtrCtx(cmp: Option[ComponentContext]): Any

  /** Creates a new component, given a closure, context and an optional name. */
  def newComponent(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext): Component

  /** Creates a new context given a closure, a list of argument values and the position of the call site. */
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]): ComponentContext

  //XXXXXXXXXXXXXXXXXXXXXXXXXX//
  // INTRA-COMPONENT ANALYSIS //
  //XXXXXXXXXXXXXXXXXXXXXXXXXX//

  // Extensions to the intraAnalysis.
  trait SchemeModFSemanticsIntra extends super.IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {
    // variable lookup: use the global store
    protected def lookupVariable(lex: LexicalRef): Value = readAddr(resolveAddr(lex))
    protected def    setVariable(lex: LexicalRef, vlu: Value): Unit = writeAddr(resolveAddr(lex), vlu)
    protected def defineVariable(cmp: Component, id: Identifier, vlu: Value): Unit = writeAddr(    VarAddr(cmp, id), vlu)
    // resolve a lexical address to the corresponding address in the store
    private def resolveAddr(lex: LexicalRef): Addr = lex match {
      case  LocalRef(identifier) => ComponentAddr(VarAddr(component, identifier))
      case GlobalRef(identifier) => ComponentAddr(VarAddr(initialComponent, identifier))
      case   PrimRef(      name) => ComponentAddr(PrmAddr(name))
      case NonLocalRef(identifier,scp) =>
        val cmp = resolveParent(component,scp)
        ComponentAddr(VarAddr(cmp, identifier))
    }
    @scala.annotation.tailrec
    private def resolveParent(cmp: Component, scp: Int): Component =
      if (scp == 0) { cmp } else resolveParent(view(cmp).asInstanceOf[CallComponent].parent, scp - 1)
    protected def applyFun(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)], cll: Position, cmp: Option[ComponentContext]): Value =
      splitArgs(args) { argsSplitted =>
        val fromClosures = applyClosures(fval,argsSplitted, cll, cmp)
        val fromPrimitives = applyPrimitives(fexp,fval,argsSplitted)
        lattice.join(fromClosures,fromPrimitives)
      }
    private def splitArgs(args: List[(SchemeExp,Value)])(fn: List[(SchemeExp,Value)] => Value): Value = args match {
      case Nil                      => fn(Nil)
      // TODO[minor]: use foldMap instead of foldLeft
      case (argExp,argVal) :: rest  =>
        lattice.split(argVal).foldLeft(lattice.bottom) { (acc,argSplitted) => lattice.join(acc,
          splitArgs(rest)(restSplitted => fn((argExp,argSplitted) :: restSplitted))
        )}
    }
    // TODO[minor]: use foldMap instead of foldLeft
    private def applyClosures(fun: Value, args: List[(SchemeExp,Value)], cll: Position, cmp: Option[ComponentContext]): Value = {
      val arity = args.length
      val closures = lattice.getClosures(fun)
      closures.foldLeft(lattice.bottom)((acc,clo) => lattice.join(acc, clo match {
        case (clo@(SchemeLambda(prs,_,_),_), nam) if prs.length == arity =>
          val argVals = args.map(_._2)
          // println(s"Allocating context with cmp context: $cmp, call: $cll")
          val context = allocCtx(nam, clo, argVals, cll, cmp)
          val component = newComponent(clo,nam,context)
          // storeParameters(component, prs)
          bindArgs(component, prs, argVals)
          call(component)
        case (clo@(SchemeVarArgLambda(prs,vararg,_,_),_), nam) if prs.length < arity =>
          val (fixedArgs,varArgs) = args.splitAt(prs.length)
          val fixedArgVals = fixedArgs.map(_._2)
          val varArgVal = allocateList(varArgs)
          val context = allocCtx(nam, clo, fixedArgVals :+ varArgVal, cll, cmp)
          val component = newComponent(clo,nam,context)
          bindArgs(component,prs,fixedArgVals)
          bindArg(component,vararg,varArgVal)
          // storeParameters(component, vararg :: prs)
          call(component)
        case _ => lattice.bottom
      }))
    }
    protected def allocateList(elms: List[(SchemeExp,Value)]): Value = elms match {
      case Nil                => lattice.nil
      case (exp,vlu) :: rest  => allocateCons(exp)(vlu,allocateList(rest))
    }
    protected def allocateCons(pairExp: SchemeExp)(car: Value, cdr: Value): Value = {
      val carAddr = allocAddr(CarAddr(pairExp.idn, ()))
      val cdrAddr = allocAddr(CdrAddr(pairExp.idn, ()))
      writeAddr(carAddr,car)
      writeAddr(cdrAddr,cdr)
      lattice.cons(carAddr,cdrAddr)
    }
    // protected def append(appendExp: SchemeExp)(l1: (SchemeExp, Value), l2: (SchemeExp, Value)): Value = {
    //   val appendPrim = lattice.primitive(primitives.PrimitiveDefs.Append)
    //   applyFun(appendExp, appendPrim, List(l1,l2))
    // }
    private def bindArg(component: Component, par: Identifier, arg: Value): Unit =
      writeAddr(VarAddr(component, par),arg,component)
    private def bindArgs(component: Component, pars: List[Identifier], args: List[Value]): Unit =
      pars.zip(args).foreach { case (par,arg) => bindArg(component,par,arg) }

    private val allocator: SchemeAllocator[Addr] = new SchemeAllocator[Addr] {
      def pointer(idn: Identity): Addr = {
        allocAddr(PtrAddr(idn, getPtrCtx(context(component))))
      }
      def carAddr(idn: Identity): Addr = allocAddr(CarAddr(idn, getPtrCtx(context(component))))
      def cdrAddr(idn: Identity): Addr = allocAddr(CdrAddr(idn, getPtrCtx(context(component))))
    }
    // TODO[minor]: use foldMap instead of foldLeft
    private def applyPrimitives(fexp: SchemeFuncall, fval: Value, args: List[(SchemeExp,Value)]): Value =
      lattice.getPrimitives(fval).foldLeft(lattice.bottom)((acc,prm) => lattice.join(acc, {
        // pre(prm.name, args.map(_._2))
//        println(s"apply ${prm.name} with ${args.map(_._2)}")
        val rs = prm.call(fexp.idn, args.map({ case (exp, arg) => (exp.idn, arg) }), StoreAdapter, allocator) match {
          case MayFailSuccess((vlu,_))  => vlu
          case MayFailBoth((vlu,_),_)   => vlu
          case MayFailError(_)          => lattice.bottom
        }
        // post(prm.name, rs)
        rs}
      ))
    // primitives glue code
    // TODO[maybe]: while this should be sound, it might be more precise to not immediately write every value update to the global store ...
    case object StoreAdapter extends Store[Addr,Value] {
      def lookup(a: Addr): Option[Value] = Some(readAddr(a))
      def extend(a: Addr, v: Value): Store[Addr, Value] = { writeAddr(a,v) ; this }
      // all the other operations should not be used by the primitives ...
      def content                               = throw new Exception("Operation not allowed!")
      def keys                                  = throw new Exception("Operation not allowed!")
      def restrictTo(a: Set[Addr])              = throw new Exception("Operation not allowed!")
      def forall(p: ((Addr, Value)) => Boolean) = throw new Exception("Operation not allowed!")
      def join(that: Store[Addr, Value])        = throw new Exception("Operation not allowed!")
      def subsumes(that: Store[Addr, Value])    = throw new Exception("Operation not allowed!")
    }
    // evaluation helpers
    protected def evalLiteralValue(literal: sexp.Value): Value = literal match {
      case sexp.ValueInteger(n)   => lattice.number(n)
      case sexp.ValueReal(r)      => lattice.real(r)
      case sexp.ValueBoolean(b)   => lattice.bool(b)
      case sexp.ValueString(s)    => lattice.string(s)
      case sexp.ValueCharacter(c) => lattice.char(c)
      case sexp.ValueSymbol(s)    => lattice.symbol(s)
      case sexp.ValueNil          => lattice.nil
      case _ => throw new Exception(s"Unsupported Scheme literal: $literal")
    }
    // The current component serves as the lexical environment of the closure.
    protected def newClosure(lambda: SchemeLambdaExp, name: Option[String]): Value =
      lattice.closure((lambda, component), name)

    // other helpers
    protected def conditional[M : Monoid](prd: Value, csq: => M, alt: => M): M = {
      val csqVal = if (lattice.isTrue(prd)) csq else Monoid[M].zero
      val altVal = if (lattice.isFalse(prd)) alt else Monoid[M].zero
      Monoid[M].append(csqVal,altVal)
    }
  }
}

trait StandardSchemeModFSemantics extends SchemeModFSemantics {
  // Components are just normal SchemeComponents, without any extra fancy features.
  // Hence, to view a component as a SchemeComponent, the component itself can be used.
  type Component = SchemeComponent
  implicit def view(cmp: Component): SchemeComponent = cmp

  // Definition of the initial component.
  case object Main extends MainComponent
  // Definition of call components.
  case class Call(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext) extends CallComponent

  lazy val initialComponent: SchemeComponent = Main
  def newComponent(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext): SchemeComponent = Call(clo,nam,ctx)
  def componentName(cmp: SchemeComponent): Option[String] = cmp match {
    case Main => None
    case Call(_, nam, _) => nam
  }
}


/*object InterceptCall {
  // By placing this in an object, we can perform timing measurements across analyses.
  var times: Map[String, List[ Long]] = Map().withDefaultValue(List())
  var primTime: Long = 0

  def init(): Unit = {
    times = Map().withDefaultValue(List())
    primTime = 0
  }

}

trait InterceptCall[Expr <: Expression] extends GlobalStore[Expr] {

  type Component

  import InterceptCall._

  var calls    :  Map[(String, List[Value]) , Value] =  Map()
  var timeStack:               List[ Long]           = List()
  var callStack: List[(String, List[Value])]         = List()

  var formalParameters: Map[Component, List[Addr]] = Map()

  def storeParameters(cmp: Component, pms: List[Identifier]): Unit = {
    formalParameters = formalParameters + (cmp -> pms.map(createAddr(cmp, _)))
  }

  def varAddr(cmp: Component, pm: Identifier): LocalAddr
  def createAddr(cmp: Component, pm: Identifier): Addr = ComponentAddr(varAddr(cmp, pm))

  def initPrimitiveBenchmarks(): Unit = {
    timeStack = List()
    times = Map().withDefaultValue(List())
    callStack = List()
    calls = Map()
    formalParameters = Map()
    primTime = 0
  }

  def readOutPrimitiveBenchmarks(): Unit = {
    assert(timeStack.isEmpty)
    assert(callStack.isEmpty)
    val prims = times.keySet.toList.sorted
    val avgTimes = times.view.mapValues(values => values.sum / values.length)
    println("*** Average call time ***")
    prims.foreach(p => println(s"$p: ${avgTimes(p)}"))
    println("*** Calls ***")
    println(calls.keySet.map(key => s"${key._1}: ${key._2} => ${calls(key)}").toList.sorted.mkString("\n"))
  }

  def toFile(suffix: String): Unit = {
    timeToFile(suffix)
    callToFile(suffix)
  }

  def timeToFile(suffix: String): Unit = {
    val timeFile = new BufferedWriter(new FileWriter(new File(s"benchOutput/time/$suffix")))
    times.keySet.toList.sorted.foreach({ prim =>
      val t = times(prim)
      val time = t.sum
      val calls = t.length
      timeFile.write(s"$prim: ${calls} calls, average time: ${time/calls}\n")
    })
    timeFile.flush()
    timeFile.close()
  }

  def callToFile(suffix: String): Unit = {
    val callFile = new BufferedWriter(new FileWriter(new File(s"benchOutput/call/$suffix")))
    callFile.write(calls.keySet.map(key => s"${key._1}: ${key._2} => ${calls(key)}").toList.sorted.mkString("\n"))
    callFile.flush()
    callFile.close()
  }

  def maybePre(name: String, cmp: Component): Unit = {
    if (criterium(name))
      maybePre(name, formalParameters(cmp).map(store.getOrElse(_, lattice.bottom)))
  }

  def pre(name: String, args: List[Value]): Unit = {
    callStack = (name, args) :: callStack
    timeStack = System.nanoTime() :: timeStack
    //System.err.println(s"call: $name(${args.mkString(",")})")
  }
  def post(name: String, result: Value): Unit = {
    val t1 = System.nanoTime()
    val t0 = timeStack.head
    timeStack = timeStack.tail
    if (timeStack.isEmpty) primTime = primTime + (t1 - t0)
    times = times + (name -> ((t1 - t0) :: times(name))) // ((t1 - t0) :: times(name)))
    val (`name`, args) = callStack.head
    callStack = callStack.tail
    calls = calls + ((name, args) -> result)
    //System.err.println(s"retn: $name(${args.mkString(",")}) => $result")
  }
  def criterium(name: String): Boolean = true //Primitives.names.contains(name)
  def maybePre(name: String, args: List[Value]): Unit = if (criterium(name)) pre(name, args)
  def maybePost(name: String, result: Value): Unit = if (criterium(name)) post(name, result)
}
 */
