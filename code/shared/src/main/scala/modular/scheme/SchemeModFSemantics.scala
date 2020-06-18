package scalaam.modular.scheme

import scalaam.language.scheme.primitives._
import scalaam.core.Position._
import scalaam.core._
import scalaam.language.scheme._
import scalaam.util.Monoid

/**
 * Base definitions for a Scheme MODF analysis.
 */
trait SchemeModFSemantics extends SchemeModXSemantics
{
  type Env = Component

  //XXXXXXXXXXXXXXXXXXXX//
  // LEXICAL ADDRESSING //
  //XXXXXXXXXXXXXXXXXXXX//

  // Ensure that the program is translated to use lexical addresses first!
  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    val initialBindings = primitives.allPrimitives.map(_.name).toSet
    SchemeLexicalAddresser.translateProgram(preludedProgram, initialBindings)
  }

  // Set up initial environment and install the primitives in the global store.
  primitives.allPrimitives.foreach { p =>
    val addr = ComponentAddr(initialComponent, PrmAddr(p.name))
    store += (addr -> lattice.primitive(p))
  }

  //XXXXXXXXXXXXXXXXXXXXXXXXX//
  // COMPONENTS AND CONTEXTS //
  //XXXXXXXXXXXXXXXXXXXXXXXXX//

  // In ModF, components are function calls in some context.
  // All components used together with this Scheme MODF analysis should be viewable as SchemeComponents.

  /** Returns the parent of a component, if any. */
  def componentParent(c: Component): Option[Component]

  /** Creates a new component, given a closure, context and an optional name. */
  def newComponent(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext): Component

  /** Creates a new context given a closure, a list of argument values and the position of the call site. */
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext

  //XXXXXXXXXXXXXXXXXXXXXXXXXX//
  // INTRA-COMPONENT ANALYSIS //
  //XXXXXXXXXXXXXXXXXXXXXXXXXX//

  // Extensions to the intraAnalysis.
  trait SchemeModFSemanticsIntra extends SchemeModXSemanticsIntra {

    // variable lookup: use the global store
    protected def lookupVariable(lex: LexicalRef): Value = readAddr(resolveAddr(lex))
    protected def    setVariable(lex: LexicalRef, vlu: Value): Unit = writeAddr(resolveAddr(lex), vlu)
    protected def defineVariable( id: Identifier, vlu: Value): Unit = writeAddr(    VarAddr( id), vlu)
    // resolve a lexical address to the corresponding address in the store
    private def resolveAddr(lex: LexicalRef): Addr = lex match {
      case  LocalRef(identifier) => ComponentAddr(component, VarAddr(identifier))
      case GlobalRef(identifier) => ComponentAddr(initialComponent, VarAddr(identifier))
      case   PrimRef(      name) => ComponentAddr(initialComponent, PrmAddr(name))
      case NonLocalRef(identifier,scp) =>
        val cmp = resolveParent(component,scp)
        ComponentAddr(cmp, VarAddr(identifier))
    }
    @scala.annotation.tailrec
    private def resolveParent(cmp: Component, scp: Int): Component =
      if (scp == 0) { cmp } else resolveParent(componentParent(cmp).get, scp - 1)

    // TODO[minor]: use foldMap instead of foldLeft
    protected def applyClosures(fun: Value, args: List[(SchemeExp,Value)], cll: Position, cmp: Component): Value = {
      val arity = args.length
      val closures = lattice.getClosures(fun)
      closures.foldLeft(lattice.bottom)((acc, clo) => lattice.join(acc, clo match {
        case (clo@(SchemeLambda(prs, _, _), _), nam) if prs.length == arity =>
          val argVals = args.map(_._2)
          val context = allocCtx(nam, clo, argVals, cll, cmp)
          val component = newComponent(clo, nam, context)
          bindArgs(component, prs, argVals)
          call(component)
        case (clo@(SchemeVarArgLambda(prs, vararg, _, _), _), nam) if prs.length <= arity =>
          val (fixedArgs, varArgs) = args.splitAt(prs.length)
          val fixedArgVals = fixedArgs.map(_._2)
          val varArgVal = allocateList(varArgs)
          val context = allocCtx(nam, clo, fixedArgVals :+ varArgVal, cll, cmp)
          val component = newComponent(clo, nam, context)
          bindArgs(component, prs, fixedArgVals)
          bindArg(component, vararg, varArgVal)
          call(component)
        case _ => lattice.bottom
      }))
    }

    private def bindArg(component: Component, par: Identifier, arg: Value): Unit =
      writeAddr(VarAddr(par),arg,component)
    private def bindArgs(component: Component, pars: List[Identifier], args: List[Value]): Unit =
      pars.zip(args).foreach { case (par,arg) => bindArg(component,par,arg) }

    // Evaluation of conditionals.
    protected def conditional[M : Monoid](prd: Value, csq: => M, alt: => M): M = {
      val csqVal = if (lattice.isTrue(prd)) csq else Monoid[M].zero
      val altVal = if (lattice.isFalse(prd)) alt else Monoid[M].zero
      Monoid[M].append(csqVal,altVal)
    }

    // The current component serves as the lexical environment of the closure.
    protected def newClosure(lambda: SchemeLambdaExp, name: Option[String]): Value =
      lattice.closure((lambda, component), name)
  }
}