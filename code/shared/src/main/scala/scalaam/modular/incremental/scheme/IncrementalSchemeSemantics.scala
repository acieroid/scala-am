package scalaam.modular.incremental.scheme

import scalaam.language.change.CodeChange
import scalaam.language.change.CodeVersion._
import scalaam.modular.scheme.modf._
import scalaam.modular.scheme._
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme.ssmodconc.SmallStepModConcSemantics

trait IncrementalSchemeSemantics {
  var version: Version = Old
}

trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics {
  trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntra {
    override protected def eval(exp: SchemeExp, env: Env): Value = exp match {
      case CodeChange(old, nw, _) => if (version == Old) eval(old, env) else eval(nw, env)
      case _                      => super.eval(exp, env)
    }
  }
}

abstract class IncrementalSimpleSchemeModFAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                                      with StandardSchemeModFComponents
                                                                      with SchemeModFSemantics
                                                                      with IncrementalSchemeModFBigStepSemantics {
  override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra
}

/*
trait IncrementalSchemeModConcSmallStepSemantics extends SmallStepModConcSemantics with IncrementalSchemeSemantics {
  trait IncrementalSmallStepIntra extends SmallStepIntra {
    override protected def evaluate(exp: Exp, env: Env, stack: Stack): Set[State] = exp match {
      case CodeChange(old, nw, _) =>if (version == Old) Set(Eval(old, env, stack)) else Set(Eval(nw, env, stack))
      case _                      => super.evaluate(exp, env, stack)
    }
  }
}

abstract class IncrementalSimpleSchemeModConcAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                                      with SchemeModFSemantics
                                                                      with IncrementalSchemeModConcSmallStepSemantics {
  override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra
}
*/

/*
/** Semantics for an incremental Scheme MODF analysis. */
trait IncrementalSchemeModFSemantics extends IncrementalModAnalysis[SchemeExp] with SchemeModFSemantics {

  // Every component holds a pointer to the corresponding lexical module.
  type ComponentData = SchemeModFComponent[ComponentContext,Addr]

  def module(cmp: Component) = body(cmp).idn 

  lazy val initialComponent: Component = ref(Main) // Need init to initialize reference bookkeeping information upfront.
  def newComponent(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext): Component = ref(Call(clo,nam,ctx)) // A new pointer is only created when the component was not registered yet.

  // When a new program is set, we need to use the lexically addressed version!
  override def setProgram(newProgram: SchemeExp): Unit = prog = {
    val originalProgram = newProgram
    val initialBindings = primitives.allPrimitives.map(_.name).toSet
    SchemeLexicalAddresser.translateProgram(originalProgram, initialBindings)
  }

  /*
   * To update a component, a new component containing the new lambda expression is created (the expression within a component always must be a lambda expression).
   * Only call components need to be updated here, since the main component is treated differently (the entire program is stored in a variable which need to be updated).
   * A call component consists out of a closure (lambda + env), an optional name and a context. These are updated as follows:
   * * The lambda expression is replaced by the new expression for the component.
   * * The environment is a pointer to the enclosing component. This is copied. Assumption: the component has not changed scope.
   * * The name is copied as well. TODO change if needed?
   * * The context is copied as well, since this is specific to this component.
   */
  def updateComponent(cmpPtr: Component, exp: SchemeExp): Unit = view(cmpPtr) match {
    case Main => // Do nothing, program is set by setProgram.
    case Call((_, env), nam, ctx) =>
      exp match {
        case e: SchemeLambdaExp =>
          // To update a component, create a new component with
          // 1) the updated version of the lambda expression, and
          // 2) an updated version of the analysis context.
          val newCall = Call((e, env), nam, updateComponentContext(ctx))
          val newCmp = newComponent(newCall)
          updateCPtr(deref(newCmp), cmpPtr)
        case _ => throw new Exception("A module must contain a lambda expression.")
      }
  }

  def componentExpression(cmpPtr: Component): SchemeExp = body(cmpPtr)

  /**
   * Updates local addressses by replacing the identity (VarAddr), or by updating the corresponding expression.
   * If no corresponding identity is present, None is returned.
   * @param addr            The address to be updated.
   * @param newIdentities   A map from old to new identities.
   * @param newExpressions  A map from new identities to corresponding expressions.
   * @return
   */
  def updateLocalAddress(addr: scalaam.core.Address, newIdentities: Map[OldIdn, NewIdn], newExpressions: Map[NewIdn, SchemeExp]): Option[scalaam.core.Address] = addr match {
    case VarAddr(id) =>  newIdentities.get(id.idn).map(idn => VarAddr(id.copy(idn = idn)))
    case PtrAddr(exp) => newIdentities.get(exp.idn).flatMap(idn => newExpressions.get(idn).map(PtrAddr))
    case PrmAddr(nam) => Some(PrmAddr(nam))
  }

  def updateComponentContext(ctx: ComponentContext): ComponentContext = ???
}

 */