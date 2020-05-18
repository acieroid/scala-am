package scalaam.modular.incremental.scheme

import scalaam.core._
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.language.scheme._
import scalaam.modular.scheme.StandardSchemeComponents

/** Semantics for an incremental Scheme MODF analysis. */
trait IncrementalSchemeModFSemantics extends IncrementalModAnalysis[SchemeExp] with StandardSchemeComponents {

  // Every component holds a pointer to the corresponding lexical module.
  trait ComponentData extends SchemeComponent with LinkedComponent {
    def mod: Module = body.idn
  }

  // Definition of the initial component.
  case object Main extends ComponentData with MainComponent
  // Definition of call components.
  case class Call(clo: lattice.Closure, nam: Option[String], ctx: ComponentContext) extends ComponentData with CallComponent

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
  def updateComponent(cmpPtr: Component, exp: SchemeExp): Unit = deref(cmpPtr) match {
    case Main => // Do nothing, program is set by setProgram.
    case Call((_, parent), nam, ctx) =>
      exp match {
        case e: SchemeLambdaExp =>
          // To update a component, create a new component with
          // 1) the updated version of the lambda expression, and
          // 2) an updated version of the analysis context.
          updateCPtr(newComponent((e, parent), nam, updateComponentContext(ctx)), cmpPtr)
        case _ => throw new Exception("A module must contain a lambda expression.")
      }
  }

  def componentExpression(cmpPtr: Component): SchemeExp = deref(cmpPtr) match {
    case Main => program
    case Call((exp, _), _, _) => exp
  }

  def updateLocalAddress(addr: LocalAddr, newIdentities: Map[Identity, Option[Identity]]): Option[LocalAddr] = ??? /* addr match {
    case VarAddr(id) => newIdentities(id.idn).map(idn => VarAddr(id.copy(idn = idn)))
    case PtrAddr(e) => ??? // TODO: update expressions
    case addr => Some(addr)
  }*/

  def updateComponentContext(ctx: ComponentContext): ComponentContext = ???
}
