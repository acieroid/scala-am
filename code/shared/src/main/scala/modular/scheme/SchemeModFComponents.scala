package scalaam.modular.scheme

import scalaam.modular._
import scalaam.language.scheme._

trait StandardSchemeModFComponents extends SchemeModFSemantics {

  // In ModF, components are function calls in some context.

  // This abstract class is parameterised by the choice of two types of components:
  // * A MainComponent type representing the main function of the program.
  // * A CallComponent type representing function calls. CallComponents must have a parent pointer and lambda expression, contain a context and may contain a name.
  // The MainComponent should be unique and can hence be an object. CallComponents can be created using the `newCallComponent` function.
  // All components used together with this Scheme MODF analysis should be viewable as SchemeComponents.

  trait MainComponent extends SchemeComponent {
    def body: SchemeExp = program
    def env(cmp: Component): Env = initialEnv
    override def toString: String = "main"
  }
  trait CallComponent extends SchemeComponent {
    // Requires a closure and a context and may contain a name.
    def nam: Option[String]
    def clo: lattice.Closure
    def ctx: ComponentContext
    // convenience accessors
    lazy val (lambda, lexEnv) = clo
    lazy val body = SchemeBody(lambda.body)
    def env(currentCmp: Component) = lexEnv.extend(lambda.args.map { id =>
      (id.name, ComponentAddr(currentCmp, VarAddr(id)))
    })
    // TODO: move this to SchemeLambdaExp
    def printName: String = nam match {
      case None => s"λ@${lambda.idn}"
      case Some(name) => s"$name"
    }
    override def toString(): String = printName
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
}

trait StandardSchemeModFSemantics extends StandardSchemeModFComponents {
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
}

/*package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.scheme._

trait ModuledSchemeComponents extends SchemeModFSemantics {

  trait Module

  case object MainModule extends Module {
    def body: SchemeExp = program
    override def toString: String = "main"
  }
  case class FunctionModule(nam: Option[String], clo: lattice.Closure) extends Module {
    // convenience accessors
    lazy val (lambda, parent) = clo
    lazy val body: SchemeExp = SchemeBody(lambda.body)
  }

  // In ModF, components are function calls in some context.

  // This abstract class is parameterised by the choice of two types of components:
  // * A MainComponent type representing the main function of the program.
  // * A CallComponent type representing function calls. CallComponents must have a parent pointer and lambda expression, contain a context and may contain a name.
  // The MainComponent should be unique and can hence be an object. CallComponents can be created using the `newCallComponent` function.
  // All components used together with this Scheme MODF analysis should be viewable as SchemeComponents.

  trait SchemeComponent { def mod: Module }

  trait MainComponent extends SchemeComponent {
    def mod: Module = MainModule
  }
  trait CallComponent extends SchemeComponent {
    // Requires a closure and a context and may contain a name.
    def mod: FunctionModule
    def ctx: ComponentContext
    // convenience accessors
    override def toString: String = mod.nam match {
      case None => s"λ@${mod.lambda.idn} (${mod.parent}) [${ctx.toString}]"
      case Some(name) => s"$name (${mod.parent}) [${ctx.toString}]"
    }
  }

  implicit def componentAsModule(component: SchemeComponent): Module = component.mod

  implicit def contentOrdering: Ordering[Option[lattice.Closure]] = new Ordering.OptionOrdering[lattice.Closure] {
    def optionOrdering = Ordering[(Identity,Component)].on(clo => (clo._1.idn,clo._2))
  }

  type ComponentContent = Option[lattice.Closure]
  def content(cmp: Component) = view(cmp) match {
    case _ : MainComponent => None
    case call: CallComponent => Some(call.mod.clo)
  }
  def context(cmp: Component) = view(cmp) match {
    case _ : MainComponent => None
    case call: CallComponent => Some(call.ctx)
  }

  def componentParent(cmp: Component): Option[Component] = view(cmp) match {
    case _ : MainComponent => None
    case call: CallComponent => Some(call.mod.parent)
  }
}
*/