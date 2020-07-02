package scalaam.modular.scheme.modf

import scalaam.modular._
import scalaam.core._
import scalaam.util._
import scalaam.language.scheme._

// A SchemeModFComponent represents function calls
sealed trait SchemeModFComponent[+Context, +Addr <: Address] extends SmartHash
// The main function call, i.e. the entry point of the program (corresponding to all top-level code)
case object Main extends SchemeModFComponent[Nothing,Nothing] {
  override def toString: String = "main"
}
// A call to a specific closure
case class Call[Context,Addr <: Address](clo: (SchemeLambdaExp, Environment[Addr]), 
                                         nam: Option[String], 
                                         ctx: Context) extends SchemeModFComponent[Context,Addr] {
  // convenience accessors
  lazy val (lambda, env) = clo
  // TODO: move this to SchemeLambdaExp
  def lambdaName: String = nam match {
    case None => s"λ@${lambda.idn}"
    case Some(name) => s"$name"
  }
  override def toString: String = lambdaName
}

case class StandardSchemeModFComponent[Context](cmp: SchemeModFComponent[Context,A[StandardSchemeModFComponent[Context]]]) {
  override def toString(): String = cmp.toString()
}

trait StandardSchemeModFComponents extends SchemeModFSemantics {
  type Component = StandardSchemeModFComponent[ComponentContext]
  lazy val initialComponent = StandardSchemeModFComponent(Main)
  def newComponent(call: Call[ComponentContext,Addr]) = StandardSchemeModFComponent(call)
  def view(cmp: Component): SchemeModFComponent[ComponentContext, Addr] = cmp.cmp
}

trait InnerSchemeModFComponents extends BaseSchemeModFSemantics {
  type Component = SchemeModFComponent[ComponentContext, Addr]
  lazy val initialComponent = Main
  def newComponent(call: Call[ComponentContext,Addr]) = call
  def view(cmp: Component): SchemeModFComponent[ComponentContext, Addr] = cmp
}

/*package scalaam.scalaam.modular.scheme

import scalaam.scalaam.core._
import scalaam.scalaam.language.scheme._

trait ModuledSchemeComponents extends SchemeModFSemantics {

  trait Module

  case object MainModule extends Module {
    def body: SchemeExp = program
    override def toString: String = "main"
  }
  case class FunctionModule(nam: Option[String], clo: scalaam.lattice.Closure) extends Module {
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

  implicit def contentOrdering: Ordering[Option[scalaam.lattice.Closure]] = new Ordering.OptionOrdering[scalaam.lattice.Closure] {
    def optionOrdering = Ordering[(Identity,Component)].on(clo => (clo._1.idn,clo._2))
  }

  type ComponentContent = Option[scalaam.lattice.Closure]
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