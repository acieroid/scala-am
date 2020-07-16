package scalaam.modular.scheme.modconc

import scalaam.core._
import scalaam.util._
import scalaam.modular._
import scalaam.language.scheme._
import scalaam.language.CScheme._

// A SchemeModConcComponent represents threads
sealed trait SchemeModConcComponent extends SmartHash with TID
// The main thread
case object MainThread extends SchemeModConcComponent {
  override def toString: String = "main-thread"
}
// A thread that was spawned in some component
case class Thread[Context](exp: SchemeExp,
                           env: Environment[Address],
                           ctx: Context) extends SchemeModConcComponent { 
  override def toString: String = s"thread@${exp.idn}"
}


trait StandardSchemeModConcComponents extends SchemeModConcSemantics {
  type Component = SchemeModConcComponent
  lazy val initialComponent = MainThread
  def newComponent(thread: Thread[ComponentContext]) = thread
  def view(cmp: Component) = cmp
}