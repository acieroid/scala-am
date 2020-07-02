package scalaam.modular.scheme.modconc

import scalaam.core._
import scalaam.util._
import scalaam.modular._
import scalaam.language.scheme._
import scalaam.language.CScheme._

// A SchemeModConcComponent represents threads
sealed trait SchemeModConcComponent[+Context, +Addr <: Address] extends SmartHash
// The main function call, i.e. the entry point of the program (corresponding to all top-level code)
case object MainThread extends SchemeModConcComponent[Nothing,Nothing] {
  override def toString: String = "main-thread"
}
// A call to a specific closure
case class Thread[Context,Addr <: Address](exp: SchemeExp,
                                           env: Environment[Addr],
                                           ctx: Context) extends SchemeModConcComponent[Context,Addr] { 
  override def toString: String = s"thread@${exp.idn}"
}

case class StandardSchemeModConcComponent[Context](cmp: SchemeModConcComponent[Context, A[StandardSchemeModConcComponent[Context]]]) extends TID {
    override def toString = cmp.toString()
}

trait StandardSchemeModConcComponents extends SchemeModConcSemantics {
    type Component = StandardSchemeModConcComponent[ComponentContext]
    lazy val initialComponent = StandardSchemeModConcComponent(MainThread)
    def newComponent(thread: Thread[ComponentContext,Addr]) = StandardSchemeModConcComponent(thread)
    def view(c: Component) = c.cmp
}