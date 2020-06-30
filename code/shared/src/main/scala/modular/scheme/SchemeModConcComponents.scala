package scalaam.modular.scheme

import scalaam.modular.scheme.semantics._
import scalaam.core._
import scalaam.util._
import scalaam.language.scheme._
import scalaam.language.CScheme._

// A SchemeModConcComponent represents threads
sealed trait SchemeModConcComponent[+Context, +Addr <: Address] extends TID with SmartHash
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

trait StandardSchemeModConcComponents extends SchemeModConcSemantics {
    // Components are just Scheme ModConc components
    // TODO: make this an "opaque type" in Scala 3?
    case class Component(cmp: SchemeModConcComponent[ComponentContext,Addr]) {
        override def toString = cmp.toString()
    }
    lazy val initialComponent = Component(MainThread)
    def newComponent(thread: Thread[ComponentContext,Addr]) = Component(thread)
    def view(c: Component) = c.cmp
}