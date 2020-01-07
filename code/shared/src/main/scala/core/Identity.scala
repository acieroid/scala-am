package scalaam.core

import java.util.UUID

import scalaam.core.Identity._
import scalaam.util.SmartHash

/** An identity to distinguish expressions. */
sealed trait Identity {
  val idn: IDN
  def pos: Position = Identity.synchronized { iMap(idn) } // Extra positional information of the element in the source code. Used for printing and during tests.
  override def toString: String = s"${pos._1}:${pos._2}"
}

/** An identity for AST elements. */
case class SimpleIdentity(idn: IDN) extends Identity with SmartHash

/** Neutral identity for to elements not in the code (constructed by the analysis). */
object NoCodeIdentity extends Identity {
  val idn: IDN = UUID.randomUUID()
  override def pos: Position = (-1, 0)
}

object Identity {

  type IDN = UUID // Type name is IDN to avoid confusion with identifiers.
  type Position = (Int, Int)

  /** Contains positional information for identifiers. ALL ACCESSES TO iMap HAVE TO BE SYNCHRONISED ON THE Identity OBJECT. */
  var iMap: Map[IDN, Position] = Map()

  def apply(p: scala.util.parsing.input.Position): Identity = {
    val idn: IDN = UUID.randomUUID()
    Identity.synchronized { iMap = iMap + (idn -> ((p.line, p.column))) }
    SimpleIdentity(idn)
  }

  def none: Identity = NoCodeIdentity
}