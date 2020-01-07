package scalaam.core

import java.util.UUID

import scalaam.core.Identity._
import scalaam.util.SmartHash

/** An identity to distinguish expressions. */
sealed trait Identity {
  val idn: IDN
  def pos: Position = iMap(idn) // Extra positional information of the element in the source code.
  override def toString: String = s"${pos._1}:${pos._2}"
}

/** A position with a line and column */
case class SimpleIdentity(idn: IDN) extends Identity with SmartHash

/** Neutral identity for to elements not in the code. */
object NoCodeIdentity extends Identity { val idn: IDN = UUID.randomUUID() }

object Identity {

  type IDN = UUID // Type name is IDN to avoid confusion with identifiers.
  type Position = (Int, Int)

  // Used for printing and during tests.
  var iMap: Map[IDN, Position] = Map(NoCodeIdentity.idn -> ((-1, 0)))

  def apply(p: scala.util.parsing.input.Position): Identity = {
    val idn: IDN = UUID.randomUUID()
    iMap = iMap + (idn -> ((p.line, p.column)))
    SimpleIdentity(idn)
  }

  def none: Identity = NoCodeIdentity
}