package scalaam.core

import scalaam.core.Identity._
import scalaam.util.SmartHash

/** An identity to distinguish expressions. */
trait Identity {
  def pos: Position
}

/** A position with a line and column */
case class SimpleIdentity(idn: Int) extends Identity with SmartHash {
  def pos = Identity.synchronized { iMap(idn) }
  override def toString: String = s"$idn (=> $pos)"
}

/** Neutral identity for to elements not in the code. */
case object NoCodeIdentity extends Identity {
  val idn = -1
  override def pos = ((-1, 0))
}

object Identity {

  var nextId = 0
  type Position = (Int, Int)

  // Used for printing and during tests.
  var iMap: Map[Int, Position] = Map()

  def apply(p: scala.util.parsing.input.Position): Identity = this.synchronized {
    val idn = nextId
    nextId = nextId + 1
    iMap = iMap + (idn -> ((p.line, p.column)))
    SimpleIdentity(idn)
  }

  def none: Identity = NoCodeIdentity
}
