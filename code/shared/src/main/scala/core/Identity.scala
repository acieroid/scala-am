package scalaam.core

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
  val idn: IDN = Identity.newId()
  override def pos: Position = (-1, 0)
}

object Identity {

  type IDN = Long // Type name is IDN to avoid confusion with identifiers.
  type Position = (Int, Int)

  /** Contains positional information for identifiers. ALL ACCESSES TO iMap HAVE TO BE SYNCHRONISED ON THE Identity OBJECT. */
  var iMap: Map[IDN, Position] = Map()

  /** Contains the last unused identity. ALL ACCESSES TO ctr HAVE TO BE SYNCHRONISED ON THE Identity OBJECT. */
  private var ctr: Long = 0
  def newId(): IDN = Identity.synchronized {
    ctr = ctr + 1
    ctr - 1
  }

  def apply(p: scala.util.parsing.input.Position): Identity = Identity.synchronized {
    val idn: IDN = newId()
    iMap = iMap + (idn -> ((p.line, p.column)))
    SimpleIdentity(idn)
  }

  def none: Identity = NoCodeIdentity
}