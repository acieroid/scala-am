package scalaam.core

import scalaam.core.Identity._
import scalaam.core.Position._
import scalaam.util.SmartHash

/** An identity to distinguish expressions. */
sealed trait Identity {
  val idn: IDN
  def pos: Position = Identity.synchronized { iMap(idn) } // Extra positional information of the element in the source code. Used for printing and during tests.
  override def toString: String = pos.toString
}

/** An identity for AST elements. */
case class SimpleIdentity(idn: IDN) extends Identity with SmartHash

/** Neutral identity for to elements not in the code (constructed by the analysis). */
object NoCodeIdentity extends Identity {
  val idn: IDN = Identity.newId()
  override def pos: Position = Position(-1, 0)
}

object Identity {

  type IDN = Long // Type name is IDN to avoid confusion with identifiers.

  /** Contains positional information for identifiers. ALL ACCESSES TO iMap HAVE TO BE SYNCHRONISED ON THE Identity OBJECT. */
  var iMap: Map[IDN, Position] = Map()

  /** Contains the last unused identity. ALL ACCESSES TO ctr HAVE TO BE SYNCHRONISED ON THE Identity OBJECT. */
  private var ctr: Long = 0
  def newId(): IDN = Identity.synchronized {
    ctr = ctr + 1
    ctr - 1
  }

  def apply(p: scala.util.parsing.input.Position, t: PTag = noTag): Identity = Identity.synchronized {
    val idn: IDN = newId()
    iMap = iMap + (idn -> Position(p.line, p.column, t))
    SimpleIdentity(idn)
  }

  def none: Identity = NoCodeIdentity // Todo (maybe): add tag.
}

object Position {

  type PTag = Option[String] // Tag for positions (can e.g. be used when ASTs of multiple parsings need to be combined).

  val noTag: PTag = None
  def newTag(tag: String): PTag = Some(tag)

  case class Position(line: Int, col: Int, tag: PTag = noTag) extends SmartHash {
    override def toString: String = tag match {
      case None    => s"$line:$col"
      case Some(t) => s"$t:$line:$col"
    }
  }

  def apply(line: Int, col: Int, tag: PTag = noTag): Position = Position(line, col, tag)
}