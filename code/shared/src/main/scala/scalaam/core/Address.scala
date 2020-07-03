package scalaam.core

import scalaam.util.SmartHash

/** An address */
trait Address extends SmartHash {

  /** Should the address be included when printing an environment or store?
    * This allows to reduce the size of the printed environment/store.
    * Address that are not printable may for example include addresses of primitive functions.
    */
  def printable: Boolean

  /** The identity of an address *
   * Should correspond to the program location where the address was allocated *
   * Can be Identity.none if there is no sensible program location (e.g., pre-allocated addresses for primitives) 
   */
  def idn: Identity
}
