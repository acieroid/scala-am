package scalaam.core

/** An address */
trait Address {
  /** Should the address be included when printing an environment or store?
      This allows to reduce the size of the printed environment/store.
      Address that are not printable may for example include addresses of primitive functions.
   */
  def printable: Boolean
}
