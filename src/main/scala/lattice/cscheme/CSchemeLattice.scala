/** A lattice for Concurrent Scheme */
trait IsCSchemeLattice[L] extends IsSchemeLattice[L] {
  /** Extract thread ids contained in this value */
  def getTids[TID : ThreadIdentifier](x: L): Set[TID]
  /** Extract lock addresses contained in this value */
  def getLocks[Addr : Address](x: L): Set[Addr]

  def isLock(x: L): L
  def isLocked(x: L): L

  /** Inject a thread id */
  def injectTid[TID : ThreadIdentifier](tid: TID): L
  /** Creates a lock wrapper (that contains the address of the lock) */
  def lock[Addr : Address](addr: Addr): L
  /** The locked value */
  def lockedValue: L
  /** The unlocked value */
  def unlockedValue: L
}

trait CSchemeLattice extends SchemeLattice {
  val isCSchemeLattice: IsCSchemeLattice[L]
  lazy val isSchemeLattice: IsSchemeLattice[L] = isCSchemeLattice
}
