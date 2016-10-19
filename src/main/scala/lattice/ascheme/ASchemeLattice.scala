/** A lattice for Actor Scheme */
trait IsASchemeLattice[L] extends IsSchemeLattice[L] {
  def injectActor[Exp : Expression, Addr : Address](e: Exp, env: Environment[Addr]): L
  def getActors[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])]
  def injectPid[PID : ThreadIdentifier](pid: PID): L
  def getPids[PID : ThreadIdentifier](x: L): Set[PID]
}

trait ASchemeLattice extends SchemeLattice {
  val isASchemeLattice: IsASchemeLattice[L]
  lazy val isSchemeLattice: IsSchemeLattice[L] = isASchemeLattice
}
