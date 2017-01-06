/** A lattice for Actor Scheme */
trait IsASchemeLattice[L] extends IsSchemeLattice[L] {
  def injectActor[Exp : Expression, Addr : Address](name: String, e: Exp, env: Environment[Addr]): L
  def getActors[Exp : Expression, Addr : Address](x: L): Set[(String, Exp, Environment[Addr])]
  def injectPid[PID : ThreadIdentifier](pid: PID): L
  def getPids[PID : ThreadIdentifier](x: L): Set[PID]
}

object IsASchemeLattice {
  def apply[L : IsASchemeLattice]: IsASchemeLattice[L] = implicitly
}

trait ASchemeLattice extends SchemeLattice {
  val isASchemeLattice: IsASchemeLattice[L]
  lazy val isSchemeLattice: IsSchemeLattice[L] = isASchemeLattice
}
