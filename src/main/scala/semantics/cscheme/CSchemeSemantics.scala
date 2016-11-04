import scalaz.Scalaz._
import scalaz._

class CSchemeSemantics[Abs : IsCSchemeLattice, Addr : Address, Time : Timestamp, TID : ThreadIdentifier](primitives: Primitives[Addr, Abs])
    extends SchemeSemantics[Abs, Addr, Time](primitives) {
  def cabs = implicitly[IsCSchemeLattice[Abs]]

  case class FrameJoin(env: Env) extends SchemeFrame
  case class FrameCasIndex(variable: Identifier, eold: SchemeExp, enew: SchemeExp, env: Env) extends SchemeFrame
  case class FrameCasOld(variable: Identifier, index: Option[Abs], enew: SchemeExp, env: Env) extends SchemeFrame
  case class FrameCasNew(variable: Identifier, index: Option[Abs], enew: SchemeExp, old: Abs, env: Env) extends SchemeFrame
  case class FrameAcquire(env: Env) extends SchemeFrame
  case class FrameRelease(env: Env) extends SchemeFrame

  override def stepEval(e: SchemeExp, env: Env, store: Sto, t: Time) = e match {
    case SchemeSpawn(exp, _) =>
      val tid = ThreadIdentifier[TID].thread[SchemeExp, Time](exp, t)
      Action.spawn(tid, exp, env, store, Action.value(IsCSchemeLattice[Abs].injectTid(tid), store))
    case SchemeJoin(exp, _) => optimizeAtomic(Action.push(FrameJoin(env), exp, env, store), t)
    case SchemeCas(variable, eold, enew, _) => Action.push(FrameCasOld(variable, None, enew, env), eold, env, store)
    case SchemeCasVector(variable, index, eold, enew, _) => Action.push(FrameCasIndex(variable, eold, enew, env), index, env, store)
    case SchemeAcquire(exp, _) => Action.push(FrameAcquire(env), exp, env, store)
    case SchemeRelease(exp, _) => Action.push(FrameRelease(env), exp, env, store)
    case _ => super.stepEval(e, env, store, t)
  }

  override def stepKont(v: Abs, frame: Frame, store: Sto, t: Time) = frame match {
    case FrameJoin(env) =>
      val tids = IsCSchemeLattice[Abs].getTids(v)
      if (tids.isEmpty) {
        Action.error(TypeError("join", "first operand", "tid value", s"non-tid value ($v)"))
      } else {
        tids.map(t => Action.join(t, store))
      }
    case FrameCasIndex(variable, eold, enew, env) =>
      Action.push(FrameCasOld(variable, Some(v), enew, env), eold, env, store)
    case FrameCasOld(variable, index, enew, env) =>
      Action.push(FrameCasNew(variable, index, enew, v, env), enew, env, store)
    case FrameCasNew(variable, index, enew, old, env) =>
      env.lookup(variable.name) match {
        case None => Action.error(UnboundVariable(variable))
        case Some(a) => index match {
          /* Compare and swap on variable value */
          case None => store.lookup(a) match {
            case None => Action.error(UnboundAddress(a.toString))
            case Some(vcomp) =>
              for { cond <- IsCSchemeLattice[Abs].binaryOp(SchemeOps.Eq)(vcomp, old) }
              yield
                conditional(cond,
                  /* Compare and swap succeeds */
                  Action.value(IsSchemeLattice[Abs].inject(true), store.update(a, v), Set(EffectWriteVariable(a), EffectReadVariable(a))),
                  /* Compare and swap fails */
                  Action.value(IsSchemeLattice[Abs].inject(false), store, Set(EffectReadVariable(a))))
          }
          /* Compare and swap on vector element */
          case Some(i) => store.lookup(a) match {
            case None => Action.error(UnboundAddress(a.toString))
            case Some(vs) =>
              val vectors = IsSchemeLattice[Abs].getVectors(vs)
              if (vectors.isEmpty && vectors != JoinLattice[Abs].bottom) {
                Action.error(TypeError("cas-vector", "first operand", "vector", s"non-vector: $vs"))
              } else {
                vectors.flatMap(va => store.lookup(va) match {
                  case None => Action.error(UnboundAddress(va.toString))
                  case Some(vec) =>
                    val mfmon = implicitly[Monoid[MayFail[Actions]]]
                    for { oldvals <- IsSchemeLattice[Abs].vectorRef(vec, i) }
                    yield {
                      val success: Actions = for {
                        (newvec, addrs) <- IsSchemeLattice[Abs].vectorSet(vec, i, Address[Addr].cell(enew, t))
                      } yield Action.value(IsCSchemeLattice[Abs].inject(true), addrs.foldLeft(store.update(va, newvec))((acc, a) => acc.updateOrExtend(a, v)),
                        addrs.flatMap(a => Set(EffectWriteVector(a), EffectReadVector(a))))
                      val fail: Actions = Action.value(IsCSchemeLattice[Abs].inject(false), store, Set(EffectReadVector(a)))
                      oldvals.foldLeft(Set[Action[SchemeExp, Abs, Addr]]())((acc, a) => store.lookup(a) match {
                        case None => acc + (Action.error(UnboundAddress(a.toString)))
                        case Some(oldval) => for { cond <- IsCSchemeLattice[Abs].binaryOp(SchemeOps.Eq)(oldval, old) }
                        yield conditional(cond, success, fail)
                      })
                    }
                })
              }
          }
        }
      }
    case FrameAcquire(env) =>
      val locks = IsCSchemeLattice[Abs].getLocks(v)
      if (locks.isEmpty && locks != JoinLattice[Abs].bottom) {
        Action.error(TypeError("acquire", "first operand", "lock value", s"non-lock value: $v"))
      } else {
        locks.flatMap(a => store.lookup(a) match {
          case None => Action.error(UnboundAddress(a.toString))
          case Some(v) => if (IsCSchemeLattice[Abs].isTrue(IsCSchemeLattice[Abs].isLock(v))) {
            if (IsSchemeLattice[Abs].isFalse(IsCSchemeLattice[Abs].isLocked(v))) {
              Action.value(IsSchemeLattice[Abs].inject(true), store.update(a, IsCSchemeLattice[Abs].lockedValue), Set(EffectAcquire(a)))
            } else {
              Action.none
            }
          } else {
            Action.error(TypeError("acquire", "first operand", "lock value", s"non-lock value: $v"))
          }
        })
      }
    case FrameRelease(env) =>
      val locks = IsCSchemeLattice[Abs].getLocks(v)
      if (locks.isEmpty && locks != JoinLattice[Abs].bottom) {
        Action.error(TypeError("release", "first operand", "lock value", s"non-lock value: $v"))
      } else {
        locks.flatMap(a => store.lookup(a) match {
          case None => Action.error(UnboundAddress(a.toString))
          case Some(v) => if (IsSchemeLattice[Abs].isTrue(IsCSchemeLattice[Abs].isLock(v))) {
            if (IsSchemeLattice[Abs].isTrue(IsCSchemeLattice[Abs].isLocked(v))) {
              Action.value(IsSchemeLattice[Abs].inject(true), store.update(a, IsCSchemeLattice[Abs].unlockedValue), Set(EffectRelease(a)))
            } else {
              /* Lock is already released */
              Action.value(IsSchemeLattice[Abs].inject(true), store)
            }
          } else {
            Action.error(TypeError("release", "first operand", "lock value", s"non-lock value: $v"))
          }
        })
      }
    case _ => super.stepKont(v, frame, store, t)
  }
}
