trait ThreadIdentifier[TID] {
 def name: String
  def initial: TID
  def thread[Exp : Expression, Time : Timestamp](exp: Exp, t: Time): TID
}

trait ContextSensitiveTID

object ContextSensitiveTID {
  object Initial extends ContextSensitiveTID {
    override def toString = "main"
  }
  case class TID[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends ContextSensitiveTID {
    override def  toString = s"$exp"
  }

  implicit object CSTIDThreadIdentifier extends ThreadIdentifier[ContextSensitiveTID] {
    def name = "ContextSensitive"
    def initial = Initial
    def thread[Exp : Expression, Time : Timestamp](exp: Exp, time: Time) = TID(exp, time)
  }
}

trait ConcreteTID

object ConcreteTID {
  var id = 0
  object Initial extends ConcreteTID {
    override def toString = "main"
  }
  case class TID[Exp : Expression](exp: Exp, id: Int) extends ConcreteTID {
    override def toString = s"$id"
  }
  implicit object ConcreteTIDThreadIdentifier extends ThreadIdentifier[ConcreteTID] {
    def name = "Concrete"
    def initial = Initial
    def thread[Exp : Expression, Time : Timestamp](exp: Exp, time: Time) = { id += 1; TID[Exp](exp, id) }
  }
}
