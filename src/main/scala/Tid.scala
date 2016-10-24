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
    override def  toString = if (false && implicitly[Timestamp[Time]].name == "Concrete") {
      t.toString
    } else {
      exp.toString
    }
  }

  implicit object CSTIDThreadIdentifier extends ThreadIdentifier[ContextSensitiveTID] {
    def name = "ContextSensitive"
    def initial = Initial
    def thread[Exp : Expression, Time : Timestamp](exp: Exp, time: Time) = TID(exp, time)
  }
}
