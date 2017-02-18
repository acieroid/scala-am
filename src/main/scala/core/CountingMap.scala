import scalaz.Scalaz._
import scalaz.Semigroup

trait Count {
  def inc: Count
}
case object CountOne extends Count {
  def inc = CountInfinity
}
case object CountInfinity extends Count {
  def inc = CountInfinity
}

object Count {
  /* We need it to form a semigroup to use |+| to join stores */
  implicit val isSemigroup  = new Semigroup[Count] {
    def append(x: Count, y: => Count) = CountInfinity
  }
}

case class CountingMap[A, B](content: Map[A, (Count, Set[B])]) {
  override def toString = content.toString
  def keys: Set[A] = content.keySet
  def forall(p: (A, B) => Boolean): Boolean = content.forall({ case (a, (_, vs)) => vs.forall(v => p(a, v)) })
  def exists(p: (A, B) => Boolean): Boolean = content.exists({ case (a, (_, vs)) => vs.exists(v => p(a, v)) })
  def foreach(p: (A, B) => Unit): Unit = content.foreach({ case (a, (_, vs)) => vs.foreach(v => p(a, v)) })
  def lookup(a: A): Set[B] = content.get(a) match {
    case None => Set[B]()
    case Some((_, bs)) => bs
  }
  def extend(a: A, b: B) = content.get(a) match {
    case None => this.copy(content = content + (a -> (CountOne, Set[B](b))))
    case Some((n, bs)) => this.copy(content = content + (a -> (n.inc, bs + b)))
  }
  def update(a: A, b: B) = content.get(a) match {
    case None => throw new RuntimeException(s"Updating counting map at a non-present key: $a")
    case Some((CountOne, _)) => this.copy(content = content + (a -> (CountOne, Set(b))))
    case _ => extend(a, b)
  }
  def updateOrExtend(a: A, b: B) = content.get(a) match {
    case None => extend(a, b)
    case Some(_) => update(a, b)
  }
  def remove(a: A, b: B): Set[CountingMap[A, B]] = content.get(a) match {
    case Some((CountOne, b2)) if b2 == Set(b) => Set(this.copy(content = content - a))
    case Some((CountOne, _)) => Set(this)
    case Some((CountInfinity, bs)) if (bs - b).isEmpty => Set(this, this.copy(content = content - a))
    case Some((CountInfinity, bs)) => Set(this, this.copy(content = content + (a -> (CountInfinity, bs -  b))))
    case None => Set(this)
  }
  def countOne(a: A) = content.get(a) match {
    case Some((CountOne, _)) => true
    case _ => false
  }
  def join(that: CountingMap[A, B]) =
    this.copy(content = content |+| that.content)

  def subsumes(that: CountingMap[A, B]) =
    that.content.forall({ case
      (a, (_, bs)) => bs.subsetOf(lookup(a))
    })
  def diff(that: CountingMap[A, B]): String =
    if (this == that) { "equals" }
    else if (this.keys != that.keys) { "keys are not equal" }
    else {
      val ks = this.keys.filter(k => (that.lookup(k) != this.lookup(k))).mkString(", ")
      s"the following keys are different: $ks"
    }

}

object CountingMap {
  def empty[A, B]: CountingMap[A, B] = CountingMap(Map[A, (Count, Set[B])]())
}
