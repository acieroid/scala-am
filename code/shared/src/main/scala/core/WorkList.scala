package scalaam.core

/** Generic interface for a work list. */
trait WorkList[X] {
  def head: X
  def tail: WorkList[X]

  def add(x: X): WorkList[X]
  def add(xs: Iterable[X]): WorkList[X]           = xs.foldLeft(this)((acc,elm) => acc.add(elm))
  def ++=(xs: Iterable[X]): WorkList[X]           = add(xs)

  def map[Y]   (f: X =>       Y): WorkList[Y]
  def filter   (f: X => Boolean): WorkList[X]
  def filterNot(f: X => Boolean): WorkList[X]

  def  isEmpty: Boolean
  def nonEmpty: Boolean

  def toList: List[X]
  def toSet:   Set[X]
}

// Default implementation used in worklistMonoid.
object WorkList {
  def empty[X]: WorkList[X]                       = LIFOWorkList(List[X](),Set[X]())
}

// a worklist with deterministic depth-first exploration order
case class LIFOWorkList[X](lst: List[X], set: Set[X]) extends WorkList[X] {
  def isEmpty: Boolean                            = lst.isEmpty
  def nonEmpty: Boolean                           = lst.nonEmpty
  def head: X                                     = lst.head
  def tail: LIFOWorkList[X]                       = LIFOWorkList(lst.tail, set - lst.head)
  def add(x: X): LIFOWorkList[X]                  = if(set.contains(x)) { this } else { LIFOWorkList(x :: lst, set + x) }
  def map[Y](f: X => Y): LIFOWorkList[Y]          = LIFOWorkList(lst.map(f))
  def toList: List[X]                             = lst
  def toSet: Set[X]                               = set
  def filter(f: X => Boolean): LIFOWorkList[X]    = LIFOWorkList(lst.filter(f), set.filter(f))
  def filterNot(f: X => Boolean): LIFOWorkList[X] = LIFOWorkList(lst.filterNot(f), set.filterNot(f))
}
object LIFOWorkList {
  def empty[X]: LIFOWorkList[X]                   = LIFOWorkList(List[X](),Set[X]())
  def apply[X](xs: Iterable[X]): LIFOWorkList[X]  = empty.add(xs).asInstanceOf[LIFOWorkList[X]]
  def apply[X](xs: X*): LIFOWorkList[X]           = apply(xs)
}
