package scalaam.core

// a worklist with deterministic depth-first exploration order (TODO: generalize with a trait to allow different implementations with different exploration strategies)
case class WorkList[X](lst: List[X], set: Set[X]) {
  def isEmpty: Boolean                          = lst.isEmpty
  def head: X                                   = lst.head
  def tail: WorkList[X]                         = WorkList(lst.tail, set - lst.head)
  def add(x: X): WorkList[X]                    = if(set.contains(x)) { this } else { WorkList(x :: lst, set + x) }
  def add(xs: Iterable[X]): WorkList[X]         = xs.foldLeft(this)((acc,elm) => acc.add(elm))
  def map[Y](f: X => Y): WorkList[Y]            = WorkList(lst.map(f))
  def toList: List[X]                           = lst
  def toSet: Set[X]                             = set
  def filter(f: X => Boolean): WorkList[X]      = WorkList(lst.filter(f), set.filter(f))
  def filterNot(f: X => Boolean): WorkList[X]   = WorkList(lst.filterNot(f), set.filterNot(f))
}
object WorkList {
  def empty[X]: WorkList[X]                     = WorkList(List[X](),Set[X]())
  def apply[X](xs: Iterable[X]): WorkList[X]    = empty.add(xs)
  def apply[X](xs: X*): WorkList[X]             = apply(xs)
}
