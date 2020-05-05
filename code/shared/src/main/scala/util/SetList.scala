package scalaam.util

// set with deterministic depth-first exploration order (TODO: generalize with a trait to allow different implementations with different exploration strategies)
case class SetList[X](lst: List[X], set: Set[X]) {
  def isEmpty: Boolean                        = lst.isEmpty
  def head: X                                 = lst.head
  def tail: SetList[X]                        = SetList(lst.tail, set - lst.head)
  def add(x: X): SetList[X]                   = if(set.contains(x)) { this } else { SetList(x :: lst, set + x) }
  def add(xs: Iterable[X]): SetList[X]        = xs.foldLeft(this)((acc,elm) => acc.add(elm))
  def map[Y](f: X => Y): SetList[Y]           = SetList(lst.map(f))
  def toList: List[X]                         = lst
  def toSet: Set[X]                           = set
  def filter(f: X => Boolean): SetList[X]     = SetList(lst.filter(f), set.filter(f))
  def filterNot(f: X => Boolean): SetList[X]  = SetList(lst.filterNot(f), set.filterNot(f))
}
object SetList {
  def empty[X]: SetList[X]                    = SetList(List[X](),Set[X]())
  def apply[X](xs: Iterable[X]): SetList[X]   = empty.add(xs)
  def apply[X](xs: X*): SetList[X]            = apply(xs)
}
