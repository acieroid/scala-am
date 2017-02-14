trait MboxSize {
  def >(that: MboxSize): Boolean
}
case class MboxSizeN(n: Int) extends MboxSize {
  override def toString = n.toString
  def >(that: MboxSize): Boolean = that match {
    case MboxSizeN(n2) => n > n2
    case MboxSizeUnbounded => false
  }
}
case object MboxSizeUnbounded extends MboxSize {
  override def toString = "+"
  def >(that: MboxSize): Boolean = true
}

trait MboxImpl[PID, Abs] {
  type Message = (PID, String, List[Abs])
  def mToString(m: Message) = m match {
    case (sender, message, vs) =>
      message + (if (vs.isEmpty) { "" } else { "(" + vs.mkString(",") + ")" })
  }
  trait T {
    def pop: Set[(Message, T)]
    def push(m: Message): T
    def isEmpty: Boolean
    def size: MboxSize
    def messagesList: List[Message] /* only for information to the user */
  }
  def empty: T
}

object IdGen {
  var i = 0;
  def next: Int = {
    i += 1
    i
  }
}
case class GraphMboxImpl[PID, Abs]() extends MboxImpl[PID, Abs] {
  /* messages are pushed on the top of the queue, popped from the bottom */
  case class M(top: Option[Message], bot: Option[Message], nodes: Map[Message, Set[Message]]) extends T {
    def pop = bot match {
      case None => Set[(Message, T)]()
      case Some(m) => nodes.get(m) match {
        case None => Set((m, empty))
        case Some(nexts) if nexts.isEmpty => Set((m, empty))
        case Some(nexts) if nexts.size == 1 && nodes.count({ case (_, vs) => vs.contains(m) }) == 0 =>
          /* can garbage collect bottom (which is m), since nothing points to it */
          Set((m, this.copy(bot = Some(nexts.head), nodes = nodes - m)))
        case Some(nexts) => nexts.map(b => (m, this.copy(bot = Some(b))))
      }
    }
    def push(m: Message) = top match {
      case None => this.copy(top = Some(m), bot = bot.orElse(Some(m)), /* only needed to ensure m is in keySet */ nodes + (m -> nodes(m)))
      case Some(t) => this.copy(top = Some(m), bot = bot.orElse(Some(m)), nodes = (nodes + /* only needed to ensure m is in keySet */ (m -> nodes(m)) + (t -> (nodes(t) + m))))
    }
    def isEmpty = !top.isDefined
    def followPath(from: Message, n: Int, visited: Set[Message]): MboxSize = {
      if (visited.contains(from)) {
        /* cycle */
        MboxSizeUnbounded
      } else if (Some(from) == top) {
        /* reached top */
        MboxSizeN(n)
      } else {
        val next = nodes(from)
        if (next.size == 0) {
          throw new Exception("no next before bottom?")
        } else if (next.size == 1) {
          followPath(next.head, n+1, visited + from)
        } else {
          MboxSizeUnbounded
        }
      }
    }
    def size = if (top.isDefined) {
      if (nodes.forall({ case (_, ns) => ns.size <= 1 })) {
        followPath(bot.get, 1, Set.empty)
      } else {
        MboxSizeUnbounded
      }
    } else { MboxSizeN(0) }
    override def toString = bot match {
      case Some(m) =>
        mToString(m) + (if (nodes.keySet.size > 1) { ", {" + (nodes.keySet - m).map(m => mToString(m)).mkString(" + ") + "}" } else { "" })
      case None => ""
    }
    def messagesList = {
      def followPath(from: Message, acc: List[Message], visited: Set[Message]): List[Message] = {
        if (visited.contains(from)) {
          nodes.keySet.toList.sortBy(_._2)
        } else if (Some(from) == top) {
          acc.reverse
        } else {
          val next = nodes(from)
          if (next.size == 0) {
            throw new Exception("no next before bottom?")
          } else if (next.size == 1) {
            followPath(next.head, next.head :: acc, visited + from)
          } else {
            nodes.keySet.toList.sortBy(_._2)
          }
        }
      }
      if (bot.isDefined && nodes(bot.get).contains(bot.get)) {
        nodes.keySet.toList.sortBy(_._2)
      } else if (bot.isDefined) {
        followPath(bot.get, List(bot.get), Set.empty)
      } else {
        List()
      }
    }
  }
  def empty = M(None, None, Map.empty.withDefaultValue(Set.empty))
}

case class PowersetMboxImpl[PID, Abs]() extends MboxImpl[PID, Abs] {
  case class M(messages: Set[Message]) extends T {
    def pop = messages.flatMap(m => Set((m, this), (m, this.copy(messages = messages - m))))
    def push(m: Message) = this.copy(messages = messages + m)
    def isEmpty = messages.isEmpty
    def size = if (messages.isEmpty) { MboxSizeN(0) } else { MboxSizeUnbounded }
    def messagesList = messages.toList.sortBy(_._2)
    override def toString = messages.map(mToString).mkString(" + ")
  }
  def empty = M(Set.empty)
}

case class ListMboxImpl[PID, Abs]() extends MboxImpl[PID, Abs] {
  case class M(messages: List[Message]) extends T {
    def pop = messages match {
      case Nil => Set.empty
      case h :: t => Set((h, M(t)))
    }
    def push(m: Message) = this.copy(messages = messages :+ m)
    def isEmpty = messages.isEmpty
    def size = MboxSizeN(messages.size)
    def messagesList = messages
    override def toString = messages.map(mToString).mkString(", ")
  }
  def empty = M(List.empty)
}

case class BoundedListMboxImpl[PID, Abs](val bound: Int) extends MboxImpl[PID, Abs] {
  case class MOrdered(messages: List[Message]) extends T {
    def pop = messages match {
      case Nil => Set.empty
      case h :: t => Set((h, MOrdered(t)))
    }
    def push(m: Message) = if (messages.size == bound) {
      MUnordered(messages.toSet + m)
    } else {
      this.copy(messages = messages :+ m)
    }
    def isEmpty = messages.isEmpty
    def size = MboxSizeN(messages.size)
    def messagesList = messages
    override def toString = messages.map(mToString).mkString(", ")
  }
  case class MUnordered(messages: Set[Message]) extends T {
    def pop = messages.flatMap(m => Set((m, this), (m,
      if (messages.size == 1) {
        MOrdered(Nil)
      } else {
        this.copy(messages = messages - m)
      })))
    def push(m: Message) = this.copy(messages = messages + m)
    def isEmpty = messages.isEmpty
    def size = MboxSizeUnbounded
    def messagesList = messages.toList.sortBy(_._2)
    override def toString = "U(" + messages.map(mToString).mkString(" + ") + ")"
  }
  def empty = MOrdered(List.empty)
}

case class MultisetMboxImpl[PID, Abs]() extends MboxImpl[PID, Abs] {
  case class M(messages: Set[(Message, Int)]) extends T {
    def pop = messages.map({
      case (m, 1) => (m, M(messages - ((m, 1))))
      case (m, count) => (m, M(messages - ((m, count)) + ((m, count - 1))))
    })
    def push(m: Message) = messages.find({ case (m2, count) => m2 == m }) match {
      case Some((_, count)) => M(messages - ((m, count)) + ((m, count + 1)))
      case None => M(messages + ((m, 1)))
    }
    def isEmpty = messages.isEmpty
    def size = MboxSizeN(messages.toList.map(_._2).sum)
    def messagesList = ???
    override def toString = messages.map(m => s"${mToString(m._1)}: ${m._2}").mkString(" + ")
  }
  def empty = M(Set.empty)
}

case class BoundedMultisetMboxImpl[PID, Abs](val bound: Int) extends MboxImpl[PID, Abs] {
  case class M(messages: Set[(Message, Int)], noCountMessages: Set[Message]) extends T {
    def pop = messages.map({
      case (m, 1) => (m, this.copy(messages = messages - ((m, 1))))
      case (m, count) => (m, (this.copy(messages = messages - ((m, count)) + ((m, count - 1)))))
    }) ++ noCountMessages.flatMap(m => Set((m, this), (m, this.copy(messages = messages + ((m, bound)), noCountMessages = noCountMessages - m))))
    def push(m: Message) = if (noCountMessages.contains(m)) { this } else if (bound >= 1) {
      messages.find({ case (m2, count) => m2 == m }) match {
        case Some((_, count)) if count + 1 < bound => this.copy(messages = messages - ((m, count)) + ((m, count + 1)))
        case Some((_, count)) => this.copy(
          messages = messages - ((m, count)),
          noCountMessages = noCountMessages + m)
        case None => this.copy(messages = messages + ((m, 1)))
      }
    } else { this.copy(noCountMessages = noCountMessages + m) }
    def isEmpty = messages.isEmpty && noCountMessages.isEmpty
    def messagesList = (messages.map(_._1).toList ++ noCountMessages.toList).sortBy(_._2)
    def size = if (noCountMessages.isEmpty) { MboxSizeN(messages.toList.map(_._2).sum) } else { MboxSizeUnbounded }
    override def toString = {
      val unord = if (messages.isEmpty) { "" } else { "O(" + messages.map(m => s"${mToString(m._1)}: ${m._2}").mkString(", ") + ")" }
      val sep = if (!messages.isEmpty && !noCountMessages.isEmpty) { ", " } else { "" }
      val ord = if (noCountMessages.isEmpty) { "" } else { "U(" + noCountMessages.map(mToString).mkString(" + ") + ")" }
      unord + sep + ord
    }
  }
  def empty = M(Set.empty, Set.empty)
}
