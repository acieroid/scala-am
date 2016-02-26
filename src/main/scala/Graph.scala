object Colors {
  val Yellow = "#FFFFDD"
  val Green = "#DDFFDD"
  val Pink = "#FFDDDD"
  val Red = "#FF0000"
  val White = "#FFFFFF"
}

case class Graph[Node, Annotation](ids: Map[Node, Int], next: Int, nodes: Set[Node], edges: Map[Node, Set[(Annotation, Node)]]) {
  def this() = this(Map[Node, Int](), 0, Set[Node](), Map[Node, Set[(Annotation, Node)]]())
  def this(node: Node) = this(Map[Node, Int]() + (node -> 0), 1, Set[Node](node), Map[Node, Set[(Annotation, Node)]]())
  def addNode(node: Node): Graph[Node, Annotation] =
    if (nodes.contains(node)) { this } else {
      Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }
  def addEdge(node1: Node, annot: Annotation, node2: Node): Graph[Node, Annotation] =
    addNode(node1).addNode(node2).addEdgeNoCheck(node1, annot, node2)
  def addEdges(l: Traversable[(Node, Annotation, Node)]): Graph[Node, Annotation] =
    l.foldLeft(this)({ case (g, (n1, annot, n2)) => g.addEdge(n1, annot, n2) })
  def addEdgeNoCheck(node1: Node, annot: Annotation, node2: Node): Graph[Node, Annotation] =
    if (edges.contains(node1) && edges(node1).contains((annot, node2))) { this } else {
      val existing: Set[(Annotation, Node)] = edges.getOrElse(node1, Set[(Annotation, Node)]())
      Graph(ids, next, nodes, edges + (node1 -> (existing ++ Set((annot, node2)))))
    }
  def size: Integer = nodes.size
  def transitions: Integer = edges.size
  def foldNodes[B](init: B)(f: (B, Node) => B) = nodes.foldLeft(init)(f)
  def getNode(id: Int): Option[Node] = ids.find({ case (_, v) => id == v }).map(_._1)
  def nodeId(node: Node): Int = ids.getOrElse(node, -1)
  def toDot(label: Node => List[scala.xml.Node], color: Node => String, annotLabel: Annotation => List[scala.xml.Node]): String = {
    val sb = new StringBuilder("digraph G {\n")
    nodes.foreach((n) => {
      val labelstr = label(n).mkString(" ")
      sb.append(s"node_${ids(n)}[label=<${ids(n)}: $labelstr>, fillcolor=<${color(n)}> style=<filled>];\n")
    })
    edges.foreach({ case (n1, ns) => ns.foreach({ case (annot, n2) =>
      val annotstr = annotLabel(annot).mkString(" ")
      sb.append(s"node_${ids(n1)} -> node_${ids(n2)} [label=<$annotstr>]\n")})})
    sb.append("}")
    return sb.toString
  }
  def toDotFile(path: String, label: Node => List[scala.xml.Node], color: Node => String, annotLabel: Annotation => List[scala.xml.Node]): Unit = {
    val f = new java.io.File(path)
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    bw.write(toDot(label, color, annotLabel))
    bw.close()
  }
}
