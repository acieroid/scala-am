case class Graph[A](ids: Map[A, Int], next: Int, nodes: Set[A], edges: Map[A, Set[A]]) {
  def this() = this(Map[A, Int](), 0, Set[A](), Map[A, Set[A]]())
  def this(node: A) = this(Map[A, Int]() + (node -> 0), 1, Set[A](node), Map[A, Set[A]]())
  def addNode(node: A): Graph[A] =
    if (nodes.contains(node)) { this } else {
      Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }
  def addEdge(node1: A, node2: A): Graph[A] =
      addNode(node1).addNode(node2).addEdgeNoCheck(node1, node2)
  def addEdges(l: Traversable[(A, A)]): Graph[A] =
    l.foldLeft(this)({ case (g, (n1, n2)) => g.addEdge(n1, n2) })
  def addEdgeNoCheck(node1: A, node2: A): Graph[A] =
    if (edges.contains(node1) && edges(node1).contains(node2)) { this } else {
      val existing: Set[A] = edges.getOrElse(node1, Set[A]())
      Graph(ids, next, nodes, edges + (node1 -> (existing ++ Set(node2))))
    }
  def size: Integer = nodes.size
  def foldNodes[B](init: B)(f: (B, A) => B) = nodes.foldLeft(init)(f)
  def getNode(id: Int): Option[A] = ids.find({ case (_, v) => id == v }).map(_._1)
  def toDot(label: A => String, color: A => String): String = {
      val sb = new StringBuilder("digraph G {\n")
      nodes.foreach((n) =>
        sb.append("node_" + ids(n) + "[label=\"" /* + ids(n).toString + " " */ + label(n).replaceAll("\"", "\\\\\"") + "\", fillcolor=\"" + color(n) + "\" style=\"filled\"];\n")
      )
      edges.foreach({ case (n1, ns) => ns.foreach((n2) => sb.append(s"node_${ids(n1)} -> node_${ids(n2)}\n")) })
      sb.append("}")
      return sb.toString
    }
  def toDotFile(path: String, label: A => String, color: A => String): Unit = {
    val f = new java.io.File(path)
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    bw.write(toDot(label, color))
    bw.close()
  }
}
