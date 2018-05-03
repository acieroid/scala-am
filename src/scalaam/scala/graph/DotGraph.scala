package scalaam.graph

class DotGraph[N <: GraphElement, E <: GraphElement](
  val ids: Map[N, Int],
  val next: Int,
  val _nodes: Set[N],
  val _edges: Map[N, Set[(E, N)]])
    extends Graph[N, E] {
  private def _addNode(node: N): DotGraph[N, E] =
    if (_nodes.contains(node)) { this } else {
      new DotGraph(ids + (node -> next), next + 1, _nodes + node, _edges)
    }
  def addNode(node: N): Graph[N, E] = _addNode(node)

  private def _addEdge(node1: N, edge: E, node2: N): DotGraph[N, E] =
    _addNode(node1)._addNode(node2)._addEdgeNoCheck(node1, edge, node2)
  def addEdge(node1: N, edge: E, node2: N): Graph[N, E] = _addEdge(node1, edge, node2)

  private def _addEdgeNoCheck(node1: N, edge: E, node2: N): DotGraph[N, E] =
    if (_edges.contains(node1) && _edges(node1).contains((edge, node2))) { this } else {
      val existing: Set[(E, N)] = _edges.getOrElse(node1, Set[(E, N)]())
      new DotGraph(ids, next, _nodes, _edges + (node1 -> (existing ++ Set((edge, node2)))))
    }

  def removeNode(node: N): Graph[N, E] =
    new DotGraph(ids, next, _nodes - node, _edges - node)
  def removeEdge(node1: N, edge: E, node2: N): Graph[N, E] = ???

  def nodes: Int = _nodes.size
  def edges: Int = _edges.size

  def toFile(path: String): Unit = {
    withFileWriter(path) { out }
  }


  private def withFileWriter(path: String)(body: java.io.Writer => Unit): Unit = {
    val f = new java.io.File(path)
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    body(bw)
    bw.close()
  }

  def out(writer: java.io.Writer) = {
    writer.append("digraph G {\n")
    _nodes.foreach((n) => {
      val id = ids(n)
      val label = n.label
      val color = n.color
      val tooltip = n.metadata.toString
      writer.append(s"node_$id[shape=box, xlabel=$id, label=<$label>, fillcolor=<$color> style=<filled>, tooltip=<$tooltip>];\n")
    })
    _edges.foreach({ case (n1, ns) => ns.foreach({ case (annot, n2) =>
      val annotstr = annot.label
      writer.append(s"node_${ids(n1)} -> node_${ids(n2)} [label=<$annotstr>]\n")})})
    writer.append("}")
  }
}

object DotGraph {
  def empty[N <: GraphElement, E <: GraphElement] =
    new DotGraph(Map[N, Int](), 0, Set[N](), Map[N, Set[(E, N)]]())
}
