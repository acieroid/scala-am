package scalaam.graph

case class DotGraph[N <: GraphElement, E <: GraphElement]() {
  class G(val ids: Map[N, Int],
          val next: Int,
          val _nodes: Set[N],
          val _edges: Map[N, Set[(E, N)]]) {
    def _addNode(node: N): G =
      if (_nodes.contains(node)) { this } else {
        new G(ids + (node -> next), next + 1, _nodes + node, _edges)
      }
    private def _addEdgeNoCheck(node1: N, edge: E, node2: N): G =
      if (_edges.contains(node1) && _edges(node1).contains((edge, node2))) { this } else {
        val existing: Set[(E, N)] = _edges.getOrElse(node1, Set[(E, N)]())
        new G(ids, next, _nodes, _edges + (node1 -> (existing ++ Set((edge, node2)))))
      }
    def _addEdge(node1: N, edge: E, node2: N): G =
      _addNode(node1)._addNode(node2)._addEdgeNoCheck(node1, edge, node2)

    def toFile(path: String): Unit = {
      withFileWriter(path)(out)
    }

    private def withFileWriter(path: String)(body: java.io.Writer => Unit): Unit = {
      val f  = new java.io.File(path)
      val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
      body(bw)
      bw.close()
    }

    def out(writer: java.io.Writer): Unit = {
      writer.write("digraph G {\n")
      _nodes.foreach((n) => {
        val id      = ids(n)
        val label   = n.label.replace("<", "&lt;").replace(">", "&gt;")
        val color   = n.color
        val tooltip = n.metadata.toString.replace("<", "&lt;").replace(">", "&gt;")
        writer.write(
          s"node_$id[shape=box, xlabel=$id, label=<$label>, fillcolor=<$color> style=<filled>, tooltip=<$tooltip>];\n")
      })
      _edges.foreach({
        case (n1, ns) =>
          ns.foreach({
            case (annot, n2) =>
              val annotstr = annot.label
              writer.write(s"node_${ids(n1)} -> node_${ids(n2)} [label=<$annotstr>]\n")
          })
      })
      writer.write("}")
    }

    def getNode(id: Int): Option[N] = ids.find({ case (_, v) => id == v }).map(_._1)

  }

  object G {
    implicit val typeclass = new Graph[G, N, E] {
      def empty                                         = new G(Map[N, Int](), 0, Set[N](), Map[N, Set[(E, N)]]())
      def addNode(g: G, node: N)                        = g._addNode(node)
      def addEdge(g: G, node1: N, edge: E, node2: N)    = g._addEdge(node1, edge, node2)
      def removeNode(g: G, node: N)                     = new G(g.ids, g.next, g._nodes - node, g._edges)
      def removeEdge(g: G, node1: N, edge: E, node2: N) = ??? /* TODO[easy] implement */
      def nodes(g: G)                                   = g._nodes.size
      def edges(g: G)                                   = g._edges.size
      def findNodes(g: G, p: N => Boolean)              = ??? /* TODO[easy]: implement */
    }
  }
}

object DotGraph {
  def empty[N <: GraphElement, E <: GraphElement] = new DotGraph().G.typeclass.empty
}
