package scalaam.graph

trait GraphMetadata
case class GraphMetadataMap(map: Map[String, GraphMetadata]) extends GraphMetadata
case class GraphMetadataString(str: String) extends GraphMetadata
case object GraphMetadataNone extends GraphMetadata

trait GraphElement {
  def label: String
  def color: Color
  def metadata: GraphMetadata
}

class NoTransition extends GraphElement {
  def label = ""
  def color = Colors.Black
  def metadata = GraphMetadataNone
}

object EmptyGraphElement

/** A graph with nodes of type N and edges of type E.
    Edges have a specific type because they may contain information (i.e., they can be annotated).
 */
trait Graph[N <: GraphElement, E <: GraphElement] {
  /** Add a node to the graph, without any edge */
  def addNode(node: N): Graph[N, E]
  /** Add an edge between two nodes, and also adds the nodes that are not yet in the graph */
  def addEdge(node1: N, edge: E, node2: N): Graph[N, E]
  /** Add multiple edges at a time */
  def addEdges(l: Iterable[(N, E, N)]): Graph[N, E] =
    l.foldLeft(this)({ case (g, (n1, e, n2)) => g.addEdge(n1, e, n2) })
  /** Remove a node from the graph */
  def removeNode(node: N): Graph[N, E]
  /** Remove an edge between two nodes from the graph.
      Does not remove any node. */
  def removeEdge(node1: N, edge: E, node2: N): Graph[N, E]

  /** Returns the number of nodes in the graph */
  def nodes: Int
  /** Returns the number of edges in the graph */
  def edges: Int
}
