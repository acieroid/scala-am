package scalaam.graph

/** A graph that preserves no information */
class NoGraph[N <: GraphElement, E <: GraphElement] extends Graph[N, E] {
  def addNode(node: N) = this
  def addEdge(node1: N, edge: E, node2: N) = this
  override def addEdges(l: Iterable[(N, E, N)]) = this
  def removeNode(node: N) = this
  def removeEdge(node1: N, edge: E, node2: N) = this
  def nodes = 0
  def edges = 0
}
