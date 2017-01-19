import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

case class Color(hex: String) {
  override def toString = hex
}
object Colors {
  object Yellow extends Color("#FFFFDD")
  object Green extends Color("#DDFFDD")
  object Pink extends Color("#FFDDDD")
  object Red extends Color("#FF0000")
  object White extends Color("#FFFFFF")
  object Black extends Color("#000000")
}

/* TODO: add context, e.g. halted nodes can be provided, also global store/kstore */
trait GraphNode[N] {
  def label(node: N): List[scala.xml.Node]
  def tooltip(node: N): String = ""
  def color(node: N): Color = Colors.White
  def content(node: N): JObject = JObject(Nil)
}
object GraphNode {
  def apply[N: GraphNode]: GraphNode[N] = implicitly
}
trait GraphAnnotation[A] {
  def label(annot: A): List[scala.xml.Node] = List.empty
  def color(annot: A): Color = Colors.Black
  def content(annot: A): JObject = JObject(Nil)
}
object GraphAnnotation {
  implicit val unitGraphAnnotation: GraphAnnotation[Unit] = new GraphAnnotation[Unit] {}
  def apply[A : GraphAnnotation]: GraphAnnotation[A] = implicitly
}

/** Represents a graph where nodes are elements of N, and edges are annotated with elements of type-class GraphAnnotation */
case class Graph[N : GraphNode, A : GraphAnnotation]
    (ids: Map[N, Int], next: Int, nodes: Set[N], edges: Map[N, Set[(A, N)]]) {
  /** Constructs an empty graph */
  def this() = this(Map[N, Int](), 0, Set[N](), Map[N, Set[(A, N)]]())
  /** Constructs a graph with a single node */
  def this(node: N) = this(Map[N, Int]() + (node -> 0), 1, Set[N](node), Map[N, Set[(A, N)]]())
  /** Add a node to the graph */
  def addNode(node: N): Graph[N, A] =
    if (nodes.contains(node)) { this } else {
      Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }
  /** Add an edge frome node1 to node2, with annotation annot */
  def addEdge(node1: N, annot: A, node2: N): Graph[N, A] =
    addNode(node1).addNode(node2).addEdgeNoCheck(node1, annot, node2)
  /** Add multiple edges */
  def addEdges(l: Traversable[(N, A, N)]): Graph[N, A] =
    l.foldLeft(this)({ case (g, (n1, annot, n2)) => g.addEdge(n1, annot, n2) })
  /** Number of nodes in the graph */
  def size: Int = nodes.size
  /** Number of edges in the graph */
  def transitions: Int = edges.size
  /** Get the node with the given id, mostly used for debugging purposes */
  def getNode(id: Int): Option[N] = ids.find({ case (_, v) => id == v }).map(_._1)
  /** Get the id of the given node, mostly used for debugging purposes */
  def nodeId(node: N): Int = ids.getOrElse(node, -1)
  /** Add an edge between two nodes without checking if the edge already exists */
  private def addEdgeNoCheck(node1: N, annot: A, node2: N): Graph[N, A] =
    if (edges.contains(node1) && edges(node1).contains((annot, node2))) { this } else {
      val existing: Set[(A, N)] = edges.getOrElse(node1, Set[(A, N)]())
      Graph(ids, next, nodes, edges + (node1 -> (existing ++ Set((annot, node2)))))
    }
}

object GraphDOTOutput {
  private def toDot[N : GraphNode, A : GraphAnnotation](graph: Graph[N, A])(writer: java.io.Writer): Unit = {
    writer.append("digraph G {\n")
    graph.nodes.foreach((n) => {
      val labelstr = GraphNode[N].label(n).mkString(" ")
      writer.append(s"node_${graph.ids(n)}[shape=box, xlabel=${graph.ids(n)}, label=<$labelstr>, fillcolor=<${GraphNode[N].color(n)}> style=<filled>, tooltip=<${GraphNode[N].tooltip(n)}>];\n")
    })
    graph.edges.foreach({ case (n1, ns) => ns.foreach({ case (annot, n2) =>
      val annotstr = GraphAnnotation[A].label(annot).mkString(" ")
      writer.append(s"node_${graph.ids(n1)} -> node_${graph.ids(n2)} [label=<$annotstr>]\n")})})
    writer.append("}")
  }
  def toDotFile[N : GraphNode, A : GraphAnnotation](graph: Graph[N, A])(path: String) = {
    Util.withFileWriter(path) { toDot(graph) }
  }
}

object GraphJSONOutput {
  /*
  def toJSONFile(path: String): Unit =  {
    throw new Exception("TODO")
    /*
    /* array of nodes, index in the array is the index of the node, e.g.
     * [a, b, c]: a has index 0, b index 1, etc. */
    val ns: List[Node] = nodes.toList.sortBy(n => ids(n))
    /* array of edges, index in the array is the source node, value is an array of
     * destination and annotation, e.g.:
     * [[[0 annotaa] [1 annotab]], ...]
     */
    import scala.language.implicitConversions
    implicit def pairToJSON(x: (Int, Annotation)) = JArray(List(JInt(x._1), annotToJSON(x._2)))
    val es: List[List[(Int, Annotation)]] = edges.toList
      .sortBy({ case (src, dests) => ids(src) })
      .map({ case (src, dests: Set[(Annotation, Node)]) =>
        dests.toList.map({ case (annot, dest) =>
          (ids(dest), annot)
        })
      })
    val json = ("nodes" -> ns) ~ ("edges" -> es)
    Util.withFileWriter(path) { w => w.append(pretty(render(json))) }
     */
  }
   */
}
