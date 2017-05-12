import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scalaz._
import scalaz.Scalaz._

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

trait GraphNode[N, C] {
  def label(node: N): String = labelXml(node).mkString(" ")
  def labelXml(node: N): List[scala.xml.Node] = List(scala.xml.Text(label(node)))
  def label(node: N, ctx: C): String = label(node)
  def labelXml(node: N, ctx: C): List[scala.xml.Node] = labelXml(node)
  def tooltip(node: N): String = ""
  def tooltip(node: N, ctx: C): String = tooltip(node)
  def color(node: N): Color = Colors.White
  def color(node: N, ctx: C): Color = color(node)
  def content(node: N): JObject = JObject(Nil)
  def content(node: N, ctx: C): JObject = content(node)
}
object GraphNode {
  def apply[N, C](implicit g: GraphNode[N, C]): GraphNode[N, C] = g
}
trait GraphAnnotation[A, C] {
  def label(annot: A): String = ""
  def labelXml(annot: A): List[scala.xml.Node] = List(scala.xml.Text(label(annot)))
  def label(annot: A, ctx: C): String = label(annot)
  def labelXml(annot: A, ctx: C): List[scala.xml.Node] = List(scala.xml.Text(label(annot, ctx)))
  def color(annot: A): Color = Colors.Black
  def color(annot: A, ctx: C): Color = color(annot)
  def content(annot: A): JObject = JObject(Nil)
  def content(annot: A, ctx: C): JObject = content(annot)
}
object GraphAnnotation {
  implicit def unitGraphAnnotation[A]: GraphAnnotation[Unit, A] = new GraphAnnotation[Unit, A] {}
  def apply[A, C](implicit a: GraphAnnotation[A, C]): GraphAnnotation[A, C] = a
}

/** Represents a graph where nodes are elements of N, and edges are annotated with elements of type-class GraphAnnotation */
class Graph[N, A, C]
  (val ids: Map[N, Int], val next: Int, val nodes: Set[N], val edges: Map[N, Set[(A, N)]])
  (implicit val g: GraphNode[N, C], implicit val a: GraphAnnotation[A, C]) {
  /** Add a node to the graph */
  def addNode(node: N): Graph[N, A, C] =
    if (nodes.contains(node)) { this } else {
      new Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }
  /** Add an edge frome node1 to node2, with annotation annot */
  def addEdge(node1: N, annot: A, node2: N): Graph[N, A, C] =
    addNode(node1).addNode(node2).addEdgeNoCheck(node1, annot, node2)
  /** Add multiple edges */
  def addEdges(l: Traversable[(N, A, N)]): Graph[N, A, C] =
    l.foldLeft(this)({ case (g, (n1, annot, n2)) => g.addEdge(n1, annot, n2) })
  /** Remove a node from the graph */
  def removeNode(node: N): Graph[N, A, C] =
    new Graph(ids, next, nodes - node, edges = edges - node)
  /** Number of nodes in the graph */
  def size: Int = nodes.size
  /** Number of edges in the graph */
  def transitions: Int = edges.size
  /** Get the node with the given id, mostly used for debugging purposes */
  def getNode(id: Int): Option[N] = ids.find({ case (_, v) => id == v }).map(_._1)
  /** Get the id of the given node, mostly used for debugging purposes */
  def nodeId(node: N): Int = ids.getOrElse(node, -1)
  /** Add an edge between two nodes without checking if the edge already exists */
  private def addEdgeNoCheck(node1: N, annot: A, node2: N): Graph[N, A, C] =
    if (edges.contains(node1) && edges(node1).contains((annot, node2))) { this } else {
      val existing: Set[(A, N)] = edges.getOrElse(node1, Set[(A, N)]())
      new Graph(ids, next, nodes, edges + (node1 -> (existing ++ Set((annot, node2)))))
    }
}

object Graph {
  /** Constructs an empty graph */
  def empty[N, A, C](implicit g: GraphNode[N, C], a: GraphAnnotation[A, C]): Graph[N, A, C] =
    new Graph(Map[N, Int](), 0, Set[N](), Map[N, Set[(A, N)]]())
  /** Constructs a graph with a single node */
  def node[N, A, C](node: N)(implicit g: GraphNode[N, C], a: GraphAnnotation[A, C]): Graph[N, A, C] =
    new Graph(Map[N, Int]() + (node -> 0), 1, Set[N](node), Map[N, Set[(A, N)]]())
}

trait GraphOutput {
  def out[N, A, C](graph: Graph[N, A, C], ctx: C)(writer: java.io.Writer)(implicit g: GraphNode[N, C], a: GraphAnnotation[A, C]) : Unit
  def toFile[N, A, C](graph: Graph[N, A, C], ctx: C)(path: String)(implicit g: GraphNode[N, C], a: GraphAnnotation[A, C]): Unit = {
    Util.withFileWriter(path) { out(graph, ctx) }
  }
}

object GraphDOTOutput extends GraphOutput {
  def out[N, A, C](graph: Graph[N, A, C], ctx: C)(writer: java.io.Writer)(implicit g: GraphNode[N, C], a: GraphAnnotation[A, C]) = {
    writer.append("digraph G {\n")
    graph.nodes.foreach((n) => {
      val id = graph.ids(n)
      val label = GraphNode[N, C].labelXml(n, ctx).mkString(" ")
      val color = GraphNode[N, C].color(n, ctx)
      val tooltip = GraphNode[N, C].tooltip(n, ctx)
      writer.append(s"node_$id[shape=box, xlabel=$id, label=<$label>, fillcolor=<$color> style=<filled>, tooltip=<$tooltip>];\n")
    })
    graph.edges.foreach({ case (n1, ns) => ns.foreach({ case (annot, n2) =>
      val annotstr = GraphAnnotation[A, C].labelXml(annot, ctx).mkString(" ")
      writer.append(s"node_${graph.ids(n1)} -> node_${graph.ids(n2)} [label=<$annotstr>]\n")})})
    writer.append("}")
  }
}

object GraphJSONOutput extends GraphOutput {
  def out[N, A, C](graph: Graph[N, A, C], ctx: C)(w: java.io.Writer)(implicit g: GraphNode[N, C], a: GraphAnnotation[A, C]) = {
    import scala.language.implicitConversions
    implicit def nodesToJSON(node: N): JValue =
      ("label" -> GraphNode[N, C].labelXml(node, ctx).mkString(" ")) ~ g.content(node, ctx)
    implicit def annotToJSON(annot: A): JValue =
      ("label" -> GraphAnnotation[A, C].labelXml(annot, ctx).mkString(" ")) ~ GraphAnnotation[A, C].content(annot)
    implicit def pairToJSON(x: (Int, A)): JValue = JArray(List(JInt(x._1), annotToJSON(x._2)))

    /* array of nodes, index in the array is the index of the node, e.g.
     * [a, b, c]: a has index 0, b index 1, etc. */
    val ns: List[N] = graph.nodes.toList.sortBy(n => graph.ids(n))

    /* array of edges, index in the array is the source node, value is an array of
     * destination and annotation, e.g.:
     * [[[0 annotaa] [1 annotab]], ...]
     */
    val es: List[List[(Int, A)]] = graph.edges.toList
      .sortBy({ case (src, dests) => graph.ids(src) })
      .map({ case (src, dests) =>
        dests.toList.map({ case (annot, dest) =>
          (graph.ids(dest), annot)
        })
      })
    val json = ("nodes" -> ns) ~ ("edges" -> es)
    w.append(pretty(render(json)))
  }
}
