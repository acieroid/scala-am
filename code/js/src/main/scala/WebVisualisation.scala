package scalaam.web

import scalaam.modular._

// Scala.js-related imports
import scala.scalajs.js
import org.scalajs.dom
import dom.{document}

import scala.collection.mutable._

object WebVisualisation {
  // some shorthands
  type JsAny = js.Dynamic
  type JsArray[A] = js.Array[A]
  val d3 = js.Dynamic.global.d3
  // some constants
  val __CIRCLE_RADIUS__ = 15
  val __SVG_ARROW_ID__ = "endarrow"
  val __CSS_NOT_VISITED__ = "not_visited"
  val __CSS_IN_WORKLIST__ = "in_worklist"
  val __CSS_NEXT_COMPONENT__ = "next_component"
  val __CSS_FINISHED__ = "finished"
  val __FORCE_COLLIDE__ = "collide"
  val __FORCE_CHARGE__ = "charge"
  val __FORCE_LINKS__ = "links"
  val __FORCE_CENTER__ = "center"
  // some JS helpers
  implicit def toJsArray[A](seq: Iterable[A]): JsArray[A] = {
    val array = new js.Array[A]()
    seq.foreach { item => array.push(item) }
    return array
  }
}

class WebVisualisation(val analysis: ModAnalysis[_]) {

  // TODO: make this abstract
  def displayText(cmp: analysis.Component): String = cmp.toString()

  import WebVisualisation._

  class Node(val component: analysis.Component) extends js.Object
  class Edge(val source: Node, val target: Node) extends js.Object

  //
  // BOOKKEEPING (needed to play nice with Scala.js and d3.js)
  //

  val nodesData = new js.Array[Node]()
  val edgesData = new js.Array[Edge]()
  val nodesColl = Map[analysis.Component, Node]()
  val edgesColl = Map[(Node,Node),Edge]()
  private def addNode(cmp: analysis.Component): Node = nodesColl.get(cmp) match {
    case None =>
      val newNode = new Node(cmp)
      nodesColl(cmp) = newNode
      nodesData.push(newNode)
      return newNode
    case Some(existingNode) =>
      return existingNode
  }
  private def addEdge(source: Node, target: Node): Edge = edgesColl.get((source,target)) match {
    case None =>
      val newEdge = new Edge(source, target)
      edgesColl((source,target)) = newEdge
      edgesData.push(newEdge)
      return newEdge
    case Some(existingEdge) =>
      return existingEdge
  }

  //
  // VISUALISATION SETUP
  //

  var nodes, edges: JsAny   = null                    // will be used to keep selections of nodes/edges in the visualisation
  val simulation: JsAny     = d3.forceSimulation()    // create a d3 force simulation

  def init(parent: dom.Node, width: Int, height: Int) = {
    // setup the svg
    val svg = d3.select(parent).append("svg").attr("width",width).attr("height",height)
    val outerContainer = svg.append("g")
    val innerContainer = outerContainer.append("g").attr("transform",s"translate(${width/2},${height/2})")
    val nodesContainer = innerContainer.append("g").attr("class","nodes")
    val edgesContainer = innerContainer.append("g").attr("class","links")
    nodes = nodesContainer.selectAll("g")
    edges = edgesContainer.selectAll("path")
    setupMarker(svg)
    // setup the click handler
    svg.on("click", () => onClick())
    // setup the key handler
    d3.select(document.body).on("keypress", () => keyHandler(d3.event.key.asInstanceOf[String]))
    // setup a fancy zoom effect
    svg.call(d3.zoom().on("zoom", () => outerContainer.attr("transform",d3.event.transform)))
    // setup the simulation
    simulation.force(__FORCE_COLLIDE__, d3.forceCollide().radius(__CIRCLE_RADIUS__))
              .force(__FORCE_CHARGE__, d3.forceManyBody().strength(-500))
              .force(__FORCE_LINKS__, d3.forceLink().distance(150))
              .force(__FORCE_CENTER__, d3.forceCenter())
              .on("tick", () => onTick())
    // reload the data and visualisation
    refresh()
  }

  private def setupMarker(svg: JsAny) = {
    // adapted from http://bl.ocks.org/fancellu/2c782394602a93921faff74e594d1bb1
    val marker = svg.append("defs").append("marker")
                                    .attr("id",__SVG_ARROW_ID__)
                                    .attr("viewBox","-0 -5 10 10")
                                    .attr("refX",0)
                                    .attr("refY",0)
                                    .attr("orient","auto")
                                    .attr("markerWidth",5)
                                    .attr("markerHeight",5)
    marker.append("svg:path")
          .attr("d", "M 0,-5 L 10 ,0 L 0,5")
          //.attr("fill", "#999")
          //.style("stroke","none")
  }

  private def onTick() = {
    // update the nodes
    nodes.attr("transform", (node: JsAny) => s"translate(${node.x},${node.y})")
    // update the edges
    edges.attr("d",(edge: JsAny) =>
      if (edge.source == edge.target) {
        val cx = edge.source.x.asInstanceOf[Double]
        val cy = edge.source.y.asInstanceOf[Double]
        val x1 = cx - __CIRCLE_RADIUS__
        val y1 = cy
        val x2 = cx - 9
        val y2 = cy - __CIRCLE_RADIUS__ - 8
        s"M$x1 $y1 A ${__CIRCLE_RADIUS__} ${__CIRCLE_RADIUS__} 1 1 1 $x2 $y2"
      } else {
        val sourceX = edge.source.x.asInstanceOf[Double]
        val sourceY = edge.source.y.asInstanceOf[Double]
        val targetX = edge.target.x.asInstanceOf[Double]
        val targetY = edge.target.y.asInstanceOf[Double]
        val deltaX = targetX - sourceX
        val deltaY = targetY - sourceY
        val dist = Math.sqrt((deltaX * deltaX) + (deltaY * deltaY))
        val scaleFactorSource = __CIRCLE_RADIUS__ / dist
        val scaleFactorTarget = (__CIRCLE_RADIUS__ + 10) / dist
        val x1 = sourceX + (deltaX * scaleFactorSource)
        val x2 = targetX - (deltaX * scaleFactorTarget)
        val y1 = sourceY + (deltaY * scaleFactorSource)
        val y2 = targetY - (deltaY * scaleFactorTarget)
        s"M$x1 $y1 L$x2 $y2"
    })
  }

  //
  // REFRESHING
  //

  // updates both the data and the visualisation
  def refresh() = {
    refreshData()
    refreshVisualisation()
  }

  // ensures that `nodesData` and `edgesData` are in sync with the analysis
  def refreshData() = {
    analysis.allComponents.foreach(addNode)
    analysis.dependencies.foreach { case (source,targets) =>
      val sourceNode = addNode(source)
      targets.foreach(target => {
        val targetNode = addNode(target)
        addEdge(sourceNode,targetNode)
      })
    }
  }

  // more efficient than `refreshData`: updates only data that may have changed after stepping
  def refreshDataAfterStep(cmp: analysis.Component) = {
    val sourceNode = addNode(cmp)
    analysis.dependencies(cmp).foreach { otherCmp =>
      val targetNode = addNode(otherCmp)
      addEdge(sourceNode,targetNode)
    }
  }

  // updates the visualisation: draws all nodes/edges, sets correct CSS classes, etc.
  def refreshVisualisation() = {
    // update the nodes
    val nodesUpdate = nodes.data(nodesData)
    val newGroup = nodesUpdate.enter().append("g")
                                      .call(dragEffect)
    newGroup.append("circle")
            .attr("r",__CIRCLE_RADIUS__)
    newGroup.append("text")
            .attr("dx",__CIRCLE_RADIUS__)
            .attr("dy",__CIRCLE_RADIUS__)
            .text((node: Node) => displayText(node.component))
    nodes = newGroup.merge(nodesUpdate)
    nodes.classed(__CSS_FINISHED__, (_: Node) => analysis.finished())
         .classed(__CSS_IN_WORKLIST__, (node: Node) => analysis.work.contains(node.component))
         .classed(__CSS_NOT_VISITED__, (node: Node) => !analysis.visited.contains(node.component))
         .classed(__CSS_NEXT_COMPONENT__, (node: Node) => analysis.work.headOption == Some(node.component))
    // update the edges
    val edgesUpdate = edges.data(edgesData)
    edges = edgesUpdate.enter().append("path")
                               .attr("stroke","black")
                               .attr("stroke-width",2)
                               .attr("fill","none")
                               .attr("marker-end",s"url(#${__SVG_ARROW_ID__})")
                               .merge(edgesUpdate)
    // update the simulation
    simulation.nodes(nodesData)
    simulation.force(__FORCE_LINKS__).links(edgesData)
    simulation.alpha(1).restart()
  }

  //
  // INPUT HANDLING
  //

  def keyHandler: PartialFunction[String,Unit] = {
    case "n" => stepAnalysis()
  }

  def onClick() = stepAnalysis()

  private def stepAnalysis() =
    if (!analysis.finished()) {
      val component = analysis.work.head
      analysis.step()
      refreshDataAfterStep(component)
      refreshVisualisation()
    }

  //
  // DRAGGING
  //

  // create a fancy drag effect
  val dragEffect = d3.drag().on("start", (node: JsAny) => onDragStart(node))
                            .on("drag", (node: JsAny) => onDragDrag(node))
                            .on("end", (node: JsAny) => onDragEnd(node))

  private def onDragStart(node: JsAny) = {
    val isActive = d3.event.active.asInstanceOf[Int]
    if(isActive == 0) simulation.alphaTarget(0.3).restart()
    node.fx = node.x
    node.fy = node.y
  }
  private def onDragDrag(node: JsAny) = {
    node.fx = d3.event.x
    node.fy = d3.event.y
  }
  private def onDragEnd(node: JsAny) = {
    val isActive = d3.event.active.asInstanceOf[Int]
    if(isActive == 0) simulation.alphaTarget(0)
    node.fx = null
    node.fy = null
  }
}
