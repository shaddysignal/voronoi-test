package com.github.signal2564.voronoi

import scala.collection.mutable.ListBuffer

// TODO: Make cleaner, refine again to more immutable style
object DelaunayTriangulation {
  /**
    * @return Undirected graph, where each edge represent 2 edges.</br>
    *         <ul>
    *           <li>1 - from triangulation</li>
    *           <li>2 - None if 1 edge adjust only to one triangle, otherwise edge between centers of 2 triangles</li>
    *         </ul>
    */
  def apply(points: List[Point]): UndirectedGraph[Point, PowerEdge] = {
    val edges: ListBuffer[PowerEdge] = ListBuffer()

    def getBestPoints(e: PowerEdge): (Option[Point], Option[Point]) = {
      val PowerEdge(p1, p2, left, right) = e

      /**
        * Find point for which `(p2 - p1) x (p - p1) > 0` and
        * three points (p1, p2, p) create a circle, which does not contains
        * any other point, from `points`
        *
        * @return p - point which meets requirements
        */
      def collectPoint(p1: Point, p2: Point): Option[Point] = {
        /**
          * @return first point for which from.find return None
          */
        def loop(p: Point, from: Stream[Point]): Point = {
          val c = Circle.fromPoints(p1, p2, p)
          val ip = from.find(c.contains)

          if (ip.nonEmpty) loop(ip.get, from.filter(_ != p))
          else p
        }

        val filteredPoints = points.toStream.filter(p => p != p1 && p != p2)

        for (p <- filteredPoints.find(p => ((p2 - p1) x (p - p1)) > 0.0)) yield loop(p, filteredPoints)
      }

      (left, right) match {
        case (Some(_), Some(_)) => (None, None) // Edge full
        case (Some(_), None) => (None, collectPoint(p2, p1)) // Invert edge for right side
        case (None, Some(_)) => (collectPoint(p1, p2), None)
        case (None, None) => (collectPoint(p1, p2), collectPoint(p2, p1))
      }
    }

    // Warning: dirty function
    // TODO: Make it cleaner
    def completeFacetForEdge(e: PowerEdge): List[PowerEdge] = {
      val newEdges: ListBuffer[PowerEdge] = ListBuffer()

      def updateFace(e: PowerEdge, start: Point, c: Point): Unit = {
        if (e.p1 == start && e.leftPoint.isEmpty) e.leftPoint = Some(c)
        else if (e.p2 == start && e.rightPoint.isEmpty) e.rightPoint = Some(c)
      }

      // Warning: dirty function
      def createTriangle(p1: Point, p2: Point, bp: Point): Unit = {
        val circle = Circle.fromPoints(p1, p2, bp)

        updateFace(e, p1, circle.center)
        val me1 = edges.find(PowerEdge(bp, p1).equals)
        if (me1.isEmpty) newEdges += new PowerEdge(bp, p1)
        else updateFace(me1.get, bp, circle.center)

        val me2 = edges.find(PowerEdge(p2, bp).equals)
        if (me2.isEmpty) newEdges += new PowerEdge(p2, bp)
        else updateFace(me2.get, p2, circle.center)
      }

      val PowerEdge(p1, p2, _, _) = e
      val (obp1, obp2) = getBestPoints(e)

      // First best point is left, second is right
      // because of necessity of ordering and bidirectional edges
      // we invert points for left and right side of edge
      for (bp1 <- obp1) createTriangle(p1, p2, bp1)
      for (bp2 <- obp2) createTriangle(p2, p1, bp2)

      newEdges.toList
    }

    // 1. Select starting edge
    val (sp1, sp2) = points.toStream
      .map(p1 => (p1, points.toStream.filter(_ != p1).minBy(p2 => p1.distanceTo(p2))))
      .minBy(pair => {
        val (p1, p2) = pair
        p1.distanceTo(p2)
      })

    edges += new PowerEdge(sp1, sp2)

    // 2. Connecting dots
    // Current edge
    var ce = 0
    while (ce < edges.length) {
      edges ++= completeFacetForEdge(edges(ce))
      ce += 1
    }

    val voroniEdges = edges.toList
    val voroniPoints = voroniEdges.flatMap(e => List(Some(e.p1), Some(e.p2), e.rightPoint, e.leftPoint).flatten).distinct

    UndirectedGraph(voroniPoints, voroniEdges)
  }
}
