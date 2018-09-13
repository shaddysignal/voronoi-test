package com.github.signal2564.voronoi

import java.lang.Math.{abs, acos, pow, signum, sqrt}

import com.badlogic.gdx.graphics.Color

import scala.collection.immutable.HashMap
import scala.util.Random.nextFloat

/**
  * @param color default color is random, with alpha 1
  */
case class Point(x: Double, y: Double, color: Color = new Color(nextFloat, nextFloat, nextFloat, 1)) {
  val sqLength = pow(x, 2) + pow(y, 2)
  val length = sqrt(sqLength)
  lazy val normalize = new Point(x / length, y / length)

  def -(p: Point): Point = new Point(x - p.x, y - p.y)
  def +(p: Point): Point = new Point(x + p.x, y + p.x)
  def *(c: Double): Point = new Point(x * c, y * c)
  def /(c: Double): Point = new Point(x / c, y / c)

  def x(p: Point): Double = x * p.y - y * p.x
  def o(p: Point): Double = x * p.x + y * p.y
  def angleTo(p: Point): Double = signum(this x p) * acos(this.normalize o p.normalize)
  def distanceTo(p: Point): Double = (this - p).length

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case p: Point => p.x == x && p.y == y
      case _ => false
    }
  }
}

sealed trait Edge {
  val p1: Point
  val p2: Point
  val length = (p2 - p1).length
}
case class DirectedEdge(p1: Point, p2: Point) extends Edge
case class UndirectedEdge(p1: Point, p2: Point) extends Edge {
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case e: UndirectedEdge => (e.p1 == p1 && e.p2 == p2) || (e.p1 == p2 && e.p2 == p1)
      case _ => false
    }
  }
}

class PowerEdge(p1: Point, p2: Point, var leftPoint: Option[Point] = None, var rightPoint: Option[Point] = None) extends UndirectedEdge(p1, p2)
object PowerEdge {
  def apply(p1: Point, p2: Point, leftPoint: Option[Point] = None, rightPoint: Option[Point] = None): PowerEdge =
    new PowerEdge(p1, p2, leftPoint, rightPoint)

  def unapply(e: PowerEdge): Option[(Point, Point, Option[Point], Option[Point])] =
    Some((e.p1, e.p2, e.leftPoint, e.rightPoint))
}

case class Line(p1: Point, p2: Point) {
  val a = (p2.y - p1.y) / (p2.x - p1.x)
  val b = - a * p2.x + p1.y

  def apply(x: Double): Double = a * x + b
  def intercept(l: Line): Option[Point] = {
    if (a == l.a) None
    else {
      val x = - (l.b - b) / (l.a - a)
      Some(new Point(x, a * x + b))
    }
  }
}

case class Circle(center: Point, radius: Double) {
  def contains(p: Point): Boolean = (p - center).length < radius
}
object Circle {
  def fromPoints(p1: Point, p2: Point, p3: Point): Circle = {
    val cp = (p2 - p1) x (p3 - p1)
    if (cp != 0.0) {
      val p1Sq = p1.sqLength
      val p2Sq = p2.sqLength
      val p3Sq = p3.sqLength

      val cx = (p1Sq * (p2.y - p3.y) + p2Sq * (p3.y - p1.y) + p3Sq * (p1.y - p2.y)) / (2.0f * cp)
      val cy = (p1Sq * (p3.x - p2.x) + p2Sq * (p1.x - p3.x) + p3Sq * (p2.x - p1.x)) / (2.0f * cp)

      val p = new Point(cx, cy)
      new Circle(p, p.distanceTo(p1))
    } else
      throw new RuntimeException("points on one line")
  }
}

case class Polygon(points: List[Point]) {
  private val eps = 1E-5

  val edges: List[Edge] = {
    List(
      List(new UndirectedEdge(points.last, points.head)),
      for (i <- 0 until points.length - 1) yield new UndirectedEdge(points(i), points(i + 1))
    ).flatten
  }

  def contains(p: Point): Boolean = abs(2 * Math.PI - edges.foldLeft(0.0) { (a, e) => a + (e.p2 - p).angleTo(e.p1 - p) }) < eps
}

sealed trait Graph[P <: Point, E <: Edge] {
  val points: List[P]
  val edges: List[E]

  val adjust: Map[P, List[E]]

  def containsEdges(es: Seq[E]): Boolean
  def containsPoints(ps: Seq[P]): Boolean
}
case class UndirectedGraph[P <: Point, E <: UndirectedEdge] (points: List[P], edges: List[E]) extends Graph[P, E] {
  lazy val adjust =
    HashMap.newBuilder[P, List[E]]
      .++=(points.map(p => (p, edges.filter(e => e.p1 == p || e.p2 == p))))
      .result()

  override def containsEdges(es: Seq[E]): Boolean = es.forall(edges.contains)

  override def containsPoints(ps: Seq[P]): Boolean = ps.forall(points.contains)
}