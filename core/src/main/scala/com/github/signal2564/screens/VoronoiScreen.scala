package com.github.signal2564.screens

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.g2d._
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.{Color, GL20}
import com.github.signal2564.logging.Loggable
import com.github.signal2564.voronoi._

import scala.collection.mutable.ListBuffer
import scala.util.Random.nextInt

class VoronoiScreen(pointCount: Int) extends CommonGameScreen with Loggable {
  val shapeRenderer = new ShapeRenderer()

  val points: ListBuffer[Point] = ListBuffer.fill(pointCount) { Point(nextInt((1.5 * width).toInt) - width / 4.0, nextInt((1.5 * height).toInt) - height / 4.0) }
  points(0) = Point(width / 2, height / 2)

  override def render(delta: Float): Unit = {
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    Gdx.gl.glLineWidth(1)

    cam.update()

    val player = points.head
    if (Gdx.input.isKeyPressed(Keys.LEFT)) { points(0) = Point(player.x - 10, player.y, player.color) }
    if (Gdx.input.isKeyPressed(Keys.RIGHT)) { points(0) = Point(player.x + 10, player.y, player.color) }
    if (Gdx.input.isKeyPressed(Keys.UP)) { points(0) = Point(player.x, player.y + 10, player.color) }
    if (Gdx.input.isKeyPressed(Keys.DOWN)) { points(0) = Point(player.x, player.y - 10, player.color) }

    if (Gdx.input.isKeyPressed(Keys.ESCAPE)) { Gdx.app.exit() }

    val graph = DelaunayTriangulation(points.toList)

    // ---------------------------------------

    shapeRenderer.setProjectionMatrix(cam.projection)
    shapeRenderer.begin(ShapeRenderer.ShapeType.Line)

    shapeRenderer.setColor(Color.GREEN)
    graph.edges.foreach(e => {
      val PowerEdge(_, _, op1, op2) = e

      for (p1 <- op1; p2 <- op2) shapeRenderer.line(p1.x.toInt, p1.y.toInt, p2.x.toInt, p2.y.toInt)
    })

    shapeRenderer.setColor(Color.WHITE)
    shapeRenderer.circle(player.x.toInt, player.y.toInt, 5)

    shapeRenderer.end()

    log.info(s"FPS: ${Gdx.graphics.getFramesPerSecond}")
  }

  override def resize(width: Int, height: Int): Unit = {
    cam.viewportHeight = height
    cam.viewportWidth = width
    cam.position.set(width / 2, height / 2, 0)
  }

  override def dispose(): Unit = {
    log.debug("disposing...")
    shapeRenderer.dispose()
  }
}
