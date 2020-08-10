package com.github.signal2564.screens

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.github.signal2564.logging.Loggable

final class GameScreen extends CommonGameScreen with Loggable {
  log.debug("Initializing game screen...")

  val shapeRenderer = new ShapeRenderer()

  var x = 0
  var y = 0

  override def render(df: Float): Unit = {
    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
    Gdx.gl.glLineWidth(1)

    // Updating camera and renderer, important when camera can move
    cam.update()
    shapeRenderer.setProjectionMatrix(cam.projection)

    shapeRenderer.begin(ShapeRenderer.ShapeType.Filled)
    shapeRenderer.setColor(1, 1, 1, 1)
    shapeRenderer.circle(x, y, 20, 16)
    shapeRenderer.end()

    if (Gdx.input.isKeyPressed(Keys.LEFT)) { x -= 1 }
    if (Gdx.input.isKeyPressed(Keys.RIGHT)) { x += 1 }
    if (Gdx.input.isKeyPressed(Keys.UP)) { y += 1 }
    if (Gdx.input.isKeyPressed(Keys.DOWN)) { y -= 1 }

    if (Gdx.input.isKeyPressed(Keys.ESCAPE)) { Gdx.app.exit() }
  }

  override def dispose(): Unit = {
    log.debug("disposing screen...")
    shapeRenderer.dispose()
  }
}
