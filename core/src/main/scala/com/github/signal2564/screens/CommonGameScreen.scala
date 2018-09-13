package com.github.signal2564.screens

import com.badlogic.gdx.graphics.OrthographicCamera
import com.badlogic.gdx.{Gdx, ScreenAdapter}

trait CommonGameScreen extends ScreenAdapter {

  val cam = new OrthographicCamera(width, height)
  cam.position.set(width / 2, height / 2, 0)

  def width = Gdx.graphics.getWidth
  def height = Gdx.graphics.getHeight

  /**
    *
    * @return tuple - (width, height)
    */
  def resolution = (width, height)

}