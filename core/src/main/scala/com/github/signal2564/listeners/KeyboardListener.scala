package com.github.signal2564.listeners

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.github.signal2564.logging.Loggable
import com.github.signal2564.state.State

final class KeyboardListener(state: State) extends InputAdapter with Loggable {
  override def keyDown(keycode: Int): Boolean = keycode match {
    case Keys.LEFT =>
      true
    case Keys.RIGHT =>
      true
    case Keys.UP =>
      true
    case Keys.DOWN =>
      true
    case Keys.ESCAPE =>
      Gdx.app.exit()
      true
    case _ => false
  }
}