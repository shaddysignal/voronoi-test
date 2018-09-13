package com.github.signal2564

import com.badlogic.gdx.Game
import com.github.signal2564.logging.{LogLevel, Loggable, Logger}
import com.github.signal2564.screens.VoronoiScreen

class MyGame extends Game with Loggable {

  override def create() = {
    Logger.setLevel(LogLevel.DEBUG)

    log.info("creating game...")

    this.setScreen(new VoronoiScreen(50))
  }

  override def dispose(): Unit = {
    super.dispose()
  }
}