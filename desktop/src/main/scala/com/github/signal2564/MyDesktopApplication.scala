package com.github.signal2564

import com.badlogic.gdx.backends.lwjgl.{LwjglApplication, LwjglApplicationConfiguration}

object MyDesktopApplication extends App {
  val cfg = new LwjglApplicationConfiguration
  cfg.title = "voronoi-shooter"
  cfg.fullscreen = false
  cfg.width = 800
  cfg.height = 600
  cfg.forceExit = false
  new LwjglApplication(new MyGame, cfg)
}
