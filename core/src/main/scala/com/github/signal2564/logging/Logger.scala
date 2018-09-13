package com.github.signal2564.logging

import com.badlogic.gdx.{Application, Gdx}
import com.github.signal2564.logging.LogLevel.{NONE, DEBUG, INFO, ERROR}

trait Loggable {
  protected lazy val log = Logger(this.getClass)
}

final class Logger private (tag: String) {
  def info(s: String): Unit = Gdx.app.log(tag, s)
  def error(s: String, e: Exception): Unit = Gdx.app.error(tag, s, e)
  def debug(s: String): Unit = Gdx.app.debug(tag, s)
}
object Logger {
  def apply(clazz: Class[_]): Logger = new Logger(clazz.getCanonicalName)
  def setLevel(logLevel: LogLevel.Value): Unit = Gdx.app.setLogLevel(logLevel.gdxLevel)
  def getLevel: LogLevel.Value = Gdx.app.getLogLevel match {
    case Application.LOG_NONE => NONE
    case Application.LOG_INFO => INFO
    case Application.LOG_DEBUG => DEBUG
    case Application.LOG_ERROR => ERROR
  }
}

object LogLevel {
  sealed abstract class Value(val gdxLevel: Int)
  case object NONE extends Value(Application.LOG_NONE)
  case object INFO extends Value(Application.LOG_INFO)
  case object DEBUG extends Value(Application.LOG_DEBUG)
  case object ERROR extends Value(Application.LOG_ERROR)
}
