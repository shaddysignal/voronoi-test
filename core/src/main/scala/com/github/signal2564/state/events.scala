package com.github.signal2564.state

sealed trait Event
case class Move(dx: Int, dy: Int) extends Event
case class Rotate(da: Double) extends Event
