package com.github.signal2564.bezier

import com.github.signal2564.voronoi.Point

/**
  * @param step   contains in [0, 1]
  * @param points which lay on curve
  */
case class BezierLine(step: Double, points: List[Point]) {
  assert(step >= 0 && step <= 1)

  // TODO: to be done
  val boundPoints: List[Point] = {
    val A: Array[Array[Double]] = points.indices.toArray.map(i => points.indices.toArray.map(j => math.pow(points(i).x, j)))
    val B: Array[Double] = points.indices.toArray.map(i => points(i).y)

    val AB: Array[Array[Double]] = (0 until points.length + 1).toArray.map(i => Array(A(i), Array(B(i))).flatten)

    val N: Int = B.length

    for (p <- 0 until N) {
      val SAB = AB.sortBy(_ (p))

      // pivot within A and B
      for (i <- p + 1 until N) {
        val alpha: Double = SAB(i)(p) / SAB(p)(p)
//        B(i) -= alpha * B(p)
        for (j <- p until N) {
          SAB(i)(j) -= alpha * SAB(p)(j)
        }
      }
    }

    // back substitution
    val x: Array[Double] = Array.fill(N) {
      0.0
    }
    for (i <- N - 1 to 0 by -1) {
      var sum: Double = 0.0
      for (j <- i + 1 until N) {
        sum += A(i)(j) * x(j)
      }
      x(i) = (B(i) - sum) / A(i)(i)
    }

    List()
  }

  def collectDots: List[Point] = {
    def acquireDot(u: Double, points: List[Point]): Point = {
      if (points.length == 1) points.head
      else acquireDot(u, (0 until points.length - 1).toList.map(i => (points(i + 1) - points(i)) * u + points(i)))
    }

    (0 to (1 / step).toInt).toList.map(i => acquireDot(step * i, points))
  }
}