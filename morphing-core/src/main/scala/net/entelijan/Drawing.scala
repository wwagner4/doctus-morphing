package net.entelijan

import doctus.core.DoctusGraphics
import doctus.core.util.DoctusPoint
import doctus.core.util.DoctusVector
import java.util.Random
import doctus.core.color.DoctusColorWhite
import doctus.core.color.DoctusColorBlack

object DrawingRotatingLine extends Drawing {

  val random = new Random()

  val lineVectors: Stream[DoctusVector] = Stream.continually(random.nextInt(360)).map { angle => calcVec(angle) }

  def calcStrokeWeight(screen: Screen): Double = screen.height.toDouble / 1500

  def draw(g: DoctusGraphics, points: Seq[DoctusPoint], screen: Screen): Unit = {
    drawBackground(g, screen)

    g.stroke(DoctusColorBlack, 255)
    g.strokeWeight(calcStrokeWeight(screen))

    points.zip(lineVectors).map {
      case (p, v) => g.line(p + v, p - v)
    }
  }

  def calcVec(angle: Int): DoctusVector = {
    val len = 10.0
    val lenh = len / 2.0
    DoctusVector(0, lenh).rot(angle * math.Pi / 180)
  }

  def drawBackground(g: DoctusGraphics, screen: Screen): Unit = {
    g.fill(DoctusColorWhite, 255)
    g.rect(DoctusPoint(0, 0), screen.width, screen.height)
  }
  

}

