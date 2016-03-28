package net.entelijan

import doctus.core.DoctusGraphics
import doctus.core.util.DoctusPoint
import doctus.core.util.DoctusVector
import java.util.Random
import doctus.core.color.DoctusColorWhite
import doctus.core.color.DoctusColorBlack
import doctus.core.color.DoctusColorGreen

object DrawingRotatingLine extends Drawing {

  val random = new Random()
  
  val lineVectors: Stream[DoctusVector] = Stream.continually(random.nextInt(360)).map { angle => calcVec(angle) }

  def draw(g: DoctusGraphics, points: Seq[DoctusPoint], screen: Screen): Unit = {
    
    val cntFactor = 20000 / points.size
    
    val len = cntFactor * math.min(screen.height, screen.width) / 100
    val weight = cntFactor * math.min(screen.height, screen.width) / 1000

    drawBackground(g, screen)

    g.stroke(DoctusColorBlack, 255)
    g.strokeWeight(weight)
    

    points.zip(lineVectors).map {
      case (p, v) => {
        val v1 = v * len
        g.line(p + v1, p - v1)
      }
    }
  }

  def calcVec(angle: Int): DoctusVector = {
    DoctusVector(0, 1).rot(angle * math.Pi / 180)
  }

  def drawBackground(g: DoctusGraphics, screen: Screen): Unit = {
    g.fill(DoctusColorWhite, 255)
    g.rect(DoctusPoint(0, 0), screen.width, screen.height)
  }
  

}

object DrawingSquare extends Drawing {

  val random = new Random()
  val len = 20.0

  val vec = calcVec(135) 

  def draw(g: DoctusGraphics, points: Seq[DoctusPoint], screen: Screen): Unit = {
    drawBackground(g, screen)

    g.fill(DoctusColorBlack, 5)
    g.noStroke()

    points.foreach {
      p => g.rect(p + vec, len, len)
    }
  }

  def calcVec(angle: Int): DoctusVector = {
    val lenh = len / 2.0
    DoctusVector(0, lenh).rot(angle * math.Pi / 180)
  }

  def drawBackground(g: DoctusGraphics, screen: Screen): Unit = {
    g.fill(DoctusColorWhite, 255)
    g.rect(DoctusPoint(0, 0), screen.width, screen.height)
  }
  

}

