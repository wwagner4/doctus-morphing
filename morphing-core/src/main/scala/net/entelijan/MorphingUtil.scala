package net.entelijan

import java.util.Random
import doctus.core.util.DoctusPoint
import doctus.core.util.DoctusVector

object MorphingUtil {

  def nextIndex(current: Int, size: Int, ran: Random): Int = {
    val next = ran.nextInt(size)
    if (next != current) next
    else nextIndex(current, size, ran)
  }
  
      // create a transition for every point of the current- and the next image
    def createTransitions(img1: List[DoctusPoint], img2: List[DoctusPoint], startTime: Long, random: Random): List[Trans] = {

      img1.zip(img2) map {
        case (a1, a2) =>
          val duration = 2000 + random.nextInt(8000)
          Trans(startTime, a1, a2, duration)
      }
    }

    // Zip all transitions with a drawing function
    def createModels(transisions: List[Trans], rotatingLineVectors: Stream[DoctusVector], drawing: Drawing[Int, DoctusVector]): List[MorphImage] = {
      transisions.zip(rotatingLineVectors).map {
        case (trans, rotatingLineVector) => MorphImage(trans, drawing.draw(rotatingLineVector))
      }
    }

    // scale every point according to the current width and height of the display
    def scale(p: DoctusPoint, w: Int, h: Int): DoctusPoint = {
      val r = w.toDouble / h
      if (r < 1) DoctusPoint(p.x, (1 - r) / 2 + p.y * r)
      else {
        val r1 = 1 / r
        DoctusPoint((1 - r1) / 2 + p.x * r1, p.y)
      }
    }



}