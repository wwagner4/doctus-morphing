package net.entelijan

import doctus.core._
import doctus.core.util._
import doctus.core.template._
import doctus.core.color._
import scala.annotation.tailrec
import easing.OutQuad
import easing.InOutQuad
import java.util.Random

// Defines the transition of a point from one image to the next
case class Trans(startTime: Long, from: DoctusPoint, to: DoctusPoint, duration: Long) {

  def terminated(time: Long) = time - startTime > duration

}

case class Screen(width: Double, height: Double)

case class MorphModel(
  imageIndex: Int,
  transitions: List[Trans])

case class MorphingDoctusTemplate(canvas: DoctusCanvas, sched: DoctusScheduler) extends DoctusTemplate {

  val random = new java.util.Random

  val pointImages = PointImages.allImages

  sched.start(nextModel, 15000, 5000)

  val drawing: Drawing = DrawingRotatingLine

  override val frameRate = Some(20)

  var currentMorphModel = createInitialModel(canvas.width, canvas.height)

  def ran(): Double = random.nextDouble()

  // Create a transition and a drawing function for every point of the current- and the next image
  def createNextModel(morphModel: MorphModel, time: Long, w: Int, h: Int): MorphModel = {

    import MorphingUtil._

    val nextImageIndex = nextIndex(morphModel.imageIndex, pointImages.size, random)

    val p1 = pointImages(morphModel.imageIndex).map { point => scale(point, w, h) }
    val p2 = pointImages(nextImageIndex).map { point => scale(point, w, h) }
    val transitions = createTransitions(p1, p2, time, random)

    MorphModel(nextImageIndex, transitions)

  }

  // Create a transition and a drawing function for every point of the initial image
  def createInitialModel(w: Int, h: Int): MorphModel = {

    import MorphingUtil._

    val imageIndex = random.nextInt(pointImages.size)

    val img = pointImages(imageIndex).map { point => scale(point, w, h) }
    val transitions = createTransitions(img, img, 0, random)

    MorphModel(imageIndex, transitions)

  }

  def draw(g: DoctusGraphics): Unit = {
    
    val screen = Screen(canvas.width, canvas.height)

    def calcPoints(trans: Trans, time: Long): DoctusPoint = {

      // Adjust the point to the display if its x or y value is greater then w or h
      def adjust(v: Double, max: Double): Double = {
        if (v < 0) {
          (1.0 - math.abs(v % 1.0)) * max
        } else {
          (v % 1.0) * max
        }
      }

      // Calculate the current position of a point using easing functions
      val t = (time - trans.startTime).toDouble
      val x = InOutQuad.calc(t, trans.from.x, trans.to.x, trans.duration)
      val y = InOutQuad.calc(t, trans.from.y, trans.to.y, trans.duration)
      DoctusPoint(adjust(x, screen.width), adjust(y, screen.height))
    }

    val points = currentMorphModel.transitions.map { calcPoints(_, System.currentTimeMillis()) }
    drawing.draw(g, points, screen)
  }

  def pointableDragged(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointablePressed(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointableReleased(pos: DoctusPoint): Unit = nextModel()

  def nextModel(): Unit = {
    val time = System.currentTimeMillis()
    if (currentMorphModel.transitions.forall { _.terminated(time) }) {
      currentMorphModel = createNextModel(currentMorphModel, time, canvas.width, canvas.height)
    }
  }

}

trait Drawing {

  def draw(g: DoctusGraphics, point: Seq[DoctusPoint], screen: Screen): Unit

}

object DrawingRotatingLine extends Drawing {

  val random = new Random()

  val rotatingLineVectors: Stream[DoctusVector] = Stream.continually(random.nextInt(360)).map { angle => calcVec(angle) }

  def drawBackground(g: DoctusGraphics, screen: Screen): Unit = {
    g.fill(DoctusColorWhite, 255)
    g.rect(DoctusPoint(0, 0), screen.width, screen.height)
  }
  
  def calcStrokeWeight(screen: Screen): Double = screen.height.toDouble / 1500

  def draw(g: DoctusGraphics, points: Seq[DoctusPoint], screen: Screen): Unit = {
    drawBackground(g, screen)

    g.stroke(DoctusColorBlack, 255)
    g.strokeWeight(calcStrokeWeight(screen))

    points.zip(rotatingLineVectors).map {
      case (p, v) => g.line(p + v, p - v)
    }
    
  }

  def calcVec(angle: Int): DoctusVector = {
    val len = 10.0
    val lenh = len / 2.0
    DoctusVector(0, lenh).rot(angle * math.Pi / 180)
  }


}

