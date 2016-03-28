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

sealed trait State

case object State_Morphing extends State
case class State_Waiting(since: Long) extends State

case class Screen(width: Double, height: Double)

case class MorphModel(
  imageIndex: Int,
  transitions: List[Trans])

case class MorphingDoctusTemplate(canvas: DoctusCanvas, sched: DoctusScheduler) extends DoctusTemplate {

  val random = new java.util.Random

  val pointImages = PointImages.allImages

  val drawing: Drawing = DrawingRotatingLine

  override val frameRate = Some(20)

  var currentMorphModel = createInitialModel(Screen(canvas.width, canvas.height))

  var state: State = State_Waiting(System.currentTimeMillis())

  sched.start(nextModelAuto, 213)

  def ran(): Double = random.nextDouble()

  // Create a transition and a drawing function for every point of the current- and the next image
  def createNextModel(morphModel: MorphModel, time: Long, screen: Screen): MorphModel = {

    import MorphingUtil._

    val nextImageIndex = nextIndex(morphModel.imageIndex, pointImages.size, random)

    val p1 = pointImages(morphModel.imageIndex).map { point => scale(point, screen) }
    val p2 = pointImages(nextImageIndex).map { point => scale(point, screen) }
    val transitions = createTransitions(p1, p2, time, random)

    MorphModel(nextImageIndex, transitions)

  }

  // Create a transition and a drawing function for every point of the initial image
  def createInitialModel(screen: Screen): MorphModel = {

    import MorphingUtil._

    val imageIndex = random.nextInt(pointImages.size)

    val img = pointImages(imageIndex).map { point => scale(point, screen) }
    val transitions = createTransitions(img, img, 0, random)

    MorphModel(imageIndex, transitions)

  }

  // scale every point according to the current width and height of the display
  def scale(p: DoctusPoint, screen: Screen): DoctusPoint = {
    val r = screen.width / screen.height
    if (r < 1) DoctusPoint(p.x, (1 - r) / 2 + p.y * r)
    else {
      val r1 = 1 / r
      DoctusPoint((1 - r1) / 2 + p.x * r1, p.y)
    }
  }

  // create a transition for every point of the current- and the next image
  def createTransitions(img1: List[DoctusPoint], img2: List[DoctusPoint], startTime: Long, random: Random): List[Trans] = {

    img1.zip(img2) map {
      case (a1, a2) =>
        val duration = 2000
        Trans(startTime, a1, a2, duration)
    }
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

  def keyPressed(code: DoctusKeyCode): Unit = {
    code match {
      case DKC_Space => nextModel()
      case _ => // nothing to do
    }
  }

  def pointableDragged(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointablePressed(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointableReleased(pos: DoctusPoint): Unit = nextModel()

  def nextModel(): Unit = {
    val time = System.currentTimeMillis()
    state match {
      case State_Waiting(_) =>
        val screen = Screen(canvas.width, canvas.height)
        currentMorphModel = createNextModel(currentMorphModel, time, screen)
        state = State_Morphing
      case State_Morphing => // Nothing to do
    }
  }

  def nextModelAuto(): Unit = {
    val time = System.currentTimeMillis()
    state match {
      case State_Morphing =>
        if (currentMorphModel.transitions.forall { _.terminated(time) }) {
          state = State_Waiting(time)
        }

      case State_Waiting(since) =>
        if (time - since > 4000) {
          val screen = Screen(canvas.width, canvas.height)
          currentMorphModel = createNextModel(currentMorphModel, time, screen)
          state = State_Morphing
        }

    }
  }

}

trait Drawing {

  def draw(g: DoctusGraphics, point: Seq[DoctusPoint], screen: Screen): Unit

}
