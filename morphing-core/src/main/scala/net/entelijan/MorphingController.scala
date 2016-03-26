package net.entelijan

import doctus.core._
import doctus.core.util._
import doctus.core.template._
import doctus.core.color._
import scala.annotation.tailrec
import easing.OutQuad
import easing.InOutQuad

// Defines the transition of a point from one image to the next
case class Trans(startTime: Long, from: DoctusPoint, to: DoctusPoint, duration: Long) {

  def terminated(time: Long) = time - startTime > duration

}

case class MorphImage(trans: Trans, draw: (DoctusGraphics, DoctusPoint) => Unit)

case class MorphModel(
    imageIndex: Int,
    images: List[MorphImage])

case class MorphingDoctusTemplate(canvas: DoctusCanvas, sched: DoctusScheduler) extends DoctusTemplate {

  val random = new java.util.Random

  val pointImages = PointImages.allImages

  sched.start(nextModel, 15000, 5000)

  val drawing: Drawing[Int, DoctusVector] = DrawingRotatingLine

  val rotatingLineVectors: Stream[DoctusVector] = Stream.continually(random.nextInt(360)).map { angle => drawing.prepareDrawing(angle) }

  override val frameRate = Some(20)

  var currentMorphModel = createInitialModel(canvas.width, canvas.height)

  def ran(): Double = random.nextDouble()

  // Create a transition and a drawing function for every point of the current- and the next image
  def createNextModel(morphModel:MorphModel, time: Long, w: Int, h: Int): MorphModel = {
    
    import MorphingUtil._

    val nextImageIndex = nextIndex(morphModel.imageIndex, pointImages.size, random)

    val p1 = pointImages(morphModel.imageIndex).map { point => scale(point, w, h) }
    val p2 = pointImages(nextImageIndex).map { point => scale(point, w, h) }
    val transitions = createTransitions(p1, p2, time, random)

    val models = createModels(transitions, rotatingLineVectors, drawing)
    
    MorphModel(nextImageIndex, models)

  }

  // Create a transition and a drawing function for every point of the initial image
  def createInitialModel(w: Int, h: Int): MorphModel = {
    
    import MorphingUtil._

    val imageIndex = random.nextInt(pointImages.size)

    val img = pointImages(imageIndex).map { point => scale(point, w, h) }
    val transitions = createTransitions(img, img, 0, random)

    val models = createModels(transitions, rotatingLineVectors, drawing)
    
    MorphModel(imageIndex, models)

  }

  def draw(g: DoctusGraphics): Unit = {
    val w = canvas.width
    val h = canvas.height

    def drawBackground(g: DoctusGraphics): Unit = {
      g.fill(DoctusColorWhite, 255)
      g.rect(DoctusPoint(0, 0), w, h)
    }

    def drawModel(model: MorphImage, time: Long): Unit = {

      // Adjust the point to the display if its x or y value is greater then w or h
      def adjust(v: Double, max: Double): Double = {
        if (v < 0) {
          (1.0 - math.abs(v % 1.0)) * max
        } else {
          (v % 1.0) * max
        }
      }

      val trans = model.trans
      // Calculate the current position of a point using easing functions
      val t = (time - trans.startTime).toDouble
      val x = InOutQuad.calc(t, trans.from.x, trans.to.x, trans.duration)
      val y = InOutQuad.calc(t, trans.from.y, trans.to.y, trans.duration)
      val dp = DoctusPoint(adjust(x, w), adjust(y, h))
      val center = DoctusPoint(dp.x, dp.y)
      model.draw(g, center)
    }

    drawBackground(g)

    g.stroke(DoctusColorBlack, 255)
    g.strokeWeight(h.toDouble / 1500)

    currentMorphModel.images.foreach { model => drawModel(model, System.currentTimeMillis()) }
  }

  def pointableDragged(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointablePressed(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointableReleased(pos: DoctusPoint): Unit = {
    nextModel()
  }

  def nextModel(): Unit = {
    val time = System.currentTimeMillis()
    if (currentMorphModel.images.forall { _.trans.terminated(time) }) {
      currentMorphModel = createNextModel(currentMorphModel, time, canvas.width, canvas.height)
    }
  }

}

// Todo use Generics in Template
trait Drawing[A, B] {

  def prepareDrawing(angle: A): B

  def draw(angleVector: B)(g: DoctusGraphics, point: DoctusPoint): Unit

}

object DrawingRotatingLine extends Drawing[Int, DoctusVector] {

  def prepareDrawing(angle: Int): DoctusVector = {
    val len = 10.0
    val lenh = len / 2.0
    DoctusVector(0, lenh).rot(angle * math.Pi / 180)
  }

  def draw(angleVector: DoctusVector)(g: DoctusGraphics, point: DoctusPoint): Unit = {
    g.line(point + angleVector, point - angleVector)
  }

}

