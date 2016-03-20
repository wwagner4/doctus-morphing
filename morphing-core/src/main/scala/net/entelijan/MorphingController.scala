package net.entelijan

import doctus.core._
import doctus.core.util._
import doctus.core.template._
import doctus.core.color._
import scala.annotation.tailrec
import easing.OutQuad
import easing.InOutQuad

case class Trans(startTime: Long, from: DoctusPoint, to: DoctusPoint, duration: Long) {

  def terminated(time: Long) = time - startTime > duration

}

case class Model(trans: Trans, disp: (DoctusGraphics, DoctusPoint) => Unit)

case class MorphingDoctusTemplate(canvas: DoctusCanvas, sched: DoctusScheduler) extends DoctusTemplate {

  val random = new java.util.Random
  
  
  sched.start(nextModel, 20000)

  def dispLinePrepare(angle: Int): DoctusVector = {
    val len = 20.0
    val lenh = len / 2.0
    DoctusVector(0, lenh).rot(angle * math.Pi / 180)
  }

  def dispLine(angleVector: DoctusVector)(g: DoctusGraphics, center: DoctusPoint): Unit = {
    g.line(center + angleVector, center - angleVector)
  }

  val disps = Stream.continually(random.nextInt(360)).map { angle => dispLinePrepare(angle) }

  override val frameRate = Some(10)

  val pointImages = PointImages.allImages

  var currentImg = random.nextInt(pointImages.size)
  var models = List.empty[Model]

  // Between 0.0 and 1.0
  def ran(): Double = random.nextDouble()

  def createNextModels(time: Long): Unit = {
    val w = canvas.width
    val h = canvas.height

    def createTransitions(img1: List[DoctusPoint], img2: List[DoctusPoint], startTime: Long): List[Trans] = {

      def randomOffset: DoctusVector = {
        val n = 0
        if (n == 0) DoctusVector(0, 0)
        else {
          val xIntOff = random.nextInt(2 * n + 1) - n
          val yIntOff = random.nextInt(2 * n + 1) - n
          DoctusVector(xIntOff, yIntOff)
        }
      }

      img1.zip(img2) map {
        case (a1, a2) =>
          val roff = randomOffset
          val duration = 10000
          Trans(startTime, a1, a2 + roff, duration)
      }
    }

    def createModels(transisions: List[Trans]): List[Model] = {
      transisions.zip(disps).map { case (trans, disp) => Model(trans, dispLine(disp)) }
    }

    def scale(p: DoctusPoint): DoctusPoint = {
      val r = w.toDouble / h
      if (r < 1) DoctusPoint(p.x, (1 - r) / 2 + p.y * r)
      else {
        val r1 = 1 / r
        DoctusPoint((1 - r1) / 2 + p.x * r1, p.y)
      }
    }

    val nextImage = MorphingUtil.nextIndex(currentImg, pointImages.size, random)
    
    val p1 = pointImages(currentImg).map { point => scale(point) }
    val p2 = pointImages(nextImage).map { point => scale(point) }
    val transitions = createTransitions(p1, p2, time)

    models = createModels(transitions)
    currentImg = nextImage

  }

  def draw(g: DoctusGraphics): Unit = {
    val w = canvas.width
    val h = canvas.height

    def drawBackground(g: DoctusGraphics): Unit = {
      g.fill(DoctusColorWhite, 255)
      g.rect(DoctusPoint(0, 0), w, h)
    }

    def drawModel(model: Model, time: Long): Unit = {

      def adjust(v: Double, max: Double): Double = {
        if (v < 0) {
          (1.0 - math.abs(v % 1.0)) * max
        } else {
          (v % 1.0) * max
        }
      }

      val trans = model.trans
      val t = (time - trans.startTime).toDouble
      val x = InOutQuad.calc(t, trans.from.x, trans.to.x, trans.duration)
      val y = InOutQuad.calc(t, trans.from.y, trans.to.y, trans.duration)
      val dp = DoctusPoint(adjust(x, w), adjust(y, h))
      val center = DoctusPoint(dp.x, dp.y)
      model.disp(g, center)
    }

    if (models.isEmpty) createNextModels(System.currentTimeMillis() - 100000)

    drawBackground(g)

    //g.fill(DoctusColorBlack, 10)
    //g.noStroke()
    g.stroke(DoctusColorBlack, 255)
    g.strokeWeight(1)

    models.foreach { model => drawModel(model, System.currentTimeMillis()) }
  }

  def pointableDragged(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointablePressed(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointableReleased(pos: DoctusPoint): Unit = {
    nextModel
  }

  def nextModel(): Unit = {
    val time = System.currentTimeMillis()
    if (models.isEmpty || models.forall { _.trans.terminated(time) }) {
      createNextModels(System.currentTimeMillis())
    }
  }
  
}

