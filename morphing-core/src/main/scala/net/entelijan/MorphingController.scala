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

case class MorphingDoctusTemplate(canvas: DoctusCanvas) extends DoctusTemplate {

  val random = new java.util.Random

  def dispLinePrepare(angle: Int): DoctusVector = {
    val len = 10.0
    val lenh = len / 2.0
    DoctusVector(0, lenh).rot(angle * math.Pi / 180)
  }

  def dispLine(angleVector: DoctusVector)(g: DoctusGraphics, center: DoctusPoint): Unit = {
    g.line(center + angleVector, center - angleVector)
  }

  val disps = Stream.continually(random.nextInt(360)).map { angle => dispLinePrepare(angle) }

  override val frameRate = Some(20)

  val pixImages = List(PixImageFactory.no0160, PixImageFactory.no0260, PixImageFactory.no0360,
    PixImageFactory.no0460, PixImageFactory.no0560, PixImageFactory.no0760, PixImageFactory.no0860)

  val pointImages = pixImages.map { pix => createPointImg(pix) }

  var currentImg = 0
  var models = List.empty[Model]

  // Between 0.0 and 1.0
  def ran(): Double = random.nextDouble()

  def createPointImg(pi: PixImage): List[DoctusPoint] = {

    @tailrec
    def createPoints(cnt: Int, points: List[DoctusPoint], img: PixImage): List[DoctusPoint] = {

      def isPoint(x: Double, y: Double, img: PixImage): Boolean = {
        val i: Int = math.floor(x * img.width).toInt
        val j: Int = math.floor(y * img.height).toInt
        val index = i * img.height + j
        val bright = img.pixels(index)
        val ranBright = ran()
        ranBright > bright
      }

      if (cnt == 0) points
      else {
        val x = ran();
        val y = ran();
        if (isPoint(x, y, img)) {
          val r = img.width.toDouble / img.height
          val p =
            if (r < 1.0) DoctusPoint((1 - r) / 2 + x * r, y)
            else DoctusPoint(x, (r - 1) / 2 + y / r)
          createPoints(cnt - 1, p :: points, img)
        } else {
          createPoints(cnt, points, img)
        }
      }
    }

    createPoints(10000, List.empty[DoctusPoint], pi)
  }

  def createNextModels: Unit = {
    val time = System.currentTimeMillis()
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

    val p1 = pointImages(currentImg).map { point => scale(point) }
    val p2 = pointImages((currentImg + 1) % pointImages.size).map { point => scale(point) }
    val transitions = createTransitions(p1, p2, time)

    models = createModels(transitions)
    currentImg = (currentImg + 1) % pixImages.size

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

    if (models.isEmpty) createNextModels

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
    val time = System.currentTimeMillis()

    if (models.isEmpty || models.forall { _.trans.terminated(time) }) {
      createNextModels
    }
  }

}

