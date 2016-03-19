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

  
  def dispLinePrep(angle: Int): DoctusVector = {
    val len = 30.0
    val lenh = len / 2.0
    DoctusVector(0, lenh).rot(angle * math.Pi / 180)
  }
  
  
  def dispLine(angleVector: DoctusVector)(g: DoctusGraphics, center: DoctusPoint): Unit = {
    g.line(center + angleVector, center - angleVector)
  }

  val disps = Stream.continually(random.nextInt(360)).map { angle => dispLinePrep(angle) }

  override val frameRate = Some(20)

  val pixImages = List(PixImageFactory.no0160, PixImageFactory.no0260, PixImageFactory.no0360,
    PixImageFactory.no0460, PixImageFactory.no0560, PixImageFactory.no0760, PixImageFactory.no0860)

  val pointImages = pixImages.map { pix => pointImg(pix) }  
    
  var currentImg = 0
  var models = List.empty[Model]

  def createNextModels: Unit = {
    val time = System.currentTimeMillis()

    def createTransitions(img1: List[DoctusPoint], img2: List[DoctusPoint], startTime: Long): List[Trans] = {

      def randomOffset: DoctusVector = {
        val xIntOff = random.nextInt(3) - 1
        val yIntOff = random.nextInt(3) - 1
        DoctusVector(xIntOff, yIntOff)
      }

      img1.zip(img2) map {
        case (a1, a2) =>
          val roff = randomOffset
          val duration = 5000 + random.nextInt(10000)
          Trans(startTime, a1, a2 + roff, duration)
      }
    }

    def createModels(transisions: List[Trans]): List[Model] = {
      transisions.zip(disps).map { case (trans, disp) => Model(trans, dispLine(disp)) }
    }

    val p1 = pointImages(currentImg)
    val p2 = pointImages((currentImg + 1) % pointImages.size)
    val transitions = createTransitions(p1, p2, time)

    models = createModels(transitions)
    currentImg = (currentImg + 1) % pixImages.size

  }

  // Between 0.0 and 1.0
  def ran(): Double = random.nextDouble()

  def pointImg(pi: PixImage): List[DoctusPoint] = {
    createPoints(5000, List.empty[DoctusPoint], pi)
  }

  case class Scale(scalex: Double, scaley: Double, offx: Double, offy: Double)

  @tailrec
  private def createPoints(cnt: Int, points: List[DoctusPoint], img: PixImage): List[DoctusPoint] = {
    if (cnt == 0) points
    else {
      val x = ran();
      val y = ran();
      if (isPoint(x, y, img)) {
        val p = DoctusPoint(x, y)
        createPoints(cnt - 1, p :: points, img)
      } else {
        createPoints(cnt, points, img)
      }
    }
  }

  def isPoint(x: Double, y: Double, img: PixImage): Boolean = {
    val i: Int = math.floor(x * img.width).toInt
    val j: Int = math.floor(y * img.height).toInt
    val index = i * img.height + j
    val bright = img.pixels(index)
    val ranBright = ran()
    ranBright > bright
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
      val dp = if (t < trans.duration) {
        val x = InOutQuad.calc(t, trans.from.x, trans.to.x, trans.duration)
        val y = InOutQuad.calc(t, trans.from.y, trans.to.y, trans.duration)
        DoctusPoint(adjust(x, w), adjust(y, h))
      } else {
        val x = model.trans.to.x
        val y = model.trans.to.y
        DoctusPoint(adjust(x, w), adjust(y, h))
      }
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

