package net.entelijan

import doctus.core._
import doctus.core.util._
import doctus.core.template._
import doctus.core.color._
import scala.annotation.tailrec

case class Trans(startTime: Long, from: DoctusPoint, to: DoctusPoint, duration: Long, disp: Int) {

  def terminated(time: Long) = time - startTime > duration

}

case class MorphingDoctusTemplate(canvas: DoctusCanvas) extends DoctusTemplate {

  val random = new java.util.Random

  override val frameRate = Some(20)

  val pixImages = List(PixImageFactory.no0160, PixImageFactory.no0260, PixImageFactory.no0360,
    PixImageFactory.no0460, PixImageFactory.no0560, PixImageFactory.no0660, PixImageFactory.no0760, PixImageFactory.no0860)

  var currentImg = 0
  var transitions = List.empty[Trans]

  def createNextTrans: Unit = {
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
          val duration = 1000 + random.nextInt(1000)
          Trans(startTime, a1, a2 + roff, duration, random.nextInt(8))
      }
    }

    val i1 = pixImages(currentImg)
    val i2 = pixImages((currentImg + 1) % pixImages.size)
    val p1 = pointImg(i1)
    val p2 = pointImg(i2)
    transitions = createTransitions(p1, p2, time)
    currentImg = (currentImg + 1) % pixImages.size

  }

  // Between 0.0 and 1.0
  def ran(): Double = random.nextDouble()

  case class Adjust(scalex: Double, scaley: Double, offx: Double, offy: Double)

  def pointImg(pi: PixImage): List[DoctusPoint] = {
    val scale = calcScale(canvas.width, canvas.height, pi.width, pi.height)
    createPoints1(5000, List.empty[DoctusPoint], pi, scale)
  }

  def calcScale(canvW: Double, canvH: Double, imgW: Double, imgH: Double): Adjust = {
    val canvasR = canvW / canvH
    val imgR = imgW / imgH
    if (canvasR > imgR) Adjust(imgR, 1.0, math.abs(1 - imgR) * 0.5, 0)
    else Adjust(1.0, imgR, 0, math.abs(1 - imgR) * 0.5)
  }

  @tailrec
  private def createPoints1(cnt: Int, points: List[DoctusPoint], img: PixImage, adj: Adjust): List[DoctusPoint] = {
    if (cnt == 0) points
    else {
      val x = ran();
      val y = ran();
      if (isPoint(x, y, img)) {
        val p = DoctusPoint(adj.offx + x * adj.scalex, adj.offy + y * adj.scaley)
        createPoints1(cnt - 1, p :: points, img, adj)
      } else {
        createPoints1(cnt, points, img, adj)
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
      g.fill(DoctusColorWhite, 100)
      g.rect(DoctusPoint(0, 0), w, h)
    }
    
    def createLine(center: DoctusPoint, disp: Int): (DoctusPoint, DoctusPoint) = {
      val len = 5.0
      val lenh = len / 2.0
      disp match {
        case 0 => (center + DoctusVector(len, 0), center + DoctusVector(-len, 0))
        case 1 => (center + DoctusVector(0, lenh), center + DoctusVector(0, -lenh))
        case 2 => (center + DoctusVector(len, len), center + DoctusVector(-len, -len))
        case 3 => (center + DoctusVector(lenh, len), center + DoctusVector(-lenh, -len))
        case 4 => (center + DoctusVector(0, len), center + DoctusVector(0, -len))
        case 5 => (center + DoctusVector(-lenh, len), center + DoctusVector(lenh, -len))
        case 6 => (center + DoctusVector(-len, len), center + DoctusVector(len, -len))
        case 7 => (center + DoctusVector(-len, lenh), center + DoctusVector(len, -lenh))
        case _ => throw new IllegalArgumentException("disp must be 0-7")
      }
    }

    def drawTrans(trans: Trans, time: Long): Unit = {
      def adj(v: Double, max: Double): Double = {
        if (v < 0) {
          (1.0 - math.abs(v % 1.0)) * max
        } else {
          (v % 1.0) * max
        }
      }

      val t = (time - trans.startTime).toDouble
      val dp = if (t < trans.duration) {
        val x = trans.from.x + (t / trans.duration) * (trans.to.x - trans.from.x)
        val y = trans.from.y + (t / trans.duration) * (trans.to.y - trans.from.y)
        DoctusPoint(adj(x, w), adj(y, h))
      } else {
        val x = trans.to.x
        val y = trans.to.y
        DoctusPoint(adj(x, w), adj(y, h))
      }
      val center = DoctusPoint(dp.x, dp.y)
      val (from, to) = createLine(center, trans.disp)
      g.line(from, to)
    }

    def currentTime: Long = System.currentTimeMillis()

    val time = currentTime
    if (transitions.isEmpty) {
      createNextTrans
    }

    drawBackground(g)
    
    //g.fill(DoctusColorBlack, 10)
    //g.noStroke()
    
    g.stroke(DoctusColorBlack, 150)
    g.strokeWeight(1)

    transitions.foreach { trans => drawTrans(trans, time) }
  }

  def pointableDragged(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointablePressed(pos: DoctusPoint): Unit = () // Nothing to do here

  def pointableReleased(pos: DoctusPoint): Unit = {
    val time = System.currentTimeMillis()

    if (transitions.isEmpty || transitions.forall { _.terminated(time) }) {
      createNextTrans
    }
  }

}

