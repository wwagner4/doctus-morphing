package net.entelijan

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import scala.BigDecimal
import java.io.File
import doctus.core.util.DoctusPoint
import java.util.Random
import scala.annotation.tailrec

object PointImageGenerator extends App {
  
  case class PixImage(width: Int, height: Int, pixels: Seq[Double])

  case class PointImage(name: String, points: List[DoctusPoint])

  val pointCnt = 20000

  val random = new Random()
  def ran(): Double = random.nextDouble()

  val imageFiles: List[File] =
    new File("src/test/resources")
      .listFiles().toList
      .filter { f => !f.getName.startsWith(".") }

  val pointImages =
    for (file <- imageFiles) yield {
      val img = createImg(file)
      val name = extractName(file.getName)
      PointImage(name, createPointImg(img))
    }
  
  println(formatPointImageObject(pointImages))
  
  def formatPointImageObject(pointImages: List[PointImage]): String = {
    
    val methods = pointImages.map { pimg => formatPointImage(pimg) }.mkString("\n")
    
    val allImageNames = pointImages.map { pimg => pimg.name }.mkString(",")
   
s"""
package net.entelijan

import doctus.core.util.DoctusPoint


object PointImages {

  def allImages: List[List[DoctusPoint]] = List($allImageNames)

$methods
}""" 
  }
  

  def extractName(fileName: String): String = {
    val len = fileName.length()
    fileName.substring(0, len - 4)
  }

  def formatGroups[T](groups: Seq[Seq[T]]): String = groups match {
    case Nil         => ""
    case grp :: Nil  => grp.mkString(", ")
    case grp :: rest => grp.mkString("", ", ", ",") + "\n    " + formatGroups(rest)
  }

  def formatGroups1[T](groups: Seq[Seq[T]], index: Int, name: String): String = groups match {
    case Nil         => ""
    case grp :: Nil  => formatSubMethod(name, index, grp)
    case grp :: rest => formatSubMethod(name, index, grp) + formatGroups1(rest, index + 1, name)
  }

  def formatSubMethod[T](name: String, index: Int, grp: Seq[T]): String = {
    s"""       def $name$index = List(${grp.mkString(", ")})
      """
  }

  def formatPixImage(img: PixImage, name: String): String = {
    val w = img.width
    val h = img.height

    val pixels = formatGroups(img.pixels.grouped(30).toList)

    s"""
  def $name: PixImage = PixImage($w, $h, Seq(
    $pixels)
  )
      """

  }

  def formatPoint(dp: DoctusPoint): String = {
    "DoctusPoint(%.3f, %.3f)" format (dp.x, dp.y)
  }

  def formatPointImage(pointImg: PointImage): String = {

    val name = pointImg.name
    val imgStr = pointImg.points.map { img => formatPoint(img) }
    val groups = imgStr.grouped(100).toList
    val subMethods = formatGroups1(groups, 0, name)
    val subMethodNames = formatSubMethodNames(name, groups.size)

    s"""
  def $name: List[DoctusPoint] = {
      $subMethods
      List($subMethodNames).flatten
    }
      """

  }

  def formatSubMethodName(name: String, i: Int) = s"$name$i"

  def formatSubMethodNames(name: String, n: Int): String = {
    val names = (0 until n).map { i => formatSubMethodName(name, i) }
    names.toList.mkString(", ")
  }

  def createImg(img: File): PixImage = {
    val bi: BufferedImage = ImageIO.read(img)
    val pixels = for (x <- 0 until bi.getWidth; y <- 0 until bi.getHeight) yield {
      val color = bi.getRGB(x, y);
      val red = (color >>> 16) & 0xFF
      val green = (color >>> 8) & 0xFF
      val blue = (color >>> 0) & 0xFF
      val re = (red.toDouble * 0.2126f + green * 0.7152f + blue * 0.0722f) / 255
      BigDecimal(re).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
    PixImage(bi.getWidth, bi.getHeight, pixels)
  }

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

    createPoints(pointCnt, List.empty[DoctusPoint], pi)
  }

}
