package net.entelijan

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import scala.BigDecimal
import java.io.File

object Images extends App {

  for (file <- new File("src/test/resources").listFiles()) {
    if (!file.getName.startsWith(".")) {
      val img = createImg(file)
      println(formatPixImage(img, extractName(file.getName)))
    }
  }

  def extractName(fileName: String): String = {
    val len = fileName.length()
    fileName.substring(0, len - 4)
  }

  def formatGroups(groups: Seq[Seq[Double]]): String = groups match {
    case Nil         => ""
    case grp :: Nil  => grp.mkString(", ")
    case grp :: rest => grp.mkString("", ", ", ",") + "\n    " + formatGroups(rest)
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

}