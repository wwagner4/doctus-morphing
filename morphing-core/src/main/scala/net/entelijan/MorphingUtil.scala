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

}