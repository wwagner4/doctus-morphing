package easing

import impl._

trait Easing {
  def calc(time: Double, from: Double, to: Double, duration: Double): Double
}

object Linear extends Easing with BaseLinear with In

object InQuad extends Easing with BaseQuad with In

object OutQuad extends Easing with BaseQuad with Out

object InOutQuad extends Easing with BaseQuad with InOut

package impl {

  private[easing] trait BaseLinear {
    def _calc(x: Double): Double = x
  }

  private[easing]trait BaseQuad {
    def _calc(x: Double): Double = x * x
  }

  private[easing] trait Base {
    def calcIn(time: Double, from: Double, to: Double, duration: Double): Double = {
      if (time < 0) from
      else if (time > duration) to
      else if (from < to) from + _calc(time / duration) * (to - from)
      else to + (from - to) - _calc(time / duration) * (from - to)
    }
    def _calc(x: Double): Double
  }

  private[easing] trait In extends Base {
    def calc(time: Double, from: Double, to: Double, duration: Double): Double =
      calcIn(time, from, to, duration)
  }

  private[easing] trait Out extends Base {
    def calc(time: Double, from: Double, to: Double, duration: Double): Double = {
      calcIn(duration - time, to, from, duration)
    }
  }

  private[easing] trait InOut extends Base {
    def calc(time: Double, from: Double, to: Double, duration: Double): Double = {
      val diff = (from - to) * 0.5
      if (time < duration * 0.5) calcIn(time * 2.0, from, to + diff, duration)
      else calcIn((duration - time) * 2.0, to, from - diff, duration)
    }
  }

}

