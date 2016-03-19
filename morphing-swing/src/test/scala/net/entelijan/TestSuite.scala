package net.entelijan

import org.scalatest.FunSuite
import java.util.Random
import scala.annotation.tailrec

class TestSuite extends FunSuite {
  
  val r = new Random()
  
  test("Select next index") {
    for(_ <- 1 to 100) {
      val ci = r.nextInt(10)
      val ni = MorphingUtil.nextIndex(ci, 10, r)
      assert(ni != ci)
    }
    
  }
  
  
}