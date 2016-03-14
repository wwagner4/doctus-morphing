package net.entelijan

import utest._

/**
 * In here come your tests for the core module
 * that run in the jvm. 
 * Advanage: Debugging inside the IDE is possible
 * You can move those tests when beeing stable to the core module
 */
object CoreJvmSuite extends TestSuite {

  def tests = TestSuite {

    'core_jvm_test {
      assert(true)
    }
  }

}
