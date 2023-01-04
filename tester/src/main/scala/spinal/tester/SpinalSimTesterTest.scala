package spinal.tester

import org.scalatest.funsuite.AnyFunSuite

class SpinalSimTesterTest extends AnyFunSuite {
  SpinalSimTester{ env =>
    import env._

    test(prefix + "a"){
      println(SimConfig._backend + " " + durationFactor)
    }
  }
}
