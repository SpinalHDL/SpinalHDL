package spinal.lib.system.dma.sg

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class SpinalSimDmaSgTester extends AnyFunSuite {
  Random.setSeed(42)
  for((name, p) <- SgDmaTestsParameter(allowSmallerStreams = false)) test(name){
      SgDmaTestsParameter.test(p)
  }
  for(testId <- 0 until 5){
    val p = SgDmaTestsParameter.random()
    test(s"random_$testId") {
      SgDmaTestsParameter.test(p)
    }
  }
}
