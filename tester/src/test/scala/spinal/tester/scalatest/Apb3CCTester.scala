package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim._

import scala.util.Random

class Apb3CCTester extends SpinalSimFunSuite{
  ghdlEnabled = false
  iverilogEnabled = false

  def doIt(name : String, pIn : Int, pOut : Int) = test(name) {
    SimConfig.compile(new Apb3CC(
      config = Apb3Config(16,8),
      inputClock = ClockDomain.external("input"),
      outputClock = ClockDomain.external("output")
    )).doSim(seed = 42) { dut =>
      dut.inputClock.forkStimulus(pIn)
      dut.outputClock.forkStimulus(pOut)
      val memRef = Array.fill(256)(Random.nextInt(256).toByte)
      val memSim = memRef.clone()

      val driver = new Apb3Driver(dut.io.input, dut.inputClock)
      val outputMonitor = new Apb3Monitor(dut.io.output, dut.outputClock) {
        override def onRead(address: BigInt): Byte = memSim(address.toInt)
        override def onWrite(address: BigInt, value: Byte): Unit = memSim(address.toInt) = value
      }
      val inputMonitor = new Apb3Listener(dut.io.input, dut.inputClock) {
        override def onRead(address: BigInt): Unit = {}
        override def onWrite(address: BigInt, value: BigInt): Unit = memRef(address.toInt) = value.toByte
      }

      driver.write(4, 0xAA)
      driver.write(8, 0xEE)
      assert(driver.read(4) == 0xAA)
      assert(driver.read(8) == 0xEE)

      for(i <- 0 until 100){
        Random.nextInt(4) match {
          case 0 => driver.write(Random.nextInt(256), Random.nextInt(256))
          case 1 => {
            val addr = Random.nextInt(256)
            val readed = driver.read(addr).toByte
            val ref = memRef(addr)
            assert(readed == ref, f"dut=$readed%x ref=$ref%x sim=${memSim(addr)}%x")
          }
          case _ => dut.inputClock.waitSampling()
        }
      }
    }
  }

  doIt("same", 10, 10)
  doIt("slower", 10, 100)
  doIt("faster", 100, 10)
}
