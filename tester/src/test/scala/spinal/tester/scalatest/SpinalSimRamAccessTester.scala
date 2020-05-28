package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random

class SpinalSimRamAccessTester extends SpinalSimFunSuite {
  ghdlEnabled = false //TODO
  
  test("test1") {
    SimConfig.withWave.compile(new Component{
      val mem = Mem(UInt(16 bits), 32).simPublic()
      val write = slave(mem.writePort)
      val read = new Area{
        val address = in(mem.addressType())
        val data = out(mem.readAsync(address))
      }
    }).doSim{ dut =>
      val model = Array.fill(32)(Random.nextInt(1024*64))
      for ((v, i) <- model.zipWithIndex) dut.mem.setBigInt(i, v)

      def simWrite() {
        val address = Random.nextInt(32)
        val data = Random.nextInt(1024*64)
        dut.mem.setBigInt(address, data)
        model(address) = data
      }
      def simCheck() = {
        val address = Random.nextInt(32)
        dut.mem.getBigInt(address) == model(address)
      }

      dut.write.valid #= false
      def dutWrite() {
        dut.clockDomain.waitSampling()
        val address = Random.nextInt(32)
        val data = Random.nextInt(1024*64)
        dut.write.valid #= true
        dut.write.address #= address
        dut.write.data #= data
        dut.clockDomain.waitSampling()
        dut.write.valid #= false
        dut.clockDomain.waitSampling()
        model(address) = data
      }
      def dutCheck() = {
        sleep(1)
        val address = Random.nextInt(32)
        dut.read.address #= address
        if(Random.nextBoolean()) sleep(1) else dut.clockDomain.waitSampling()
        assert(dut.read.data.toInt == model(address))
      }


      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(10)
      for(i <- 0 until 1000) Random.nextInt(4) match {
        case 0 => simWrite()
        case 1 => simCheck()
        case 2 => dutWrite()
        case 3 => dutCheck()
      }
      println(simTime())
    }
  }
}