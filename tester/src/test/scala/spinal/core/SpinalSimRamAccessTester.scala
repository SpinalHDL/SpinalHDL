package spinal.core

import spinal.lib._
import spinal.core.sim._
import spinal.tester.SpinalSimFunSuite

import scala.util.Random

class SpinalSimRamAccessTester extends SpinalSimFunSuite {
  ghdlEnabled = false //TODO
  
  test("test1") {
    SimConfig.compile(new Component{
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
        sleep(1)
      }
      def simCheck() = {
        val address = Random.nextInt(32)
        val readed = dut.mem.getBigInt(address)
        assert(readed == model(address))
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
        sleep(1)
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

  test("test2") {
    SimConfig.compile(new Component{
      val mem = Mem(UInt(16 bits), 32).simPublic()
      val write = new Area{
        val valid = in Bool()
        val address = in(mem.addressType())
        val data = in(mem.wordType())
        val mask = in Bits(4 bits)
        mem.write(address, data, enable = valid, mask = mask)
      }
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
        sleep(1)
      }
      def simCheck() = {
        val address = Random.nextInt(32)
        assert(dut.mem.getBigInt(address) == model(address))
      }

      dut.write.valid #= false
      def dutWrite() {
        dut.clockDomain.waitSampling()
        val address = Random.nextInt(32)
        val data = Random.nextInt(1024*64)
        val mask = Random.nextInt(16)
        dut.write.valid #= true
        dut.write.address #= address
        dut.write.data #= data
        dut.write.mask #= mask
        dut.clockDomain.waitSampling()
        dut.write.valid #= false
        dut.clockDomain.waitSampling()
        var buffer = model(address)
        for(i <- 0 until 4 if (mask & (1 << i)) != 0){
          val m = 0xF << 4*i
          buffer = (buffer & ~m) | (data & m)
        }
        model(address) = buffer
      }
      def dutCheck() = {
        sleep(1)
        val address = Random.nextInt(32)
        dut.read.address #= address
        sleep(1)
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
        case _ =>
      }
      println(simTime())
    }
  }
}

