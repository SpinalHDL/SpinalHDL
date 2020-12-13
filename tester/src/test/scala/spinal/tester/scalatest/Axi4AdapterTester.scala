package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import org.scalatest.FunSuite
import spinal.core.sim.SimCompiled
import spinal.lib.bus.amba4.axi.sim.{Axi4ReadOnlyMasterAgent, Axi4ReadOnlyMonitor, Axi4ReadOnlySlaveAgent, Axi4WriteOnlyMasterAgent, Axi4WriteOnlyMonitor, Axi4WriteOnlySlaveAgent}
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnlyUpsizer, Axi4WriteOnlyUpsizer}
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.util.Random

class Axi4UpsizerTester extends FunSuite {

  def writeTester(dut : Axi4WriteOnlyUpsizer): Unit ={
    dut.clockDomain.forkStimulus(10)


    val regions = mutable.Set[SizeMapping]()
    val inputAgent = new Axi4WriteOnlyMasterAgent(dut.io.input, dut.clockDomain) {
      override def genAddress(): BigInt = ((Random.nextInt(1 << 19)))// & 0xFFF00) | 6

      override def bursts: List[Int] = List(1)

      override def mappingAllocate(mapping: SizeMapping): Boolean = {
        if(regions.exists(_.overlap(mapping))) return false
        regions += mapping
        true
      }

      override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
    }
    val outputAgent = new Axi4WriteOnlySlaveAgent(dut.io.output, dut.clockDomain)

    val writes = mutable.HashMap[BigInt, Byte]()
    val inputMonitor = new Axi4WriteOnlyMonitor(dut.io.input, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte): Unit = {
        //          println(s"I $address -> $data")
        writes(address) = data
      }
    }

    val outputMonitor = new Axi4WriteOnlyMonitor(dut.io.output, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte): Unit = {
        //          println(s"O $address -> $data")
        assert(writes(address) == data)
        writes.remove(address)
      }
    }


    dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
    inputAgent.allowGen = false
    dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
    dut.clockDomain.waitSampling(100)
    assert(writes.isEmpty)
    println("done")
  }

  test("writeOnly_32_64") {
    SimConfig.compile(Axi4WriteOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 64, 4))).doSim("test", 42)(writeTester)
  }
  test("writeOnly_16_64") {
    SimConfig.compile(Axi4WriteOnlyUpsizer(Axi4Config(20, 16, 4), Axi4Config(20, 64, 4))).doSim("test", 42)(writeTester)
  }
  test("writeOnly_32_128") {
    SimConfig.compile(Axi4WriteOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 128, 4))).doSim("test", 42)(writeTester)
  }



  def readTester(dut : Axi4ReadOnlyUpsizer): Unit ={
    dut.clockDomain.forkStimulus(10)

    val regions = mutable.Set[SizeMapping]()
    val inputAgent = new Axi4ReadOnlyMasterAgent(dut.io.input, dut.clockDomain) {
      override def genAddress(): BigInt = Random.nextInt(1 << 19)
      override def bursts: List[Int] = List(1)
      override def mappingAllocate(mapping: SizeMapping): Boolean = {
        if(regions.exists(_.overlap(mapping))) return false
        regions += mapping
        true
      }

      override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
    }

    val outputAgent = new Axi4ReadOnlySlaveAgent(dut.io.output, dut.clockDomain)



    val reads = mutable.HashMap[BigInt, Byte]()
    val inputMonitor = new Axi4ReadOnlyMonitor(dut.io.input, dut.clockDomain) {
      override def onReadByte(address: BigInt, data: Byte, id : Int): Unit = {
        reads(address) = data
      }
      override def onLast(id: Int): Unit = {}
    }

    val outputMonitor = new Axi4ReadOnlyMonitor(dut.io.output, dut.clockDomain) {
      override def onReadByte(address: BigInt, data: Byte, id : Int): Unit = {
        if(reads.contains(address)) {
          assert(reads(address) == data)
          reads.remove(address)
        }
      }

      override def onLast(id: Int): Unit = assert(reads.isEmpty)
    }



    dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
    inputAgent.allowGen = false
    dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
    dut.clockDomain.waitSampling(100)
    assert(reads.isEmpty)
    println("done")
  }

  test("readOnly_32_64") {
    SimConfig.compile(Axi4ReadOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 64, 4),4)).doSim("test", 42)(readTester)
  }
  test("readOnly_16_64") {
    SimConfig.compile(Axi4ReadOnlyUpsizer(Axi4Config(20, 16, 4), Axi4Config(20, 64, 4),4)).doSim("test", 42)(readTester)
  }
  test("readOnly_32_128") {
    SimConfig.compile(Axi4ReadOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 128, 4),4)).doSim("test", 42)(readTester)
  }
}