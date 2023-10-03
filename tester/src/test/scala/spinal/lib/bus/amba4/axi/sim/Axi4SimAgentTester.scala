package spinal.lib.bus.amba4.axi.sim

import spinal.core._
import spinal.core.sim._
import spinal.core.sim.SimCompiled
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.misc.SizeMapping
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.util.Random

case class Axi4PassthroughFixture(config: Axi4Config) extends Component {
    val io = new Bundle {
        val input  = slave(Axi4(config))
        val output = master(Axi4(config))
    }
    io.input.aw >-> io.output.aw
    io.input.ar >-> io.output.ar
    io.input.w >-> io.output.w
    io.output.r >> io.input.r
    io.output.b >> io.input.b
}

class Axi4SimAgentTester extends SpinalAnyFunSuite {

    def writeTester(dut: Axi4PassthroughFixture): Unit = {
        dut.clockDomain.forkStimulus(10)

        val regions = mutable.Set[SizeMapping]()
        val inputAgent = new Axi4WriteOnlyMasterAgent(dut.io.input, dut.clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << 19) // & 0xFFF00) | 6

            override def bursts: List[Int] = List(1)

            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }
        val outputAgent = new Axi4WriteOnlySlaveAgent(dut.io.output, dut.clockDomain)

        val writes = mutable.HashMap[BigInt, Byte]()
        val inputMonitor = new Axi4WriteOnlyMonitor(dut.io.input, dut.clockDomain) {
            override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
                // println(s"I $address -> $data")
                writes(address) = data
            }
        }

        val outputMonitor = new Axi4WriteOnlyMonitor(dut.io.output, dut.clockDomain) {
            override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
                // println(s"O $address -> $data")
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

    test("writeOnly") {
        SimConfig.compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4))).doSim("test", 42)(writeTester)
    }

    test("writeOnly_disable_strb") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useStrb = false)))
            .doSim("test", 42)(writeTester)
    }

    test("writeOnly_disable_id") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useId = false)))
            .doSim("test", 42)(writeTester)
    }

    test("writeOnly_disable_size") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useSize = false)))
            .doSim("test", 42)(writeTester)
    }

    test("writeOnly_disable_resp") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useResp = false)))
            .doSim("test", 42)(writeTester)
    }

    test("writeOnly_disable_last") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useLast = false)))
            .doSim("test", 42)(writeTester)
    }

    test("writeOnly_disable_burst") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useBurst = false)))
            .doSim("test", 42)(writeTester)
    }

    test("writeOnly_disable_len") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useLen = false)))
            .doSim("test", 42)(writeTester)
    }

    def readTester(dut: Axi4PassthroughFixture): Unit = {
        dut.clockDomain.forkStimulus(10)

        val regions = mutable.Set[SizeMapping]()
        val inputAgent = new Axi4ReadOnlyMasterAgent(dut.io.input, dut.clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << 19)
            override def bursts: List[Int]    = List(1)
            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }

        val outputAgent = new Axi4ReadOnlySlaveAgent(dut.io.output, dut.clockDomain)

        val reads = mutable.HashMap[BigInt, Byte]()
        val inputMonitor = new Axi4ReadOnlyMonitor(dut.io.input, dut.clockDomain) {
            override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
                reads(address) = data
            }
        }

        val outputMonitor = new Axi4ReadOnlyMonitor(dut.io.output, dut.clockDomain) {
            override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
                if (reads.contains(address)) {
                    assert(reads(address) == data)
                    reads.remove(address)
                }
            }

            override def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = if (last) assert(reads.isEmpty)
        }

        dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
        inputAgent.allowGen = false
        dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
        dut.clockDomain.waitSampling(100)
        assert(reads.isEmpty)
        println("done")
    }

    test("readOnly") {
        SimConfig.compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4))).doSim("test", 42)(readTester)
    }

    test("readOnly_disable_strb") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useStrb = false)))
            .doSim("test", 42)(readTester)
    }

    test("readOnly_disable_id") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useId = false)))
            .doSim("test", 42)(readTester)
    }

    test("readOnly_disable_size") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useSize = false)))
            .doSim("test", 42)(readTester)
    }

    test("readOnly_disable_resp") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useResp = false)))
            .doSim("test", 42)(readTester)
    }

    test("readOnly_disable_last") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useLast = false)))
            .doSim("test", 42)(readTester)
    }

    test("readOnly_disable_burst") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useBurst = false)))
            .doSim("test", 42)(readTester)
    }

    test("readOnly_disable_len") {
        SimConfig
            .compile(Axi4PassthroughFixture(Axi4Config(20, 32, 4, useLen = false)))
            .doSim("test", 42)(readTester)
    }
}
