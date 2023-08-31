package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.tester.{SpinalAnyFunSuite, SpinalTesterCocotbBase}

class Axi4SharedOnChipRamTester extends SpinalTesterCocotbBase {
  override def getName: String = "Axi4SharedOnChipRamTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/Axi4SharedOnChipRamTester"
  override def createToplevel: Component = Axi4SharedOnChipRam(32,4096,4).setDefinitionName(getName)
  override def noVhdl = true
}

import scala.collection.mutable
import scala.util.Random
import org.scalatest.funsuite.AnyFunSuite

import spinal.sim._
import spinal.core.sim._

import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SharedOnChipRam}
import spinal.lib.bus.amba4.axi.sim.{
    Axi4ReadOnlyMasterAgent,
    Axi4ReadOnlyMonitor,
    Axi4WriteOnlyMasterAgent,
    Axi4WriteOnlyMonitor
}

case class Axi4SharedOnChipRamMultiPortFixture(axiConfig: Axi4Config, wordCount: BigInt, portCount: Int)
    extends Component {

    val io = new Bundle {
        val axis = Vec(slave(Axi4(axiConfig)), portCount)
    }

    val dut = Axi4SharedOnChipRamMultiPort(axiConfig, wordCount, portCount)
    (io.axis, dut.io.axis).zipped map (_.toShared >> _)
}

case class Axi4SharedOnChipRamMultiPortWithSoftresetFixture(config: Axi4Config, wordCount: BigInt, portCount: Int)
    extends Component {

    val io = new Bundle {
        val axis      = Vec(slave(Axi4(config)), portCount)
        val softReset = in Bool ()
    }

    val ram   = Mem(config.dataType, wordCount.toInt)
    val port0 = Axi4SharedOnChipRamPort(config, ram, io.softReset)
    val port1 = Axi4SharedOnChipRamPort(config, ram)
    io.axis(0).toShared() >> port0
    io.axis(1).toShared() >> port1
}

class Axi4SharedOnChipRamMultiPortTester extends SpinalAnyFunSuite {

    val memory  = mutable.HashMap[BigInt, Byte]()
    val regions = mutable.Set[SizeMapping]()
    def axi4master(
        port: Axi4,
        clockDomain: ClockDomain
    ): (Axi4ReadOnlyMasterAgent, Axi4WriteOnlyMasterAgent, Axi4ReadOnlyMonitor, Axi4WriteOnlyMonitor) = {
        val read = new Axi4ReadOnlyMasterAgent(port, clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << port.config.addressWidth)
            override def bursts: List[Int]    = List(1)
            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }

        val readMonitor = new Axi4ReadOnlyMonitor(port, clockDomain) {
            override def onReadStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {}

            override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
                if (memory.contains(address)) assert(memory(address) == data, id.toString + ":" + address.toString(16))
            }

            override def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = {}
        }

        val write = new Axi4WriteOnlyMasterAgent(port, clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << port.config.addressWidth)
            override def bursts: List[Int]    = List(1)
            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }
        val writeMonitor = new Axi4WriteOnlyMonitor(port, clockDomain) {
            override def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {}

            override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
                memory(address) = data
            }

            override def onResponse(id: Int, resp: Byte): Unit = {}
        }
        (read, write, readMonitor, writeMonitor)
    }

    def generic_test(ports: Int) {
        memory.clear()

        val idWidth = log2Up(ports)
        val compiled = SimConfig.compile {
            val byteCount = 1 MiB
            val config = Axi4Config(
                addressWidth = log2Up(byteCount),
                dataWidth = 32,
                idWidth = idWidth,
                useLock = false,
                useRegion = false,
                useCache = false,
                useProt = false,
                useQos = false
            )
            val dut =
                new Axi4SharedOnChipRamMultiPortFixture(
                    axiConfig = config,
                    wordCount = byteCount / config.bytePerWord,
                    portCount = ports
                )
            dut
        }

        compiled.doSim { dut =>
            dut.clockDomain.forkStimulus(period = 10)
            for (i <- 0 until ports) {
                axi4master(dut.io.axis(i), dut.clockDomain)
            }
            dut.clockDomain.waitSampling((100 KiB).toInt)
            simSuccess()
        }
    }

    test("port2") {
        generic_test(2)
    }

    test("port3") {
        generic_test(3)
    }

    test("port4") {
        generic_test(4)
    }

    test("softreset") {
        memory.clear()

        val ports   = 2
        val idWidth = log2Up(ports)
        val compiled = SimConfig.compile {
            val byteCount = 1 MiB
            val config = Axi4Config(
                addressWidth = log2Up(byteCount),
                dataWidth = 32,
                idWidth = idWidth,
                useId = false,
                useLock = false,
                useRegion = false,
                useCache = false,
                useProt = false,
                useQos = false
            )
            val dut =
                new Axi4SharedOnChipRamMultiPortWithSoftresetFixture(
                    config = config,
                    wordCount = byteCount / config.bytePerWord,
                    portCount = ports
                )
            dut
        }

        compiled.doSim { dut =>
            dut.io.softReset #= false
            dut.clockDomain.forkStimulus(period = 10)
            val agents = axi4master(dut.io.axis(0), dut.clockDomain)
            axi4master(dut.io.axis(1), dut.clockDomain)

            dut.clockDomain.waitSampling((10 KiB).toInt)
            for (i <- 0 until 20) {
                dut.clockDomain.waitSamplingWhere(
                    dut.io.axis(0).ar.valid.toBoolean && dut.io.axis(0).ar.ready.toBoolean
                )
                dut.io.softReset #= true
                dut.clockDomain.waitSampling(1)

                agents._3.reset()
                agents._1.reset()

                agents._4.reset()
                agents._2.reset()

                dut.io.softReset #= false
                dut.clockDomain.waitSampling(Random.nextInt(5000))
            }

            simSuccess()
        }
    }
}
