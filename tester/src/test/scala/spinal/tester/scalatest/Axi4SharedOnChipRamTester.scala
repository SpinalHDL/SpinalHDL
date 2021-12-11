package spinal.tester.scalatest


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._


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

class Axi4SharedOnChipRamMultiPortTester extends AnyFunSuite {

    val memory = mutable.HashMap[BigInt, Byte]()
    def axi4master(dut: Axi4SharedOnChipRamMultiPortFixture, id: Int) {
        val slave   = dut.io.axis(id)
        val regions = mutable.Set[SizeMapping]()
        val read = new Axi4ReadOnlyMasterAgent(slave, dut.clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << slave.config.addressWidth)
            override def bursts: List[Int]    = List(1)
            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }

        val readMonitor = new Axi4ReadOnlyMonitor(slave, dut.clockDomain) {
            override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
                if (memory.contains(address)) assert(memory(address) == data, id.toString + ":" + address.toString(16))
            }
            override def onLast(id: Int): Unit = {}
        }

        val write = new Axi4WriteOnlyMasterAgent(slave, dut.clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << slave.config.addressWidth)
            override def bursts: List[Int]    = List(1)
            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }
        val writeMonitor = new Axi4WriteOnlyMonitor(slave, dut.clockDomain) {
            override def onWriteByte(address: BigInt, data: Byte): Unit = {
                memory(address) = data
            }
        }
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
                useId = true,
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
                axi4master(dut, i)
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
}