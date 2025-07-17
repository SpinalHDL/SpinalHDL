package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver
import spinal.lib.bus.amba4.axis._
import spinal.lib.bus.localbus.{MemBus, MemBusConfig, MemBusDriver}
import spinal.lib.bus.regif.BusInterface
import spinal.tester.{SpinalAnyFunSuite, SpinalSimFunSuite}

import scala.language.postfixOps

case class AxiLiteExample() extends Component {

  val config = AxiLite4Config(17, 32)

  val ctrlIn  = slave(AxiLite4(config))
  val ram = master(MemBus(MemBusConfig(15,32)))

  val AxiLiteBusIf = BusInterface(ctrlIn,(0 , 100 KiB), "register")
  val MemWRBus = AxiLiteBusIf.newRAMAt(0, 100 KiB, "Ram")

  ram << MemWRBus.bus
}
class AxiExample extends SpinalAnyFunSuite {

  test("busIf") {
    //Run the simulation
    SimConfig
      .withVerilator
      .withFstWave
      .compile(AxiLiteExample())
      .doSimUntilVoid { dut =>
        dut.clockDomain.forkStimulus(10 ns)
        MemBusDriver(dut.ram, dut.clockDomain).hangMem()
        val axiLiteDrive = AxiLite4Driver(dut.ctrlIn,dut.clockDomain)
        (0 until 100).foreach{
          case i =>
            axiLiteDrive.write(i*4, i)
            val data = axiLiteDrive.read(i*4)
            assert(data == i,s"read data: ${data} is not same with write data${i}")
        }
        dut.clockDomain.waitSampling((1 KiB).toInt)
        simSuccess()
      }
  }
}
