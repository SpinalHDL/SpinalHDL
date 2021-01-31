package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.slave
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.regif.AccessType.RW

import scala.util.Random

case class Apb3BusInterface(bus: Apb3, sizeMap: SizeMapping, selId: Int = 0, readSync: Boolean = true, regPre: String = "")(implicit moduleName: ClassName) extends BusIf{

  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData  = Bits(bus.config.dataWidth bits)

  if(readSync) {
    readError.setAsReg() init False
    readData.setAsReg()  init 0
    // if readSync, PREADY should halt the first PENABLE cycle.
    bus.PREADY := RegNext(!(bus.PSEL(selId) && !bus.PENABLE && !bus.PWRITE))
  } else {
    readError := False
    readData  := 0
    bus.PREADY := True
  }

  //bus.PREADY := True
  bus.PRDATA := readData
  if(bus.config.useSlaveError) bus.PSLVERROR := readError

//   val askWrite  = (bus.PSEL(selId) && bus.PENABLE && bus.PWRITE).allowPruning()
//   val askRead   = (bus.PSEL(selId) && bus.PENABLE && !bus.PWRITE).allowPruning()
//   val doWrite   = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY &&  bus.PWRITE).allowPruning()
//   val doRead    = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY && !bus.PWRITE).allowPruning()
//   val writeData = bus.PWDATA
  // improve apb timing closure.
  val askWrite: Bool = RegNext(bus.PSEL(selId) && !bus.PENABLE && bus.PWRITE).allowPruning()
  val askRead: Bool = RegNext(bus.PSEL(selId) && !bus.PENABLE && !bus.PWRITE).allowPruning()
  val doWrite: Bool = askWrite.allowPruning()
  val doRead: Bool = askRead.allowPruning()
  val writeData: Bits = RegNext(bus.PWDATA)

  // improve apb timing closure.
  val addrReg: UInt = RegNext(bus.PADDR)
  override def readAddress() = addrReg
  override def writeAddress() = addrReg

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False

  override def busDataWidth   = bus.config.dataWidth
}

// testbench
// object Apb3BusInterfaceSim {
//   class Apb3BusInterfaceExample(readSync:Boolean) extends Component {
//     val io = new Bundle {
//       val apb = slave(Apb3(Apb3Config(16, 32)))
//     }
//     val busSlave = Apb3BusInterface(io.apb, (0x0000, 100 Byte), readSync = readSync)
//     val M_REG0: RegInst = busSlave.newReg(doc = "REG0")
//     val M_REG1: RegInst = busSlave.newReg(doc = "REG1")
//     val M_REG2: RegInst = busSlave.newReg(doc = "REG2")
//     val M_REG3: RegInst = busSlave.newReg(doc = "REG3")
//     val test0: Bits = M_REG0.field(32 bits, RW, doc = "")
//     val test1: Bits = M_REG1.field(32 bits, RW, doc = "")
//     val test2: Bits = M_REG2.field(32 bits, RW, doc = "")
//     val test3: Bits = M_REG3.field(32 bits, RW, doc = "")
//   }

//   def test(dut:Apb3BusInterfaceExample):Unit={
//     dut.clockDomain.forkStimulus(10)
//     dut.io.apb.PSEL #= 0
//     dut.io.apb.PENABLE.randomize()
//     dut.io.apb.PADDR.randomize()
//     dut.io.apb.PWDATA.randomize()
//     dut.io.apb.PWRITE.randomize()
//     dut.clockDomain.waitSampling(10)

//     val drv = Apb3Driver(dut.io.apb, dut.clockDomain)
//     (0 until 100).foreach(_=>{
//       val addr = Random.nextInt(4)*4
//       val data = Random.nextLong() & ((1L << 32) - 1)
//       drv.write(addr, data)
//       assert(drv.read(addr).toLong == data)
//     })
//     simSuccess()
//   }
//   def main(args: Array[String]): Unit = {
//     SimConfig.compile(new Apb3BusInterfaceExample(false)).doSimUntilVoid(dut =>test(dut))
//     SimConfig.compile(new Apb3BusInterfaceExample(true)).doSimUntilVoid(dut =>test(dut))
//   }
// }