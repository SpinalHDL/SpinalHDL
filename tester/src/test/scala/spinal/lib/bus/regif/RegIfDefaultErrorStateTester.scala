package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config}

/*
  Test the default error state
 */
class RegIfDefaultErrorStateTester(defaultError: Boolean) extends Component{
  val io = new Bundle{
    val axilite4 = slave(AxiLite4(AxiLite4Config(16,32)))
    val wProtect = in Bool()
    val rProtect = in Bool()
  }
  val busif = BusInterface(io.axilite4,(0x000,4 KiB), regPre = "TestBusif", withSecFireWall = true)
  busif.setReservedAddressErrorState(defaultError)
  val testGroup = busif.newGrp(64,"Test secure Group",Secure.CS(io.wProtect,io.rProtect))
  val REG0 = testGroup.newReg("Reg0")
  val reg0_field = REG0.field(Bits(32 bit), AccessType.RW, 0x00, doc = "test")
  val REG1 = testGroup.newReg("Reg1")
  val reg1_field = REG1.field(Bits(32 bit), AccessType.RW, 0x00, doc = "test")
}

object playregifDefaultError extends App {
  val rde = SpinalConfig().copy(targetDirectory = "./out/regifEst", removePruned = true, headerWithDate = true)
  rde.generateVerilog(new RegIfDefaultErrorStateTester(false).setDefinitionName("RIDES_false"))
  rde.generateVerilog(new RegIfDefaultErrorStateTester(true).setDefinitionName("RIDES_true"))
}