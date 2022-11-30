package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4B, AxiLite4R}
import spinal.lib.bus.misc.SizeMapping

case class AxiLite4BusInterface(bus: AxiLite4, sizeMap: SizeMapping, regPre: String = "")(implicit moduleName: ClassName) extends BusIf {
  override val withstrb: Boolean = true
  val wstrb: Bits = withstrb generate (Bits(strbWidth bit))
  val wmask: Bits = withstrb generate (Bits(busDataWidth bit))
  val wmaskn: Bits = withstrb generate (Bits(busDataWidth bit))
  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData  = Bits(bus.config.dataWidth bits)

  readError.setAsReg() init False
  readData.setAsReg()  init 0

  val axiAr = bus.readCmd.stage()
  val axiR  = Stream(AxiLite4R(bus.config))
  val axiRValid = Reg(Bool()) init(False)

  val axiAw = bus.writeCmd.stage()
  val axiW  = bus.writeData.stage()
  val axiB  = Stream(AxiLite4B(bus.config))
  val axiBValid = Reg(Bool()) init(False)

  //The RC/RS/W1RC/W0RC of the regif allows the use strb to be used when reading, which is possible for apb4.
  //However, for AXI, Strb is only used in the Write channel, so set high is mandatory here
  withstrb generate (wstrb := Mux(axiAr.valid ,B((1 << strbWidth) -1, strbWidth bit), axiW.strb))
  InitLogic()

  
  axiAr.ready := !axiRValid || axiR.ready
  when(readError) {
    axiR.payload.setSLVERR()
  } otherwise {
    axiR.payload.setOKAY()
  }
  axiR.valid := axiRValid
  axiR.payload.data := readData

  axiAw.ready := axiAw.valid && axiW.valid
  axiW.ready  := axiAw.valid && axiW.valid
  axiB.setOKAY()
  axiB.valid := axiBValid

  bus.r << axiR
  bus.b << axiB

  val askWrite  = axiAw.valid
  val askRead   = axiAr.valid
  val doWrite   = axiAw.valid && axiW.valid 
  val doRead    = axiAr.valid && axiAr.ready
  val writeData = axiW.payload.data

  axiRValid := doRead
  axiBValid := doWrite

  def readAddress() : UInt = axiAr.addr
  def writeAddress() : UInt = axiAw.addr

  override def readHalt(): Unit = axiAr.ready := False
  override def writeHalt(): Unit = axiAw.ready := False

  override def busDataWidth   = bus.config.dataWidth
}
