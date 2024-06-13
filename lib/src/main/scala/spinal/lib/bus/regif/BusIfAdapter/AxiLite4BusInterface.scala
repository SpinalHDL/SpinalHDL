package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4B, AxiLite4R}
import spinal.lib.bus.misc.SizeMapping

case class AxiLite4BusInterface(bus: AxiLite4, sizeMap: SizeMapping, regPre: String = "", withSecFireWall: Boolean = false)(implicit moduleName: ClassName) extends BusIf {
  override val busDataWidth   = bus.config.dataWidth
  override val busAddrWidth   = bus.config.addressWidth
  override val withStrb: Boolean = true

  val bus_rderr: Bool = Bool()
  val bus_rdata: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = Reg(Bool(), init = False)
  val reg_rdata: Bits = Reg(Bits(busDataWidth bits), init = defualtReadBits)

  val wstrb: Bits = withStrb generate (Bits(strbWidth bit))
  val wmask: Bits = withStrb generate (Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate (Bits(busDataWidth bit))
  override def getModuleName = moduleName.name

  val axiAr = bus.readCmd.stage()
  val axiR  = Stream(AxiLite4R(bus.config))
  val axiRValid = Reg(Bool()) init(False)

  val axiAw = bus.writeCmd.stage()
  val axiW  = bus.writeData.stage()
  val axiB  = Stream(AxiLite4B(bus.config))
  val axiBValid = Reg(Bool()) init(False)

  //The RC/RS/W1RC/W0RC of the regif allows the use strb to be used when reading, which is possible for apb4.
  //However, for AXI, Strb is only used in the Write channel, so set high is mandatory here
  withStrb generate (wstrb := Mux(axiAr.valid ,B((1 << strbWidth) -1, strbWidth bit), axiW.strb))
  initStrbMasks()


  when(bus_rderr) {
    axiR.payload.setSLVERR()
  } otherwise {
    axiR.payload.setOKAY()
  }
  axiR.valid := axiRValid
  axiR.payload.data := bus_rdata


  axiB.setOKAY()
  axiB.valid := axiBValid

  bus.r << axiR
  bus.b << axiB

  val askWrite  = axiAw.valid && axiW.valid 
  val askRead   = axiAr.valid || (axiR.valid && !axiR.ready)
  val doWrite   = askWrite && (!axiB.valid || axiB.ready)    //Assume one stage between xw and B
  val doRead    = axiAr.valid && (!axiR.valid || axiR.ready) //Assume one stage between Ar and R
  val writeData = axiW.payload.data

  override lazy val bus_nsbit: Bool = Mux(askRead, bus.ar.prot(1), bus.aw.prot(1))

  axiRValid clearWhen(axiR.ready) setWhen(doRead)
  axiAr.ready := doRead
  
  axiBValid clearWhen(axiB.ready) setWhen(doWrite) 
  axiAw.ready := doWrite
  axiW.ready  := doWrite

  def readAddress()  = if(withStrb) U(axiAr.addr.drop(underbitWidth) ## B(0, underbitWidth bit)) else axiAr.addr
  def writeAddress() : UInt = axiAw.addr

  override def readHalt(): Unit = doRead := False
  override def writeHalt(): Unit = doWrite := False
}
