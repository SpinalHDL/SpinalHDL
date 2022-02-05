package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4B, AxiLite4R}
import spinal.lib.bus.misc.SizeMapping

case class AxiLite4BusInterface(bus: AxiLite4, sizeMap: SizeMapping, regPre: String = "")(implicit moduleName: ClassName) extends BusIf {
  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData  = Bits(bus.config.dataWidth bits)

  readError.setAsReg() init False
  readData.setAsReg()  init 0

  val axiAr = bus.readCmd.stage()
  val axiR  = Stream(AxiLite4R(bus.config))
  val axiRValid = Reg(Bool) init(False)

  val axiAw = bus.writeCmd.stage()
  val axiW  = bus.writeData.stage()
  val axiB  = Stream(AxiLite4B(bus.config))
  val axiBValid = Reg(Bool) init(False)
  
  axiAr.ready := !axiRValid || axiR.ready
  axiR.payload.setOKAY()
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