package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4B, AxiLite4R}
import spinal.lib.bus.misc.SizeMapping

//case class AxiLite4BusInterface(bus: AxiLite4, sizeMap: SizeMapping, readSync: Boolean = true)(implicit moduleName: ClassName) extends BusIf{
//  override def getModuleName = moduleName.name
//
//  val readHaltRequest = False
//  val writeHaltRequest = False
//
//  val readError = Bool()
//  val readData  = Bits(bus.config.dataWidth bits)
//
//  if(readSync) {
//    readError.setAsReg() init False
//    readData.setAsReg()  init 0
//  } else {
//    readError := False
//    readData := 0
//  }
//
//  val readDataStage = bus.readCmd.stage()
//  val readRsp = AxiLite4R(bus.config)
//  val writeRsp = AxiLite4B(bus.config)
//
//  writeRsp.setOKAY()
//  readRsp.setOKAY()
//  readRsp.data := 0
//
//  def readAddress() : UInt = readDataStage.addr
//  def writeAddress() : UInt = bus.writeCmd.addr
//
//  override def readHalt(): Unit = readHaltRequest := True
//  override def writeHalt(): Unit = writeHaltRequest := True
//}