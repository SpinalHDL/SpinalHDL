package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.amba4.axilite.AxiLite4B
import spinal.lib.bus.amba4.axilite.AxiLite4R

case class AxiLite4MMSlaveFactory(bus: AxiLite4, sizeMap: SizeMapping, selId: Int = 0) extends MMSlaveFactory {

  val readError = Bool()
  val readRespStream = Stream(AxiLite4R(bus.config))
  val writeRespStream = Stream(AxiLite4B(bus.config))
  
  val readData  = readRespStream.payload.data
  
  bus.ar.ready := False
  bus.aw.ready := False
  bus.w.ready  := False
  
  writeRespStream.valid := False
  writeRespStream.payload.setOKAY()
  bus.b << writeRespStream.stage()
  
  readRespStream.valid := False
  readRespStream.payload.data := 0x0
  readRespStream.payload.setOKAY()
  bus.r << readRespStream.stage()  

  val writeReq    = (bus.aw.valid && bus.w.valid).allowPruning()
  val readReq     = (bus.ar.valid).allowPruning()
  val writeResp   = (bus.aw.valid && bus.aw.ready && bus.w.valid && bus.w.ready).allowPruning()
  val readResp    = (bus.ar.valid && bus.ar.ready).allowPruning()
  val writeData   = bus.w.payload.data

  override def readAddress()  = bus.ar.payload.addr
  override def writeAddress() = bus.aw.payload.addr

  override def readAccept() = {
    bus.ar.ready := True
  }

  override def writeAccept() = {
    bus.aw.ready := True
    bus.w.ready  := True 
  }

  override def readRespond(data : Bits, error : Boolean) = {
    readRespStream.valid := True
    readRespStream.payload.data := data
    if(error)
      readRespStream.payload.setDECERR()
    else
      readRespStream.payload.setOKAY()
  }

  override def writeRespond(error : Boolean) = {
    writeRespStream.valid := True
    if(error)
      writeRespStream.payload.setDECERR()
    else
      writeRespStream.payload.setOKAY()
  }

  override def busDataWidth = bus.config.dataWidth
}
