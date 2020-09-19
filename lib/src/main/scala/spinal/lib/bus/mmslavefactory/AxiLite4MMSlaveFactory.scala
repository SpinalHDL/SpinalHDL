package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.amba4.axilite.AxiLite4B
import spinal.lib.bus.amba4.axilite.AxiLite4R

case class AxiLite4MMSlaveFactory(bus: AxiLite4, sizeMap: SizeMapping, selId: Int = 0, readSync: Boolean = true) extends MMSlaveFactory {

  val readError = Bool()
  val readResp = Stream(AxiLite4R(bus.config))
  val writeResp = Stream(AxiLite4B(bus.config))
  
  val readData  = readResp.payload.data
  
  bus.ar.ready := False
  bus.aw.ready := False
  bus.w.ready  := False
  
  writeResp.valid := False
  writeResp.payload.setOKAY()
  bus.b << writeResp.stage()
  
  readResp.valid := False
  readResp.payload.data := 0x0
  readResp.payload.setOKAY()
  bus.r << readResp.stage()  

  val askWrite    = (bus.aw.valid && bus.w.valid).allowPruning()
  val askRead     = (bus.ar.valid).allowPruning()
  val doWrite     = (bus.aw.valid && bus.aw.ready && bus.w.valid && bus.w.ready).allowPruning()
  val doRead      = (bus.ar.valid && bus.ar.ready).allowPruning()
  val writeData   = bus.w.payload.data

  override def readAddress()  = bus.ar.payload.addr
  override def writeAddress() = bus.aw.payload.addr

  override def readHalt()  = bus.ar.ready := False
  override def writeHalt() = {
    bus.aw.ready := False
    bus.w.ready := False
  }

  override def readAccept() = {
    bus.ar.ready := True
  }

  override def writeAccept() = {
    bus.aw.ready := True
    bus.w.ready  := True 
  }

  override def readRespond(data : Bits, error : Boolean) = {
    readResp.valid := True
    readResp.payload.data := data
    if(error)
      readResp.payload.setDECERR()
    else
      readResp.payload.setOKAY()
  }

  override def writeRespond(error : Boolean) = {
    writeResp.valid := True
    if(error)
      writeResp.payload.setDECERR()
    else
      writeResp.payload.setOKAY()
  }

  override def busDataWidth = bus.config.dataWidth
}
