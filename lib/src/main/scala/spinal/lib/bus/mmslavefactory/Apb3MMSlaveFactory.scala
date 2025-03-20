package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.misc.SizeMapping

case class Apb3MMSlaveFactory(bus: Apb3, sizeMap: SizeMapping, selId: Int = 0) extends MMSlaveFactory {

  val ready = Reg(Bool()) init False
  val readError = Reg(Bool()) init False
  val readData  = Reg(Bits(bus.config.dataWidth bits)) init 0

  ready := False
  readError := False
  bus.PREADY := ready
  bus.PRDATA := readData
  if(bus.config.useSlaveError) bus.PSLVERROR := readError

  // All outputs (ready, readError, readData) are registered. Thus, its ok
  // to trigger already on the request stage.
  // Request lasts as long as pready is low.
  val writeReq  = (bus.PSEL(selId) &&  bus.PWRITE && !bus.PREADY).allowPruning()
  val readReq   = (bus.PSEL(selId) && !bus.PWRITE && !bus.PREADY).allowPruning()
  val writeResp = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY &&  bus.PWRITE).allowPruning()
  val readResp  = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY && !bus.PWRITE).allowPruning()
  val writeData = bus.PWDATA

  override def readAddress()  = bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readAccept() = {
    // no such thing in APB
  }

  override def writeAccept() = {
    // no such thing in APB
  }

  override def readRespond(data : Bits, error : Boolean) = {
    ready := True
    readData := data
    if(bus.config.useSlaveError) {
      if(error)
        readError := True
    }
  }

  override def writeRespond(error : Boolean) = {
    ready := True
    if(bus.config.useSlaveError) {
      if(error)
        readError := True
    }
  }

  override def busDataWidth = bus.config.dataWidth
}
