package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactoryDelayed, BusSlaveFactoryRead, BusSlaveFactoryWrite, BusSlaveFactoryElement}
import scala.collection.mutable.ArrayBuffer


class AxiLiteSlaveFactory(bus : AxiLite) extends BusSlaveFactoryDelayed{
  val writeJoinEvent = StreamJoin(bus.writeCmd,bus.writeData)
  val writeRsp = AxiLiteB(bus.config)
  bus.writeRet <-< writeJoinEvent.translateWith(writeRsp)

  val readDataStage = bus.readCmd.stage()
  val readRsp = AxiLiteR(bus.config)
  bus.readData << readDataStage.translateWith(readRsp)

  override def build(): Unit = {
    //writes
    //TODO writeRsp.resp := OKAY
    when(writeJoinEvent.valid) {
      for (e <- elements) e match {
        case e: BusSlaveFactoryWrite => {
          when(bus.writeCmd.addr === e.address) {
            e.that.assignFromBits(bus.writeData.data(e.bitOffset, e.that.getBitsWidth bits))
//            when(writeJoinEvent.ready) {
//              e.onCmd
//            }
          }
        }
      }
    }

    //Reads
    //TODO readRsp.resp := OKEY
    readRsp.data := 0
    for (e <- elements) e match {
      case e: BusSlaveFactoryRead => {
        when(readDataStage.addr === e.address) {
          readRsp.data(e.bitOffset, e.that.getBitsWidth bits) := e.that.asBits
//          when(bus.readData.fire) {
//            e.onRsp
//          }
        }
      }
    }

    ??? //many todo
  }

  override def busDataWidth: Int = bus.config.dataWidth
}
