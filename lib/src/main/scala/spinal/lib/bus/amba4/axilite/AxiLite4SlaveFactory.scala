package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._



class AxiLite4SlaveFactory(bus : AxiLite4) extends BusSlaveFactoryDelayed{
  val writeJoinEvent = StreamJoin(bus.writeCmd,bus.writeData)
  val writeRsp = AxiLite4B(bus.config)
  bus.writeRsp <-< writeJoinEvent.translateWith(writeRsp)

  val readDataStage = bus.readCmd.stage()
  val readRsp = AxiLite4R(bus.config)
  bus.readRsp << readDataStage.translateWith(readRsp)


  writeRsp.setOKAY()
  readRsp.setOKAY()
  readRsp.data := 0

  override def build(): Unit = {
    for(element <- elements) element match {
      case element : BusSlaveFactoryNonStopWrite =>
        element.that.assignFromBits(bus.writeData.data(element.bitOffset, element.that.getBitsWidth bits))
      case _ =>
    }

    for((address,jobs) <- elementsPerAddress){
      when(bus.writeCmd.addr === address) {
        when(writeJoinEvent.valid) {
          //TODO writeRsp.resp := OKAY
          for(element <- jobs) element match{
            case element : BusSlaveFactoryWrite => {
              element.that.assignFromBits(bus.writeData.data(element.bitOffset, element.that.getBitsWidth bits))
            }
            case element : BusSlaveFactoryOnWrite => element.doThat()
            case _ =>
          }
        }

        //TODO readRsp.resp := OKAY
        for(element <- jobs) element match{
          case element : BusSlaveFactoryRead => {
            readRsp.data(element.bitOffset, element.that.getBitsWidth bits) := element.that.asBits
          }
          case element : BusSlaveFactoryOnRead => element.doThat()
          case _ =>
        }
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth
}
