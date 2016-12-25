package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._


class AxiLite4SlaveFactory(bus : AxiLite4, configBus: BusSlaveFactoryConfig = BusSlaveFactoryConfig()) extends BusSlaveFactoryDelayed{

  val writeJoinEvent = StreamJoin.arg(bus.writeCmd,bus.writeData)
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
      case element: BusSlaveFactoryNonStopWrite =>  element.that.assignFromBits(bus.writeData.data(element.bitOffset, element.that.getBitsWidth bits))
      case _ =>
    }

    val writeOccur = writeJoinEvent.fire
    val readOccur = bus.readRsp.fire

    for((address,jobs) <- elementsPerAddress){
      when(bus.writeCmd.addr === address) {
        when(writeOccur) {
          //TODO writeRsp.resp := OKAY
          for (element <- jobs) element match {
            case element: BusSlaveFactoryWrite   =>  element.that.assignFromBits(bus.writeData.data(element.bitOffset, element.that.getBitsWidth bits))
            case element: BusSlaveFactoryOnWrite => element.doThat()
            case _ =>
          }
        }
      }
      when(readDataStage.addr === address) {
        //TODO readRsp.resp := OKAY
        for(element <- jobs) element match{
          case element: BusSlaveFactoryRead =>  readRsp.data(element.bitOffset, element.that.getBitsWidth bits) := element.that.asBits
          case _ =>
        }
        when(readOccur) {
          for (element <- jobs) element match {
            case element: BusSlaveFactoryOnRead => element.doThat()
            case _ =>
          }
        }
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth

  override def configFactory: BusSlaveFactoryConfig = configBus
}
