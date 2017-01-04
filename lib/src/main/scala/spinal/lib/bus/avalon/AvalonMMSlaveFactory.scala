package spinal.lib.bus.avalon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

object AvalonMMSlaveFactory{
  def getAvalonConfig(addressWidth: Int,
                      dataWidth: Int) = {
    AvalonMMConfig.pipelined(
      addressWidth = addressWidth,
      dataWidth = dataWidth
    ).copy(
      useByteEnable = false,
      useWaitRequestn = false
    )
  }

  def apply(bus: AvalonMM) = new AvalonMMSlaveFactory(bus)
}


class AvalonMMSlaveFactory(bus: AvalonMM, configBus: BusSlaveFactoryConfig = BusSlaveFactoryConfig()) extends BusSlaveFactoryDelayed{

  assert(bus.config == AvalonMMSlaveFactory.getAvalonConfig(bus.config.addressWidth, bus.config.dataWidth))

  val readAtCmd = Flow(Bits(bus.config.dataWidth bits))
  val readAtRsp = readAtCmd.stage()

  bus.readDataValid := readAtRsp.valid
  bus.readData := readAtRsp.payload

  readAtCmd.valid := bus.read
  readAtCmd.payload := 0

  override def build(): Unit = {
    for(element <- elements) element match {
      case element: BusSlaveFactoryNonStopWrite => element.that.assignFromBits(bus.writeData(element.bitOffset, element.that.getBitsWidth bits))
      case _ =>
    }

    for((address, jobs) <- elementsPerAddress){
      when(bus.address === address){

        when(bus.write){
          for(element <- jobs) element match{
            case element: BusSlaveFactoryWrite   => element.that.assignFromBits(bus.writeData(element.bitOffset, element.that.getBitsWidth bits))
            case element: BusSlaveFactoryOnWrite => element.doThat()
            case _ =>
          }
        }

        when(bus.read){
          for(element <- jobs) element match{
            case element: BusSlaveFactoryRead   => readAtCmd.payload(element.bitOffset, element.that.getBitsWidth bits) := element.that.asBits
            case element: BusSlaveFactoryOnRead => element.doThat()
            case _ =>
          }
        }
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth

  override def configFactory: BusSlaveFactoryConfig = configBus

  override def multiWordAddressInc: Int = busDataWidth / 8
}
