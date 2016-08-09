package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

object Apb3SlaveFactory {
  def apply(bus: Apb3,selId : Int = 0) = new Apb3SlaveFactory(bus,selId)
}

class Apb3SlaveFactory(bus : Apb3,selId : Int) extends BusSlaveFactoryDelayed{
  bus.PREADY := True
  bus.PRDATA := 0
  if(bus.config.useSlaveError) bus.PSLVERROR := False

  val doWrite = bus.PSEL(selId) && bus.PENABLE &&  bus.PWRITE
  val doRead  = bus.PSEL(selId) && bus.PENABLE && !bus.PWRITE

  override def build(): Unit = {
    for(element <- elements) element match {
      case element : BusSlaveFactoryNonStopWrite =>
        element.that.assignFromBits(bus.PWDATA(element.bitOffset, element.that.getBitsWidth bits))
      case _ =>
    }

    for((address,jobs) <- elementsPerAddress){
      when(bus.PADDR === address){
        when(doWrite){
          for(element <- jobs) element match{
            case element : BusSlaveFactoryWrite => {
              element.that.assignFromBits(bus.PWDATA(element.bitOffset, element.that.getBitsWidth bits))
            }
            case element : BusSlaveFactoryOnWrite => element.doThat()
            case _ =>
          }
        }
        when(doRead){
          for(element <- jobs) element match{
            case element : BusSlaveFactoryRead => {
              bus.PRDATA(element.bitOffset, element.that.getBitsWidth bits) := element.that.asBits
            }
            case element : BusSlaveFactoryOnRead => element.doThat()
            case _ =>
          }
        }
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth
}
