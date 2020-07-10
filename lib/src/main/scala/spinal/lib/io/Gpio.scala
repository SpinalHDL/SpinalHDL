package spinal.lib.io

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

object Gpio {
  case class Parameter(width : Int,                   //Number of pin
                       var input : Seq[Int] = null,   //List of pin id which can be inputs (null mean all)
                       var output : Seq[Int]  = null, //List of pin id which can be outputs (null mean all)
                       interrupt : Seq[Int]  = Nil,   //List of pin id which can be used as interrupt source
                       readBufferLength : Int = 2)    //Number of syncronisation stages

  abstract class Ctrl[T <: spinal.core.Data with IMasterSlave](p: Gpio.Parameter,
                                                                busType: HardType[T],
                                                                factory: T => BusSlaveFactory
                                                               ) extends Component {

    if(p.input == null) p.input = (0 until p.width)
    if(p.output == null) p.output = (0 until p.width)

    val io = new Bundle {
      val gpio = master(TriStateArray(p.width bits))
      val bus = slave(busType())
      val interrupt = out(Bits(p.width bits))
    }

    val mapper = factory(io.bus)
    val syncronized = BufferCC(io.gpio.read)
    val last = RegNext(syncronized)


    for(i <- 0 until p.width){
      if(p.input.contains(i)) mapper.read(syncronized(i), 0x00, i)
      if(p.output.contains(i)) mapper.driveAndRead(io.gpio.write(i), 0x04, i) else io.gpio.write(i) := False
      if(p.output.contains(i) && p.input.contains(i)) mapper.driveAndRead(io.gpio.writeEnable(i), 0x08, i) init(False) else io.gpio.writeEnable(i) := Bool(p.output.contains(i))
    }

    val interrupt = new Area {
      val enable = new Area{
        val high, low, rise, fall = Bits(p.width bits)
      }

      val valid = ((enable.high & syncronized)
                | (enable.low & ~syncronized)
                | (enable.rise & (syncronized & ~last))
                | (enable.fall & (~syncronized & last)))

      for(i <- 0 until p.width){
        if(p.interrupt.contains(i)){
          io.interrupt(i) := valid(i)
          mapper.driveAndRead(enable.rise(i), 0x20,i) init(False)
          mapper.driveAndRead(enable.fall(i), 0x24,i) init(False)
          mapper.driveAndRead(enable.high(i), 0x28,i) init(False)
          mapper.driveAndRead(enable.low(i),  0x2C,i) init(False)
        } else {
          io.interrupt(i) := False
          enable.rise(i) := False
          enable.fall(i) := False
          enable.high(i) := False
          enable.low(i) := False
        }
      }
    }
  }

  def addressWidth = 8
}


case class Apb3Gpio2(  parameter: Gpio.Parameter,
                       busConfig: Apb3Config = Apb3Config(12, 32)
                     ) extends Gpio.Ctrl[Apb3] (
  parameter,
  Apb3(busConfig),
  Apb3SlaveFactory(_)
)
object BmbGpio2{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = Gpio.addressWidth,
    dataWidth = 32
  )
}
case class BmbGpio2(   parameter: Gpio.Parameter,
                       busConfig: BmbParameter
                    ) extends Gpio.Ctrl[Bmb] (
  parameter,
  Bmb(busConfig),
  BmbSlaveFactory(_)
)