package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

/**
 * Created by PIC32F_USER on 02/08/2017.
 */

case class SpiMasterCtrlGenerics( ssWidth : Int,
                              timerWidth : Int,
                              dataWidth : Int = 8){
  def ssGen = ssWidth != 0
}

case class SpiMasterCtrlConfig(generics : SpiMasterCtrlGenerics) extends Bundle{
  val kind = SpiKind()
  val sclkToogle = UInt(generics.timerWidth bits)
  val ss = if(generics.ssGen) new Bundle {
    val setup   = UInt(generics.timerWidth bits)
    val hold    = UInt(generics.timerWidth bits)
    val disable = UInt(generics.timerWidth bits)
  } else null
}

object SpiMasterCtrlCmdMode extends SpinalEnum(binarySequential){
  val DATA, SS = newElement()
}

case class SpiMasterCtrlCmdData(generics : SpiMasterCtrlGenerics) extends Bundle{
  val data = Bits(generics.dataWidth bits)
  val read = Bool
}

case class SpiMasterCtrlCmdSs(generics : SpiMasterCtrlGenerics) extends Bundle{
  val enable = Bool
  val index = UInt(log2Up(generics.ssWidth) bits)
}

case class SpiMasterCmd(generics : SpiMasterCtrlGenerics) extends Bundle{
  val mode = if(generics.ssGen) SpiMasterCtrlCmdMode() else null
  val args = Bits(Math.max(widthOf(SpiMasterCtrlCmdData(generics)), log2Up(generics.ssWidth) + 1 ) bits)

  def isData = if(generics.ssGen) mode === SpiMasterCtrlCmdMode.DATA else True

  def argsData = {
    val ret = SpiMasterCtrlCmdData(generics)
    ret.assignFromBits(args)
    ret
  }
  def argsSs = {
    val ret = SpiMasterCtrlCmdSs(generics)
    ret.assignFromBits(args)
    ret
  }
}

case class SpiMasterCtrl(generics : SpiMasterCtrlGenerics) extends Component{
  import generics._

  val io = new Bundle {
    val config = in(SpiMasterCtrlConfig(generics))
    val cmd = slave Stream(SpiMasterCmd(generics))
    val rsp = master Flow(Bits(dataWidth bits))
    val spi = master(SpiMaster(ssWidth))

    def driveFrom(bus : BusSlaveFactory)(cmdFifoSize : Int, rspFifoSize : Int) = new Area {
      //CMD
      val cmdLogic = new Area {
        val streamUnbuffered = bus.createAndDriveFlow(SpiMasterCmd(generics),address =  0).toStream
        val (stream, fifoAvailability) = streamUnbuffered.queueWithAvailability(cmdFifoSize)
        cmd << stream
        bus.read(fifoAvailability, address = 4, 16)
      }

      //RSP
      val rspLogic = new Area {
        val (stream, fifoOccupancy) = rsp.queueWithOccupancy(rspFifoSize)
        bus.readStreamNonBlocking(stream, address = 0, validBitOffset = 31, payloadBitOffset = 0)
        bus.read(fifoOccupancy, address = 0, 16)
      }

      //Status
      val interruptCtrl = new Area {
        val cmdIntEnable = bus.createReadAndWrite(Bool, address = 4, 0) init(False)
        val rspIntEnable  = bus.createReadAndWrite(Bool, address = 4, 1) init(False)
        val cmdInt  = cmdIntEnable & !cmdLogic.stream.valid
        val rspInt   = rspIntEnable  &  rspLogic.stream.valid
        val interrupt = rspInt || cmdInt
        bus.read(cmdInt, address = 4, 8)
        bus.read(rspInt , address = 4, 9)
      }

      //Configs
      bus.drive(config.kind, 8)
      bus.drive(config.sclkToogle, 12)
      if(ssGen) new Bundle {
        bus.drive(config.ss.setup,   address = 16)
        bus.drive(config.ss.hold,    address = 20)
        bus.drive(config.ss.disable, address = 24)
      } else null
    }
  }

  val timer = new Area{
    val counter = Reg(UInt(timerWidth bits))
    val reset = False
    val ss = if(ssGen) new Area{
      val setupHit    = counter === io.config.ss.setup
      val holdHit     = counter === io.config.ss.hold
      val disableHit  = counter === io.config.ss.disable
    } else null
    val sclkToogleHit = counter === io.config.sclkToogle

    counter := counter + 1
    when(reset){
      counter := 0
    }
  }

  val fsm = new Area{
    val counter = Counter(dataWidth*2)
    val buffer = Reg(Bits(dataWidth bits))
    val ss = RegInit(B((1 << ssWidth)-1, ssWidth bits))

    io.cmd.ready := False
    when(io.cmd.valid){
      when(io.cmd.isData) {
        when(timer.sclkToogleHit) {
          counter.increment()
          timer.reset := True
          io.cmd.ready := counter.willOverflowIfInc
          when(counter.lsb) {
            buffer := (buffer ## io.spi.miso).resized
          }
        }
      } otherwise{
        if(ssGen){
          when(io.cmd.argsSs.enable){
            ss(io.cmd.argsSs.index) := False
            when(timer.ss.setupHit){
              io.cmd.ready := True
            }
          } otherwise{
            when(!counter.lsb){
              when(timer.ss.holdHit){
                counter.increment()
                timer.reset := True
              }
            } otherwise{
              ss(io.cmd.argsSs.index) := True
              when(timer.ss.disableHit){
                io.cmd.ready := True
              }
            }
          }
        }
      }
    }

    //CMD responses
    io.rsp.valid   := RegNext(io.cmd.fire && io.cmd.isData && io.cmd.argsData.read) init(False)
    io.rsp.payload := buffer

    //Idle states
    when(!io.cmd.valid || io.cmd.ready){
      counter := 0
      timer.reset := True
    }

    //SPI connections
    if(ssGen) io.spi.ss := ss
    io.spi.sclk := RegNext(((io.cmd.valid && io.cmd.isData) && (counter.lsb ^ io.config.kind.cpha)) ^ io.config.kind.cpol)
    io.spi.mosi := RegNext(io.cmd.argsData.data(dataWidth-1 - (counter >> 1)))
  }
}


object SpiMasterCtrl{
  def main(args: Array[String]) {
    SpinalVerilog({
      new Component{
        val ctrl = new SpiMasterCtrl(SpiMasterCtrlGenerics(8,16))
        val factory = Apb3SlaveFactory(slave(Apb3(8,32)))
        ctrl.io.driveFrom(factory)(cmdFifoSize = 32, rspFifoSize = 32)
        master(cloneOf(ctrl.io.spi)) <> ctrl.io.spi
      }.setDefinitionName("TopLevel")
    })
   // SpinalVerilog(new SpiMaster(SpiMasterGenerics(2,0,16)).setDefinitionName("TopLevelV"))
  }
}