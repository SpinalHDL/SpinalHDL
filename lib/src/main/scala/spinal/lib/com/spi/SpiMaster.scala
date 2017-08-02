package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

/**
 * Created by PIC32F_USER on 02/08/2017.
 */

case class SpiMasterGenerics( sioCount : Int,
                              ssWidth : Int,
                              timerWidth : Int,
                              dataWidth : Int = 8){
  val ssGen = ssWidth != 0
  require(isPow2(sioCount))
}

case class SpiMasterConfig(generics : SpiMasterGenerics) extends Bundle{
  val kind = SpiKind()
  val sclkToogle = UInt(generics.timerWidth bits)
  val ssSetup    = if(generics.ssGen) UInt(generics.timerWidth bits) else null
  val ssHold     = if(generics.ssGen) UInt(generics.timerWidth bits) else null
  val ssDisable  = if(generics.ssGen) UInt(generics.timerWidth bits) else null
}

object SpiMasterCmdMode extends SpinalEnum(binarySequential){
  val DATA, SS = newElement()
}

case class SpiMasterCmdData(generics : SpiMasterGenerics) extends Bundle{
  val data = Bits(generics.dataWidth bits)
  val read = Bool
  val halfDuplex = Bool
  val sioUsage = UInt(log2Up(log2Up(generics.sioCount) + 1) bits)
}

case class SpiMasterCmdSs(generics : SpiMasterGenerics) extends Bundle{
  val enable = Bool
  val index = UInt(log2Up(generics.ssWidth) bits)
}

case class SpiMasterCmd(generics : SpiMasterGenerics) extends Bundle{
  val mode = SpiMasterCmdMode()
  val args = Bits(Math.max(widthOf(SpiMasterCmdData(generics)), log2Up(generics.ssWidth) + 1 ) bits)
  def argsData = {
    val ret = SpiMasterCmdData(generics)
    ret.assignFromBits(args)
    ret
  }
  def argsSs = {
    val ret = SpiMasterCmdSs(generics)
    ret.assignFromBits(args)
    ret
  }
}

case class SpiMaster(generics : SpiMasterGenerics) extends Component{
  import generics._

  val io = new Bundle {
    val config = in(SpiMasterConfig(generics))
    val cmd = slave Stream(SpiMasterCmd(generics))
    val rsp = master Flow(Bits(dataWidth bits))
    val spi = master(SpiSio(sioCount, ssWidth))

    def driveFrom(bus : BusSlaveFactory)(cmdFifoSize : Int, rspFifoSize : Int) = new Area {
      //CMD
      val cmdLogic = new Area {
        val streamUnbuffered = bus.createAndDriveFlow(SpiMasterCmd(generics), 0).toStream
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
    }
  }

  def cmdSioUsageMax = log2Up(generics.sioCount)

  val timer = new Area{
    val counter = Reg(UInt(timerWidth bits))
    val reset = False
    val ss = if(ssGen) new Area{
      val setupHit    = counter === io.config.ssSetup
      val holdHit     = counter === io.config.ssHold
      val disableHit  = counter === io.config.ssDisable
    } else null
    val sclkToogleHit = counter === io.config.sclkToogle

    counter := counter + 1
    when(reset){
      counter := 0
    }
  }

  val fsm = new Area{
    val counter = Reg(UInt(log2Up(dataWidth << 1) bits)) init(0)
    val counterInc = False
    val counterIncValue = U(0, log2Up(dataWidth) bits)
    val (counterIncResult, counterWillOverflow) = AddWithCarry(counter, (counterIncValue @@ True))
    val buffer = Reg(Bits(dataWidth bits))
    val ss = RegInit(B((1 << ssWidth)-1, ssWidth bits))
    val misoSample = RegNext(io.spi.sio.read)

    when(counterInc){
      counter := counterIncResult
    }

    io.spi.sio.writeEnable := 0
    io.cmd.ready := False
    when(io.cmd.valid){
      switch(io.cmd.mode){
        is(SpiMasterCmdMode.DATA){
          when(!io.cmd.argsData.halfDuplex) {
            io.spi.sio.writeEnable := 1
          } otherwise {
            for (i <- 0 to cmdSioUsageMax if i != 0) {
              io.spi.sio.writeEnable(1 << i - 1 downto 0) :=  B((1 << i - 1 downto 0) -> true)
            }
          }
          when(timer.sclkToogleHit) {
            timer.reset := True
            counterInc := True
            io.cmd.ready := counterWillOverflow
            when(counter.lsb) { //Sample data
              counterIncValue := io.cmd.argsData.sioUsage.muxList((0 to io.cmd.argsData.sioUsage.maxValue.toInt).map(v => v -> U((1 << v) - 1))).resized
              when(!io.cmd.argsData.halfDuplex) {
                buffer := (buffer ## misoSample(0)).resized
              } otherwise {
                switch(io.cmd.argsData.sioUsage) {
                 /* default{
                    buffer := (buffer ## misoSample(0)).resized
                  }*/
                  for (i <- 0 to cmdSioUsageMax) {
                    is(i) {
                      buffer := (buffer ## misoSample((1 << i) - 1 downto 0)).resized
                    }
                  }
                }
              }
            }
          }
        }
        if(ssGen)
        is(SpiMasterCmdMode.SS){
          when(io.cmd.argsSs.enable){
            ss(io.cmd.argsSs.index) := False
            when(timer.ss.setupHit){
              io.cmd.ready := True
            }
          } otherwise{
            when(!counter.lsb){
              when(timer.ss.holdHit){
                counterInc := True
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
    io.rsp.valid   := io.cmd.fire && io.cmd.mode === SpiMasterCmdMode.DATA && io.cmd.argsData.read
    io.rsp.payload := (buffer ## misoSample).resized

    //Idle states
    when(!io.cmd.valid || io.cmd.ready){
      counter := 0
      timer.reset := True
    }

    //SPI connections
    if(ssGen) io.spi.ss   := ss
    io.spi.sclk := RegNext(((io.cmd.valid && io.cmd.mode === SpiMasterCmdMode.DATA) && (counter.lsb ^ io.config.kind.cpha)) ^ io.config.kind.cpol)
    for(i <- 0 until sioCount){
      val minimalSioUsage = 1 << log2Up(i+1)    //1 2 4 4 8 8 8 8
      val bitsIndexes = (0 until dataWidth).filter(_ % minimalSioUsage == 0)
      io.spi.sio.write(i) := RegNext(bitsIndexes.map(io.cmd.argsData.data(_)).read(counter >> (1 + log2Up(minimalSioUsage))))
    }
  }
}


object SpiMaster{
  def main(args: Array[String]) {
    SpinalVerilog(new SpiMaster(SpiMasterGenerics(4,8,16)).setDefinitionName("TopLevelV"))
  }
}