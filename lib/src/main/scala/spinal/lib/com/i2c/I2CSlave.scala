/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory



/**
  * Generics for the I2C Slave
  *
  * @param samplingWindowSize        : depth sampling
  * @param samplingClockDividerWidth : Width of the clock divider
  * @param tsuDataWidth              : Data set-up time width
  * @param timeoutWidth              : Timeout width
  */
case class I2cSlaveGenerics(
  samplingWindowSize        : Int = 3,
  samplingClockDividerWidth : BitCount = 10 bits,
  tsuDataWidth              : BitCount =  6 bits,
  timeoutWidth              : BitCount = 20 bits
){}


/**
  * Run-time configuration for the I2CSlave
  */
case class I2cSlaveConfig(g: I2cSlaveGenerics) extends Bundle {

  val samplingClockDivider = UInt(g.samplingClockDividerWidth)
  val timeout              = UInt(g.timeoutWidth)
  val tsuData              = UInt(g.tsuDataWidth)


  def setFrequencySampling(frequencySampling: HertzNumber, clkFrequency: HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    samplingClockDivider := (clkFrequency / frequencySampling).toInt
  }

  def setTimeoutPeriod(period: TimeNumber, clkFrequency: HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    timeout := (period*clkFrequency).toInt
  }
}


/**
 * Mode used to manage the slave
 */
object I2cSlaveCmdMode extends SpinalEnum {
  val NONE, START, RESTART, STOP, DROP, DRIVE, READ = newElement()
}


case class I2cSlaveCmd() extends Bundle {
  val kind = I2cSlaveCmdMode()
  val data = Bool
}


case class I2cSlaveRsp() extends Bundle {
  val valid  = Bool
  val enable = Bool
  val data   = Bool
}


case class I2cSlaveBus() extends Bundle with IMasterSlave {
  val cmd = I2cSlaveCmd()
  val rsp = I2cSlaveRsp()

  override def asMaster(): Unit = {
    out(cmd)
    in(rsp)
  }
}


case class I2cSlaveMemoryMappedGenerics(
  ctrlGenerics       : I2cSlaveGenerics,
  addressFilterCount : Int = 0,
  masterGenerics     : I2cMasterMemoryMappedGenerics = null
){
  def genMaster        = masterGenerics != null
  def genAddressFilter = addressFilterCount > 0
}


case class I2cMasterMemoryMappedGenerics(timerWidth: Int)


case class I2cSlaveIo(g: I2cSlaveGenerics) extends Bundle {

  val i2c    = master(I2c())
  val config = in(I2cSlaveConfig(g))
  val bus    = master(I2cSlaveBus())

  val internals = out(new Bundle {
    val inFrame = Bool
    val sdaRead, sclRead = Bool
  })

  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt)(generics: I2cSlaveMemoryMappedGenerics) = {
    I2cCtrl.driveI2cSlaveIo(this, busCtrl, baseAddress)(generics)
  }
}


/**
  * I2C Slave IO Layer :
  *
  *  This component manages the low level of the I2C protocol. (START, STOP, Send & Receive bit data)
  *
  *          ________                       ________
  *         |        |<------- I2C ------->|        |---> CMD
  *         | Master |                     |  Slave |
  *         |________|                     |________|<--- RSP
  *
  *
  * 3 bit frame =>  :
  *              |       |       |       |       |       |       |       |       |
  *   CMD    :   START   DRIVE   READ    DRIVE   READ    DRIVE   READ    DRIVE   STOP
  *   RSP    :   |       |RSP    |       |RSP    |       |RSP   |
  */
class I2cSlave(g : I2cSlaveGenerics) extends Component{

  import spinal.lib.com.i2c.{I2cSlaveCmdMode => CmdMode}

  /**
    * Interface of the I2C Hal slave
    */
  val io = I2cSlaveIo(g)


  /**
    * Filter SDA and SCL input
    */
  val filter = new I2cIoFilter(
    i2c               = io.i2c,
    clockDivider      = io.config.samplingClockDivider,
    samplingSize      = g.samplingWindowSize,
    clockDividerWidth = g.samplingClockDividerWidth
  )

  /**
    * Detect the rising and falling edge of the scl signal
    */
  val sclEdge = filter.scl.edges(True)
  val sdaEdge = filter.sda.edges(True)


  /**
    * Detect the start/restart and the stop sequences
    */
  val detector = new Area{
    val start = filter.scl && sdaEdge.fall
    val stop  = filter.scl && sdaEdge.rise
  }

  val tsuData = new Area{
    val counter = Reg(UInt(g.tsuDataWidth)) init(0)

    val done    = counter === 0
    val reset   = False

    when(!done) {
      counter := counter - 1
    }

    when(reset){
      counter := io.config.tsuData
    }
  }


  /**
    * Slave controller
    */
  val ctrl = new Area{

    val inFrame, inFrameData = RegInit(False)
    val sdaWrite, sclWrite   = True

    // Create a bus RSP buffer
    case class Rsp() extends Bundle{
      val enable = Bool
      val data   = Bool
    }

    val rspBufferIn = Stream(Rsp())
    val rspBuffer   = rspBufferIn.stage() //Store rsp transaction
    val rspAhead    = rspBuffer.valid ? rspBuffer.asFlow | rspBufferIn.asFlow

    rspBufferIn.valid  := io.bus.rsp.valid
    rspBufferIn.enable := io.bus.rsp.enable
    rspBufferIn.data   := io.bus.rsp.data
    rspBuffer.ready    := False

    // default value
    io.bus.cmd.kind := CmdMode.NONE
    io.bus.cmd.data := filter.sda

    // Send & Receive bit data
    when(inFrame) {
      when(sclEdge.rise) {
        io.bus.cmd.kind := CmdMode.READ
      }

      when(sclEdge.fall) {
        inFrameData     := True
        rspBuffer.ready := True //Flush
      }
    }

    when(inFrameData) {
      when(!rspBuffer.valid || rspBuffer.ready){
        io.bus.cmd.kind := CmdMode.DRIVE
      }

      when(!rspAhead.valid  || (rspAhead.enable && !tsuData.done)) {
        sclWrite := False
      }

      tsuData.reset := !rspAhead.valid

      when(rspAhead.valid && rspAhead.enable){
        sdaWrite := rspAhead.data
      }
    }

    when(detector.start){
      io.bus.cmd.kind := inFrame ? CmdMode.RESTART | CmdMode.START
      inFrame         := True
      inFrameData     := False
    }
  }

  val timeout = new Area{

    val counter = Reg(UInt(g.timeoutWidth)) init(0)
    val tick    = counter === 0

    counter := counter - 1

    when(sclEdge.toggle || !ctrl.inFrame){
      counter := io.config.timeout
      tick    := False
    }
  }

  when(detector.stop || timeout.tick){
    when(ctrl.inFrame) {
      io.bus.cmd.kind := timeout.tick ? CmdMode.DROP | CmdMode.STOP
    }
    ctrl.inFrame     := False
    ctrl.inFrameData := False
  }

  io.internals.inFrame := ctrl.inFrame
  io.internals.sdaRead := filter.sda
  io.internals.sclRead := filter.scl


  /*
   * Drive SCL & SDA signals
   */
  io.i2c.scl.write := ctrl.sclWrite
  io.i2c.sda.write := ctrl.sdaWrite
}